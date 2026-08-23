(in-package #:autolith)

;;;; -- Terminal Execution Backends --

;;; The managed Modal contract follows the read-only
;;; /Users/ericfode/src/hermes-agent-reference checkout at commit
;;; 981101239a064c020a9d18fc3b1060ae306934ed, specifically
;;; tools/environments/managed_modal.py and modal_utils.py.

(defparameter *managed-modal-gateway-url*
  "https://modal-gateway.nousresearch.com"
  "The Nous-managed Modal gateway origin.")

(defparameter *managed-modal-image*
  "nikolaik/python-nodejs:python3.11-nodejs20"
  "The container image requested for managed Modal sandboxes.")

(defparameter *managed-modal-poll-interval-seconds* 0.25
  "The delay between managed Modal exec status polls.")

(defparameter *managed-modal-client-timeout-grace-seconds* 10
  "The client-side grace after the server exec timeout.")

(defparameter *managed-modal-maximum-timeout-seconds* 3600
  "The largest shell.run timeout accepted by the managed Modal backend.")

(defparameter *managed-modal-maximum-response-bytes* (* 2 1024 1024)
  "The largest managed Modal JSON response accepted by Autolith.")

(defparameter *managed-modal-http-request-function* nil
  "Optional test request function replacing managed Modal HTTP transport.")

(defparameter *managed-modal-credential-function* nil
  "Optional test function returning a managed Modal Nous bearer token.")

(defparameter *managed-modal-time-function* nil
  "Optional test function returning monotonic seconds.")

(defclass terminal-execution-backend ()
  ((route
    :initarg :route
    :reader terminal-execution-backend-route
    :type (member :local :modal)
    :documentation "The session route implemented by this backend."))
  (:documentation "An explicit destination for authorized shell.run commands."))

(defclass local-terminal-execution-backend (terminal-execution-backend)
  ((configuration
    :initarg :configuration
    :reader local-terminal-execution-configuration
    :type configuration
    :documentation "The configuration defining the writable local workspace."))
  (:default-initargs :route ':local)
  (:documentation "The existing local sandboxed command backend."))

(defclass managed-modal-terminal-execution-backend (terminal-execution-backend)
  ((configuration
    :initarg :configuration
    :reader managed-modal-terminal-configuration
    :type configuration
    :documentation "The configuration locating independent Nous OAuth credentials.")
   (logical-key
    :initarg :logical-key
    :reader managed-modal-terminal-logical-key
    :type non-empty-string
    :documentation "The launcher- or child-unique persistent filesystem identity.")
   (sandbox-id
    :initform nil
    :accessor managed-modal-terminal-sandbox-id
    :type (option string)
    :documentation "The lazily created gateway sandbox identifier.")
   (state
    :initform ':not-created
    :accessor managed-modal-terminal-state
    :type keyword
    :documentation "The current local lifecycle view of the remote sandbox.")
   (create-idempotency-key
    :initform (make-identifier)
    :reader managed-modal-terminal-create-idempotency-key
    :type non-empty-string
    :documentation "The stable idempotency key for this backend's create attempt.")
   (lock
    :initform (make-lock "Autolith managed Modal terminal")
    :reader managed-modal-terminal-lock
    :documentation "The lock serializing sandbox creation and execs."))
  (:default-initargs :route ':modal)
  (:documentation "One lazy Nous-managed Modal sandbox for one agent session."))

(define-condition managed-modal-error (tool-error)
  ((stage
    :initarg :stage
    :reader managed-modal-error-stage
    :type keyword
    :documentation
    "The authentication, timeout, create, start, poll, cancel, or terminate stage.")
   (status
    :initarg :status
    :initform nil
    :reader managed-modal-error-status
    :type (option integer)
    :documentation "The gateway HTTP status, when available.")
   (sandbox-id
    :initarg :sandbox-id
    :initform nil
    :reader managed-modal-error-sandbox-id
    :type (option string)
    :documentation "The sandbox involved in the failed operation, when known.")
   (exec-id
    :initarg :exec-id
    :initform nil
    :reader managed-modal-error-exec-id
    :type (option string)
    :documentation "The exec involved in the failed operation, when known."))
  (:documentation "A typed managed Modal gateway or protocol failure."))

(-> terminal-execution-backend-run
    (terminal-execution-backend string pathname keyword (integer 1) (integer 0))
    tool-result)
(defgeneric terminal-execution-backend-run
    (backend command directory authorization timeout output-limit)
  (:documentation
   "Execute one already authorized command through BACKEND without fallback."))

(-> terminal-execution-backend-close (terminal-execution-backend) null)
(defgeneric terminal-execution-backend-close (backend)
  (:documentation "Release BACKEND's external runtime idempotently."))

(defmethod terminal-execution-backend-close ((backend terminal-execution-backend))
  "Leave a stateless terminal BACKEND unchanged."
  (declare (ignore backend))
  nil)

(-> terminal-execution-backend-create
    (configuration non-empty-string)
    terminal-execution-backend)
(defun terminal-execution-backend-create (configuration logical-key)
  "Create the backend selected by CONFIGURATION for agent LOGICAL-KEY."
  (ecase (configuration-terminal-route configuration)
    (:local
     (make-instance 'local-terminal-execution-backend
                    :configuration configuration))
    (:modal
     (make-instance 'managed-modal-terminal-execution-backend
                    :configuration configuration
                    :logical-key logical-key))))


;;;; -- Local Backend --

(defmethod terminal-execution-backend-run
    ((backend local-terminal-execution-backend)
     (command string)
     (directory pathname)
     authorization
     (timeout integer)
     (output-limit integer))
  "Run COMMAND through the existing local sandbox policy."
  (let* ((configuration (local-terminal-execution-configuration backend))
         (policy
           (ecase authorization
             (:sandboxed
              (workspace-write-sandbox-policy
               :workspace-roots
               (list (configuration-working-directory configuration))))
             (:full-access
              (external-sandbox-policy))))
         (result
           (handler-bind
               ((sb-int:stream-decoding-error
                  (lambda (condition)
                    (let ((restart (find-restart 'use-value condition)))
                      (when restart
                        (invoke-restart restart (code-char #xFFFD)))))))
             (run-sandboxed
              "/bin/sh" (list "-c" command)
              :policy policy
              :working-directory directory
              :timeout timeout
              :merge-output-p t
              :output-limit output-limit
              :error-output-limit output-limit)))
         (output (sandbox-result-output result))
         (presented-output
           (if (sandbox-result-output-truncated-p result)
               (format nil "~A~%[combined output truncated after ~D characters]"
                       output output-limit)
               output)))
    (if (sandbox-result-timed-out-p result)
        (tool-failure
         (format nil "The command was stopped after ~D seconds.~%~A"
                 timeout presented-output))
        (tool-success
         (format nil "exit ~D~%~A"
                 (sandbox-result-exit-code result)
                 presented-output)))))


;;;; -- Managed Modal Transport --

(-> managed-modal-gateway-url () string)
(defun managed-modal-gateway-url ()
  "Return the validated managed Modal HTTPS origin."
  (let* ((override (uiop:getenv "AUTOLITH_NOUS_MODAL_GATEWAY_URL"))
         (value (string-right-trim
                 '(#\/)
                 (if (non-empty-string-p override)
                     override
                     *managed-modal-gateway-url*)))
         (uri (handler-case (quri:uri value) (error () nil))))
    (unless (and uri
                 (string= (or (quri:uri-scheme uri) "") "https")
                 (non-empty-string-p (quri:uri-host uri))
                 (not (non-empty-string-p (quri:uri-userinfo uri)))
                 (not (non-empty-string-p (quri:uri-query uri)))
                 (not (non-empty-string-p (quri:uri-fragment uri))))
      (error 'configuration-error
             :message
             "AUTOLITH_NOUS_MODAL_GATEWAY_URL must be an HTTPS origin without user information, a query, or a fragment."))
    value))

(-> managed-modal--time () real)
(defun managed-modal--time ()
  "Return monotonic seconds for managed Modal timeout accounting."
  (if *managed-modal-time-function*
      (funcall *managed-modal-time-function*)
      (/ (get-internal-real-time) internal-time-units-per-second)))

(-> managed-modal--access-token
    (managed-modal-terminal-execution-backend)
    non-empty-string)
(defun managed-modal--access-token (backend)
  "Load a fresh independent Nous OAuth bearer for BACKEND."
  (if *managed-modal-credential-function*
      (let ((token (funcall *managed-modal-credential-function* backend)))
        (unless (non-empty-string-p token)
          (error 'managed-modal-error
                 :message "Managed Modal requires Nous OAuth credentials; run autolith auth nous."
                 :tool-name "shell.run"
                 :stage ':authentication))
        token)
      (oauth-credentials-access-token
       (credential-manager-load
        (nous-credential-manager-create
         (managed-modal-terminal-configuration backend))))))

(-> managed-modal--headers
    (managed-modal-terminal-execution-backend &key (:idempotency-key (option string)))
    list)
(defun managed-modal--headers (backend &key idempotency-key)
  "Return bearer and JSON headers for one managed Modal request."
  (append
   (list (cons "Authorization"
               (format nil "Bearer ~A" (managed-modal--access-token backend)))
         (cons "Content-Type" "application/json"))
   (when idempotency-key
     (list (cons "x-idempotency-key" idempotency-key)))))

(-> managed-modal--body-string (t) string)
(defun managed-modal--body-string (body)
  "Return bounded UTF-8 text from BODY and close response streams."
  (labels ((fail ()
             (error 'managed-modal-error
                    :message "Managed Modal response exceeded the accepted size."
                    :tool-name "shell.run"
                    :stage ':response))
           (decode (octets)
             (when (> (length octets) *managed-modal-maximum-response-bytes*)
               (fail))
             (handler-case
                 (sb-ext:octets-to-string octets :external-format ':utf-8)
               (error ()
                 (error 'managed-modal-error
                        :message "Managed Modal returned invalid UTF-8."
                        :tool-name "shell.run"
                        :stage ':response)))))
    (cond
      ((null body) "")
      ((stringp body)
       (when (> (length body) *managed-modal-maximum-response-bytes*)
         (fail))
       body)
      ((typep body '(vector (unsigned-byte 8)))
       (decode body))
      ((streamp body)
       (unwind-protect
            (let* ((limit (1+ *managed-modal-maximum-response-bytes*))
                   (octets (make-array limit :element-type '(unsigned-byte 8)))
                   (count (read-sequence octets body)))
              (when (= count limit) (fail))
              (decode (subseq octets 0 count)))
         (ignore-errors (close body))))
      (t
       (error 'managed-modal-error
              :message "Managed Modal returned an unreadable response body."
              :tool-name "shell.run"
              :stage ':response)))))

(-> managed-modal--dexador-request
    (keyword string list (option string) real real)
    (values string integer t))
(defun managed-modal--dexador-request
    (method url headers content connect-timeout read-timeout)
  "Send one managed Modal HTTP request and return body, status, and headers."
  (multiple-value-bind (body status response-headers)
      (handler-case
          (ecase method
            (:get
             (dexador:get url
                          :headers headers
                          :force-binary t
                          :want-stream t
                          :keep-alive nil
                          :connect-timeout connect-timeout
                          :read-timeout read-timeout))
            (:post
             (dexador:post url
                           :headers headers
                           :content (or content "")
                           :force-binary t
                           :want-stream t
                           :keep-alive nil
                           :connect-timeout connect-timeout
                           :read-timeout read-timeout)))
        (http-request-failed (condition)
          (values (response-body condition)
                  (response-status condition)
                  (response-headers condition))))
    (values (managed-modal--body-string body) status response-headers)))

(-> managed-modal--request
    (managed-modal-terminal-execution-backend keyword string
     &key (:payload (option json-object))
          (:idempotency-key (option string))
          (:connect-timeout real)
          (:read-timeout real))
    (values string integer t))
(defun managed-modal--request
    (backend method path
     &key payload idempotency-key (connect-timeout 1) (read-timeout 30))
  "Send one authenticated JSON gateway request for BACKEND."
  (let ((function (or *managed-modal-http-request-function*
                      #'managed-modal--dexador-request)))
    (funcall function
             method
             (concatenate 'string (managed-modal-gateway-url) path)
             (managed-modal--headers backend :idempotency-key idempotency-key)
             (and payload (json-encode payload))
             connect-timeout
             read-timeout)))

(-> managed-modal--document (string keyword integer (option string) (option string)) json-object)
(defun managed-modal--document (body stage status sandbox-id exec-id)
  "Decode BODY as an object or signal a typed STAGE failure."
  (let ((document
          (handler-case (json-decode body)
            (error () nil))))
    (unless (json-object-p document)
      (error 'managed-modal-error
             :message (format nil "Managed Modal ~(~A~) returned malformed JSON."
                              stage)
             :tool-name "shell.run"
             :stage stage
             :status status
             :sandbox-id sandbox-id
             :exec-id exec-id))
    document))

(-> managed-modal--error-message (string integer) string)
(defun managed-modal--error-message (body status)
  "Return a bounded non-secret gateway refusal explanation."
  (let ((document (handler-case (json-decode body) (error () nil))))
    (or (and (json-object-p document)
             (loop for key in '("error" "message" "code")
                   for value = (json-get document key)
                   when (non-empty-string-p value)
                     return value))
        (case status
          (401 "Nous OAuth credentials were rejected; run autolith auth nous.")
          ((402 403) "The Nous account is not entitled to managed Modal.")
          (429 "The managed Modal gateway rate-limited the request.")
          (t (format nil "HTTP ~D" status))))))

(-> managed-modal--require-success
    (string integer keyword (option string) (option string))
    null)
(defun managed-modal--require-success (body status stage sandbox-id exec-id)
  "Signal when STATUS is not successful for one managed Modal STAGE."
  (unless (<= 200 status 299)
    (error 'managed-modal-error
           :message (format nil "Managed Modal ~(~A~) failed: ~A"
                            stage (managed-modal--error-message body status))
           :tool-name "shell.run"
           :stage stage
           :status status
           :sandbox-id sandbox-id
           :exec-id exec-id))
  nil)

(-> managed-modal--validate-timeout ((integer 1)) (integer 1))
(defun managed-modal--validate-timeout (timeout)
  "Return TIMEOUT when it fits the managed Modal sandbox and exec bound."
  (when (> timeout *managed-modal-maximum-timeout-seconds*)
    (error 'managed-modal-error
           :message
           (format nil "Managed Modal timeout must not exceed ~D seconds."
                   *managed-modal-maximum-timeout-seconds*)
           :tool-name "shell.run"
           :stage ':timeout))
  timeout)

(-> managed-modal--ensure-sandbox
    (managed-modal-terminal-execution-backend (integer 1))
    non-empty-string)
(defun managed-modal--ensure-sandbox (backend timeout)
  "Create BACKEND's sandbox lazily and return its identifier."
  (or (managed-modal-terminal-sandbox-id backend)
      (restart-case
          (progn
            (setf (managed-modal-terminal-state backend) ':creating)
            (multiple-value-bind (body status headers)
                (managed-modal--request
                 backend :post "/v1/sandboxes"
                 :payload
                 (json-object
                  "image" *managed-modal-image*
                  "cwd" "/root"
                  "cpu" 1
                  "memoryMiB" 5120
                  "timeoutMs" 3600000
                  "idleTimeoutMs" (max 300000 (* timeout 1000))
                  "persistentFilesystem" t
                  "logicalKey" (managed-modal-terminal-logical-key backend))
                 :idempotency-key
                 (managed-modal-terminal-create-idempotency-key backend)
                 :connect-timeout 1
                 :read-timeout 60)
              (declare (ignore headers))
              (managed-modal--require-success body status ':create nil nil)
              (let* ((document
                       (managed-modal--document body ':create status nil nil))
                     (sandbox-id (json-get document "id")))
                (unless (non-empty-string-p sandbox-id)
                  (error 'managed-modal-error
                         :message "Managed Modal create did not return a sandbox id."
                         :tool-name "shell.run"
                         :stage ':create
                         :status status))
                (setf (managed-modal-terminal-sandbox-id backend) sandbox-id
                      (managed-modal-terminal-state backend) ':ready)
                sandbox-id)))
        (retry-managed-modal-create ()
          :report "Retry the idempotent managed Modal sandbox creation."
          (managed-modal--ensure-sandbox backend timeout)))))

(-> managed-modal--terminal-status-p (t) boolean)
(defun managed-modal--terminal-status-p (status)
  "Return true when STATUS finishes one remote exec."
  (if (and (stringp status)
           (member status '("completed" "failed" "cancelled" "timeout")
                   :test #'string=))
      t
      nil))

(-> managed-modal--bounded-output (string (integer 0)) (values string boolean))
(defun managed-modal--bounded-output (output limit)
  "Return OUTPUT capped to LIMIT characters and whether truncation occurred."
  (if (> (length output) limit)
      (values (subseq output 0 limit) t)
      (values output nil)))

(-> managed-modal--result (json-object (integer 0)) tool-result)
(defun managed-modal--result (document output-limit)
  "Convert a terminal exec DOCUMENT into one bounded tool result."
  (let ((output (json-get document "output"))
        (return-code (json-get document "returncode"))
        (status (json-get document "status")))
    (unless (stringp output)
      (setf output ""))
    (unless (integerp return-code)
      (setf return-code 1))
    (multiple-value-bind (bounded truncated-p)
        (managed-modal--bounded-output output output-limit)
      (let ((content
              (format nil "exit ~D~%~A~:[~;~%[combined output truncated after ~D characters]~]"
                      return-code bounded truncated-p output-limit)))
        (if (and (stringp status)
                 (member status '("cancelled" "timeout") :test #'string=))
            (tool-failure content)
            (tool-success content))))))

(-> managed-modal--cancel
    (managed-modal-terminal-execution-backend non-empty-string non-empty-string)
    null)
(defun managed-modal--cancel (backend sandbox-id exec-id)
  "Best-effort cancel EXEC-ID in SANDBOX-ID."
  (ignore-errors
    (managed-modal--request
     backend :post
     (format nil "/v1/sandboxes/~A/execs/~A/cancel" sandbox-id exec-id)
     :connect-timeout 1
     :read-timeout 5))
  nil)

(defmethod terminal-execution-backend-run
    ((backend managed-modal-terminal-execution-backend)
     (command string)
     (directory pathname)
     authorization
     (timeout integer)
     (output-limit integer))
  "Execute COMMAND in BACKEND's persistent remote sandbox with bounded polling."
  (declare (ignore authorization))
  (let ((timeout (managed-modal--validate-timeout timeout)))
    (with-lock-held ((managed-modal-terminal-lock backend))
      (let* ((sandbox-id (managed-modal--ensure-sandbox backend timeout))
             (exec-id (make-identifier))
             (cwd (namestring directory))
             (completed-p nil))
        (setf (managed-modal-terminal-state backend) ':running)
        (unwind-protect
             (multiple-value-bind (body status headers)
                 (managed-modal--request
                  backend :post
                  (format nil "/v1/sandboxes/~A/execs" sandbox-id)
                  :payload (json-object "execId" exec-id
                                        "command" command
                                        "cwd" cwd
                                        "timeoutMs" (* timeout 1000))
                  :connect-timeout 1
                  :read-timeout 10)
               (declare (ignore headers))
               (managed-modal--require-success
                body status ':start sandbox-id exec-id)
               (let* ((document
                        (managed-modal--document
                         body ':start status sandbox-id exec-id))
                      (remote-status (json-get document "status")))
                 (when (managed-modal--terminal-status-p remote-status)
                   (setf completed-p t
                         (managed-modal-terminal-state backend) ':ready)
                   (return-from terminal-execution-backend-run
                     (managed-modal--result document output-limit)))
                 (unless (string= (or (json-get document "execId") "") exec-id)
                   (error 'managed-modal-error
                          :message
                          "Managed Modal exec start did not return the expected exec id."
                          :tool-name "shell.run"
                          :stage ':start
                          :status status
                          :sandbox-id sandbox-id
                          :exec-id exec-id))
                 (let ((deadline (+ (managed-modal--time)
                                    timeout
                                    *managed-modal-client-timeout-grace-seconds*)))
                   (loop
                     (when (> (managed-modal--time) deadline)
                       (managed-modal--cancel backend sandbox-id exec-id)
                       (setf completed-p t
                             (managed-modal-terminal-state backend) ':ready)
                       (return
                         (tool-failure
                          (format nil "Managed Modal exec timed out after ~Ds."
                                  timeout))))
                     (sleep *managed-modal-poll-interval-seconds*)
                     (multiple-value-bind (poll-body poll-status poll-headers)
                         (managed-modal--request
                          backend :get
                          (format nil "/v1/sandboxes/~A/execs/~A"
                                  sandbox-id exec-id)
                          :connect-timeout 1
                          :read-timeout 5)
                       (declare (ignore poll-headers))
                       (when (= poll-status 404)
                         (error 'managed-modal-error
                                :message "Managed Modal exec was not found."
                                :tool-name "shell.run"
                                :stage ':poll
                                :status poll-status
                                :sandbox-id sandbox-id
                                :exec-id exec-id))
                       (managed-modal--require-success
                        poll-body poll-status ':poll sandbox-id exec-id)
                       (let* ((poll-document
                                (managed-modal--document
                                 poll-body ':poll poll-status sandbox-id exec-id))
                              (poll-state (json-get poll-document "status")))
                         (when (managed-modal--terminal-status-p poll-state)
                           (setf completed-p t
                                 (managed-modal-terminal-state backend) ':ready)
                           (return
                             (managed-modal--result
                              poll-document output-limit)))))))))
          (unless completed-p
            (managed-modal--cancel backend sandbox-id exec-id)
            (setf (managed-modal-terminal-state backend) ':ready)))))))

(defmethod terminal-execution-backend-close
    ((backend managed-modal-terminal-execution-backend))
  "Terminate BACKEND while snapshotting its persistent filesystem."
  (with-lock-held ((managed-modal-terminal-lock backend))
    (let ((sandbox-id (managed-modal-terminal-sandbox-id backend)))
      (when sandbox-id
        (unwind-protect
             (multiple-value-bind (body status headers)
                 (managed-modal--request
                  backend :post
                  (format nil "/v1/sandboxes/~A/terminate" sandbox-id)
                  :payload (json-object "snapshotBeforeTerminate" t)
                  :connect-timeout 1
                  :read-timeout 60)
               (declare (ignore headers))
               (managed-modal--require-success
                body status ':terminate sandbox-id nil))
          (setf (managed-modal-terminal-sandbox-id backend) nil
                (managed-modal-terminal-state backend) ':closed)))))
  nil)

(-> terminal-execution-backend-status
    ((option terminal-execution-backend))
    list)
(defun terminal-execution-backend-status (backend)
  "Return bounded status fields for BACKEND without creating remote resources."
  (cond
    ((null backend)
     (list :state ':not-created))
    ((typep backend 'managed-modal-terminal-execution-backend)
     (list :state (managed-modal-terminal-state backend)
           :logical-key (managed-modal-terminal-logical-key backend)
           :sandbox-id (managed-modal-terminal-sandbox-id backend)))
    (t
     (list :state ':local))))
