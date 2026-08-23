(in-package #:autolith)

;;;; -- Nous Managed Browser Runtime --

;;; The managed lifecycle contract follows the read-only
;;; /Users/ericfode/src/hermes-agent-reference checkout at commit
;;; 981101239a064c020a9d18fc3b1060ae306934ed, specifically
;;; plugins/browser/browser_use/provider.py. Browser actions use CDP through
;;; the returned cdpUrl or connectUrl and never fall back to another backend.

(defparameter *nous-browser-gateway-origin*
  "https://browser-use-gateway.nousresearch.com"
  "The default Nous-managed Browser Use gateway origin.")

(defparameter *nous-browser-maximum-http-response-bytes* (* 2 1024 1024)
  "The largest managed browser lifecycle response accepted by Autolith.")

(defparameter *nous-browser-maximum-cdp-message-bytes* (* 4 1024 1024)
  "The largest inbound or outbound CDP message accepted by Autolith.")

(defparameter *nous-browser-command-timeout-seconds* 15
  "The maximum wait for one correlated CDP command response.")

(defparameter *nous-browser-action-wait-seconds* 8
  "The maximum bounded readiness wait after navigation and actions.")

(defparameter *nous-browser-maximum-snapshot-nodes* 250
  "The maximum accessible nodes returned by one browser.snapshot call.")

(defparameter *nous-browser-maximum-snapshot-characters* 16000
  "The maximum model-visible browser.snapshot text length.")

(defparameter *nous-browser-http-request-function* nil
  "Optional test transport replacing managed browser lifecycle HTTP.")

(defparameter *nous-browser-credential-function* nil
  "Optional test function returning an independent Nous OAuth token.")

(defparameter *nous-browser-cdp-connect-function* nil
  "Optional test function creating a CDP transport for one connection.")


;;;; -- Conditions --

(define-condition nous-browser-error (tool-error)
  ((stage
    :initarg :stage
    :reader nous-browser-error-stage
    :type keyword
    :documentation "The lifecycle, connection, command, action, or response stage.")
   (status
    :initarg :status
    :initform nil
    :reader nous-browser-error-status
    :type (option integer)
    :documentation "The lifecycle HTTP status, when available.")
   (session-id
    :initarg :session-id
    :initform nil
    :reader nous-browser-error-session-id
    :type (option string)
    :documentation "The managed browser session identifier, when available.")
   (command
    :initarg :command
    :initform nil
    :reader nous-browser-error-command
    :type (option string)
    :documentation "The CDP command that failed, when available."))
  (:documentation "A typed managed browser lifecycle or CDP failure."))

(define-condition nous-browser-stale-reference (nous-browser-error)
  ((reference
    :initarg :reference
    :reader nous-browser-stale-reference-reference
    :type string
    :documentation "The stale or unknown per-snapshot element reference."))
  (:documentation "A browser action named an element outside the current snapshot."))

(-> nous-browser--fail (keyword string &rest t) null)
(defun nous-browser--fail (stage control &rest arguments)
  "Signal a typed managed browser failure at STAGE."
  (error 'nous-browser-error
         :message (apply #'format nil control arguments)
         :tool-name "browser"
         :stage stage))


;;;; -- Lifecycle Transport --

(-> nous-browser-gateway-origin () string)
(defun nous-browser-gateway-origin ()
  "Return the validated managed Browser Use HTTPS origin."
  (let* ((override (uiop:getenv "AUTOLITH_NOUS_BROWSER_GATEWAY_URL"))
         (value (string-right-trim
                 '(#\/)
                 (if (non-empty-string-p override)
                     override
                     *nous-browser-gateway-origin*)))
         (uri (handler-case (quri:uri value) (error () nil))))
    (unless (and uri
                 (string= (or (quri:uri-scheme uri) "") "https")
                 (non-empty-string-p (quri:uri-host uri))
                 (not (non-empty-string-p (quri:uri-userinfo uri)))
                 (not (non-empty-string-p (quri:uri-query uri)))
                 (not (non-empty-string-p (quri:uri-fragment uri))))
      (error 'configuration-error
             :message
             "AUTOLITH_NOUS_BROWSER_GATEWAY_URL must be an HTTPS origin without user information, a query, or a fragment."))
    value))

(-> nous-browser--access-token (configuration) non-empty-string)
(defun nous-browser--access-token (configuration)
  "Load a fresh independent Nous OAuth token for CONFIGURATION."
  (if *nous-browser-credential-function*
      (let ((token (funcall *nous-browser-credential-function* configuration)))
        (unless (non-empty-string-p token)
          (nous-browser--fail
           ':authentication
           "Nous browser automation requires Nous OAuth credentials; run autolith auth nous."))
        token)
      (oauth-credentials-access-token
       (credential-manager-load
        (nous-credential-manager-create configuration)))))

(-> nous-browser--body-string (t) string)
(defun nous-browser--body-string (body)
  "Return bounded UTF-8 lifecycle BODY and close response streams."
  (labels ((fail ()
             (nous-browser--fail ':response
                                 "Managed browser response exceeded the accepted size."))
           (decode (octets)
             (when (> (length octets) *nous-browser-maximum-http-response-bytes*)
               (fail))
             (handler-case
                 (sb-ext:octets-to-string octets :external-format ':utf-8)
               (error ()
                 (nous-browser--fail ':response
                                     "Managed browser returned invalid UTF-8.")))))
    (cond
      ((null body) "")
      ((stringp body)
       (when (> (length body) *nous-browser-maximum-http-response-bytes*) (fail))
       body)
      ((typep body '(vector (unsigned-byte 8)))
       (decode body))
      ((streamp body)
       (unwind-protect
            (let* ((limit (1+ *nous-browser-maximum-http-response-bytes*))
                   (octets (make-array limit :element-type '(unsigned-byte 8)))
                   (count (read-sequence octets body)))
              (when (= count limit) (fail))
              (decode (subseq octets 0 count)))
         (ignore-errors (close body))))
      (t
       (nous-browser--fail ':response
                           "Managed browser returned an unreadable response body.")))))

(-> nous-browser--dexador-request
    (keyword string list (option string) real real)
    (values string integer t))
(defun nous-browser--dexador-request
    (method url headers content connect-timeout read-timeout)
  "Send one browser lifecycle request and return body, status, and headers."
  (multiple-value-bind (body status response-headers)
      (handler-case
          (ecase method
            (:post
             (dexador:post url :headers headers :content (or content "")
                               :force-binary t :want-stream t :keep-alive nil
                               :connect-timeout connect-timeout
                               :read-timeout read-timeout))
            (:patch
             (dexador:patch url :headers headers :content (or content "")
                                :force-binary t :want-stream t :keep-alive nil
                                :connect-timeout connect-timeout
                                :read-timeout read-timeout)))
        (http-request-failed (condition)
          (values (response-body condition)
                  (response-status condition)
                  (response-headers condition))))
    (values (nous-browser--body-string body) status response-headers)))

(-> nous-browser--request
    (configuration keyword string json-object &key (:idempotency-key (option string))
                   (:connect-timeout real) (:read-timeout real))
    (values string integer t))
(defun nous-browser--request
    (configuration method path payload
     &key idempotency-key (connect-timeout 1) (read-timeout 30))
  "Send one authenticated managed browser lifecycle request."
  (let ((headers
          (append
           (list (cons "Content-Type" "application/json")
                 (cons "X-Browser-Use-API-Key"
                       (nous-browser--access-token configuration)))
           (when idempotency-key
             (list (cons "X-Idempotency-Key" idempotency-key)))))
        (function (or *nous-browser-http-request-function*
                      #'nous-browser--dexador-request)))
    (funcall function
             method
             (concatenate 'string (nous-browser-gateway-origin) path)
             headers
             (json-encode payload)
             connect-timeout
             read-timeout)))

(-> nous-browser--document (string keyword integer) json-object)
(defun nous-browser--document (body stage status)
  "Decode lifecycle BODY as a JSON object or signal STAGE failure."
  (let ((document (handler-case (json-decode body) (error () nil))))
    (unless (json-object-p document)
      (error 'nous-browser-error
             :message (format nil "Managed browser ~(~A~) returned malformed JSON."
                              stage)
             :tool-name "browser"
             :stage stage
             :status status))
    document))

(-> nous-browser--preserve-create-key-p (integer string) boolean)
(defun nous-browser--preserve-create-key-p (status body)
  "Return true when retrying create must retain its idempotency key."
  (or (>= status 500)
      (and (= status 409)
           (let* ((document (handler-case (json-decode body) (error () nil)))
                  (error-object (and (json-object-p document)
                                     (json-get document "error")))
                  (message (and (json-object-p error-object)
                                (json-get error-object "message"))))
             (and (stringp message)
                  (search "already in progress" message :test #'char-equal)
                  t)))))


;;;; -- CDP Correlation --

(defclass browser-cdp-pending ()
  ((done-p
    :initform nil
    :accessor browser-cdp-pending-done-p
    :type boolean
    :documentation "Whether the correlated response arrived.")
   (result
    :initform nil
    :accessor browser-cdp-pending-result
    :type t
    :documentation "The response result object.")
   (error
    :initform nil
    :accessor browser-cdp-pending-error
    :type t
    :documentation "The response error object."))
  (:documentation "One synchronous command awaiting its CDP response."))

(defclass browser-cdp-transport ()
  ()
  (:documentation "A replaceable text transport carrying CDP JSON messages."))

(-> browser-cdp-transport-send (browser-cdp-transport string) null)
(defgeneric browser-cdp-transport-send (transport message)
  (:documentation "Send one bounded CDP JSON MESSAGE through TRANSPORT."))

(-> browser-cdp-transport-close (browser-cdp-transport) null)
(defgeneric browser-cdp-transport-close (transport)
  (:documentation "Close TRANSPORT idempotently."))

(defclass websocket-browser-cdp-transport (browser-cdp-transport)
  ((socket
    :initarg :socket
    :reader websocket-browser-cdp-transport-socket
    :documentation "The websocket-driver-client socket."))
  (:documentation "A CDP transport implemented by websocket-driver-client."))

(defmethod browser-cdp-transport-send
    ((transport websocket-browser-cdp-transport) (message string))
  "Send MESSAGE through the live WebSocket."
  (send (websocket-browser-cdp-transport-socket transport) message)
  nil)

(defmethod browser-cdp-transport-close
    ((transport websocket-browser-cdp-transport))
  "Close the underlying WebSocket idempotently."
  (let ((socket (websocket-browser-cdp-transport-socket transport)))
    (unless (eq (ready-state socket) ':closed)
      (ignore-errors (close-connection socket))))
  nil)

(defclass browser-cdp-connection ()
  ((transport
    :initform nil
    :accessor browser-cdp-connection-transport
    :type (option browser-cdp-transport)
    :documentation "The active CDP text transport.")
   (next-id
    :initform 0
    :accessor browser-cdp-connection-next-id
    :type integer
    :documentation "The next monotonically increasing request identifier.")
   (pending
    :initform (make-hash-table)
    :reader browser-cdp-connection-pending
    :type hash-table
    :documentation "Request identifiers mapped to pending responses.")
   (events
    :initform nil
    :accessor browser-cdp-connection-events
    :type list
    :documentation "A bounded newest-first list of received CDP events.")
   (event-sequence
    :initform 0
    :accessor browser-cdp-connection-event-sequence
    :type integer
    :documentation "The sequence of the newest received event.")
   (closed-p
    :initform nil
    :accessor browser-cdp-connection-closed-p
    :type boolean
    :documentation "Whether the transport closed or failed.")
   (close-reason
    :initform nil
    :accessor browser-cdp-connection-close-reason
    :type (option string)
    :documentation "The bounded transport close reason.")
   (lock
    :initform (make-lock "Autolith browser CDP")
    :reader browser-cdp-connection-lock
    :documentation "The lock protecting correlation and events.")
   (condition
    :initform (make-condition-variable)
    :reader browser-cdp-connection-condition
    :documentation "The condition signaled for responses, events, and closure."))
  (:documentation "One correlated, event-aware CDP WebSocket connection."))

(-> browser-cdp--close-with-reason (browser-cdp-connection string) null)
(defun browser-cdp--close-with-reason (connection reason)
  "Mark CONNECTION closed with bounded REASON and wake every waiter."
  (with-lock-held ((browser-cdp-connection-lock connection))
    (setf (browser-cdp-connection-closed-p connection) t
          (browser-cdp-connection-close-reason connection)
          (bounded-string reason))
    (condition-notify (browser-cdp-connection-condition connection)))
  nil)

(-> browser-cdp-handle-message (browser-cdp-connection t) null)
(defun browser-cdp-handle-message (connection message)
  "Correlate one bounded CDP MESSAGE or record it as an event."
  (unless (stringp message)
    (browser-cdp--close-with-reason connection "CDP returned a binary message.")
    (return-from browser-cdp-handle-message nil))
  (when (> (length message) *nous-browser-maximum-cdp-message-bytes*)
    (browser-cdp--close-with-reason connection "CDP message exceeded the size limit.")
    (return-from browser-cdp-handle-message nil))
  (let ((document (handler-case (json-decode message) (error () nil))))
    (unless (json-object-p document)
      (browser-cdp--close-with-reason connection "CDP returned malformed JSON.")
      (return-from browser-cdp-handle-message nil))
    (with-lock-held ((browser-cdp-connection-lock connection))
      (multiple-value-bind (identifier present-p) (gethash "id" document)
        (if present-p
            (let ((pending (gethash identifier
                                    (browser-cdp-connection-pending connection))))
              (when pending
                (setf (browser-cdp-pending-result pending) (json-get document "result")
                      (browser-cdp-pending-error pending) (json-get document "error")
                      (browser-cdp-pending-done-p pending) t)))
            (progn
              (incf (browser-cdp-connection-event-sequence connection))
              (push (cons (browser-cdp-connection-event-sequence connection) document)
                    (browser-cdp-connection-events connection))
              (when (> (length (browser-cdp-connection-events connection)) 256)
                (setf (browser-cdp-connection-events connection)
                      (subseq (browser-cdp-connection-events connection) 0 256)))))
        (condition-notify (browser-cdp-connection-condition connection)))))
  nil)

(-> browser-cdp--actual-connect (browser-cdp-connection string) browser-cdp-transport)
(defun browser-cdp--actual-connect (connection url)
  "Create and start one websocket-driver CDP connection to URL."
  (let* ((socket (make-client url :max-length *nous-browser-maximum-cdp-message-bytes*))
         (transport (make-instance 'websocket-browser-cdp-transport :socket socket)))
    (on :message socket
        (lambda (message)
          (browser-cdp-handle-message connection message)))
    (on :error socket
        (lambda (condition)
          (browser-cdp--close-with-reason connection (format nil "~A" condition))))
    (on :close socket
        (lambda (&key code reason)
          (browser-cdp--close-with-reason
           connection
           (format nil "WebSocket closed~@[ (~A)~]~@[: ~A~]." code reason))))
    (start-connection socket)
    transport))

(-> browser-cdp-connect (string) browser-cdp-connection)
(defun browser-cdp-connect (url)
  "Connect to a bounded WS(S) CDP URL and return a correlated connection."
  (let* ((uri (handler-case (quri:uri url) (error () nil)))
         (scheme (and uri (quri:uri-scheme uri))))
    (unless (and uri (member scheme '("ws" "wss") :test #'string=)
                 (non-empty-string-p (quri:uri-host uri)))
      (nous-browser--fail ':connection "Managed browser returned an invalid CDP URL."))
    (let ((connection (make-instance 'browser-cdp-connection)))
      (handler-case
          (setf (browser-cdp-connection-transport connection)
                (if *nous-browser-cdp-connect-function*
                    (funcall *nous-browser-cdp-connect-function* connection url)
                    (browser-cdp--actual-connect connection url)))
        (error (condition)
          (error 'nous-browser-error
                 :message (format nil "Could not connect to managed browser CDP: ~A"
                                  condition)
                 :tool-name "browser"
                 :stage ':connection)))
      connection)))

(-> browser-cdp-close (browser-cdp-connection) null)
(defun browser-cdp-close (connection)
  "Close CONNECTION and wake every pending request."
  (let ((transport (browser-cdp-connection-transport connection)))
    (when transport (browser-cdp-transport-close transport)))
  (browser-cdp--close-with-reason connection "CDP connection closed.")
  nil)

(-> browser-cdp-command
    (browser-cdp-connection string json-object &key (:session-id (option string))
                            (:timeout real))
    json-object)
(defun browser-cdp-command
    (connection method parameters
     &key session-id (timeout *nous-browser-command-timeout-seconds*))
  "Send one correlated CDP METHOD and return its result object."
  (let ((pending (make-instance 'browser-cdp-pending))
        (identifier nil)
        (message nil))
    (with-lock-held ((browser-cdp-connection-lock connection))
      (when (browser-cdp-connection-closed-p connection)
        (nous-browser--fail ':connection "CDP is closed: ~A"
                            (or (browser-cdp-connection-close-reason connection)
                                "unknown reason")))
      (setf identifier (incf (browser-cdp-connection-next-id connection))
            (gethash identifier (browser-cdp-connection-pending connection)) pending
            message
            (json-encode
             (let ((document (json-object "id" identifier
                                          "method" method
                                          "params" parameters)))
               (when session-id (setf (gethash "sessionId" document) session-id))
               document))))
    (when (> (length message) *nous-browser-maximum-cdp-message-bytes*)
      (remhash identifier (browser-cdp-connection-pending connection))
      (nous-browser--fail ':command "CDP command ~A exceeded the message limit." method))
    (handler-case
        (browser-cdp-transport-send (browser-cdp-connection-transport connection)
                                    message)
      (error (condition)
        (with-lock-held ((browser-cdp-connection-lock connection))
          (remhash identifier (browser-cdp-connection-pending connection)))
        (error 'nous-browser-error
               :message (format nil "Could not send CDP command ~A: ~A" method condition)
               :tool-name "browser"
               :stage ':command
               :command method)))
    (let ((deadline (+ (/ (get-internal-real-time) internal-time-units-per-second)
                       timeout)))
      (with-lock-held ((browser-cdp-connection-lock connection))
        (loop until (browser-cdp-pending-done-p pending)
              do (when (browser-cdp-connection-closed-p connection)
                   (remhash identifier (browser-cdp-connection-pending connection))
                   (nous-browser--fail ':connection "CDP closed while waiting for ~A." method))
                 (let ((remaining
                         (- deadline
                            (/ (get-internal-real-time)
                               internal-time-units-per-second))))
                   (when (<= remaining 0)
                     (remhash identifier (browser-cdp-connection-pending connection))
                     (error 'nous-browser-error
                            :message (format nil "CDP command ~A timed out." method)
                            :tool-name "browser"
                            :stage ':command
                            :command method))
                   (condition-wait (browser-cdp-connection-condition connection)
                                   (browser-cdp-connection-lock connection)
                                   :timeout remaining)))
        (remhash identifier (browser-cdp-connection-pending connection))))
    (when (browser-cdp-pending-error pending)
      (error 'nous-browser-error
             :message (format nil "CDP command ~A failed: ~A"
                              method
                              (json-encode (browser-cdp-pending-error pending)))
             :tool-name "browser"
             :stage ':command
             :command method))
    (or (browser-cdp-pending-result pending) (json-object))))

(-> browser-cdp-event-sequence (browser-cdp-connection) integer)
(defun browser-cdp-event-sequence (connection)
  "Return CONNECTION's current event sequence."
  (with-lock-held ((browser-cdp-connection-lock connection))
    (browser-cdp-connection-event-sequence connection)))

(-> browser-cdp-wait-event
    (browser-cdp-connection string integer &key (:session-id (option string))
                            (:timeout real))
    (option json-object))
(defun browser-cdp-wait-event
    (connection method after-sequence
     &key session-id (timeout *nous-browser-action-wait-seconds*))
  "Wait boundedly for METHOD after AFTER-SEQUENCE, optionally in SESSION-ID."
  (let ((deadline (+ (/ (get-internal-real-time) internal-time-units-per-second)
                     timeout)))
    (with-lock-held ((browser-cdp-connection-lock connection))
      (loop
        (let ((match
                (find-if
                 (lambda (entry)
                   (let ((document (rest entry)))
                     (and (> (first entry) after-sequence)
                          (string= (or (json-get document "method") "") method)
                          (or (null session-id)
                              (string= (or (json-get document "sessionId") "")
                                       session-id)))))
                 (browser-cdp-connection-events connection))))
          (when match (return (rest match))))
        (when (browser-cdp-connection-closed-p connection) (return nil))
        (let ((remaining
                (- deadline
                   (/ (get-internal-real-time) internal-time-units-per-second))))
          (when (<= remaining 0) (return nil))
          (condition-wait (browser-cdp-connection-condition connection)
                          (browser-cdp-connection-lock connection)
                          :timeout remaining))))))


;;;; -- Agent Browser Runtime --

(defclass nous-browser-runtime ()
  ((configuration
    :initarg :configuration
    :reader nous-browser-runtime-configuration
    :type configuration
    :documentation "The agent configuration locating independent Nous OAuth.")
   (logical-key
    :initarg :logical-key
    :reader nous-browser-runtime-logical-key
    :type non-empty-string
    :documentation "The agent-unique logical browser identity.")
   (create-key
    :initform (format nil "browser-use-session-create:~A" (make-identifier))
    :accessor nous-browser-runtime-create-key
    :type non-empty-string
    :documentation "The create idempotency key retained across retryable failures.")
   (session-id
    :initform nil
    :accessor nous-browser-runtime-session-id
    :type (option string)
    :documentation "The managed Browser Use session identifier.")
   (cdp-url
    :initform nil
    :accessor nous-browser-runtime-cdp-url
    :type (option string)
    :documentation "The managed CDP WebSocket URL.")
   (connection
    :initform nil
    :accessor nous-browser-runtime-connection
    :type (option browser-cdp-connection)
    :documentation "The lazy correlated CDP connection.")
   (target-id
    :initform nil
    :accessor nous-browser-runtime-target-id
    :type (option string)
    :documentation "The browser page target identifier.")
   (target-session-id
    :initform nil
    :accessor nous-browser-runtime-target-session-id
    :type (option string)
    :documentation "The flattened CDP target session identifier.")
   (snapshot-generation
    :initform 0
    :accessor nous-browser-runtime-snapshot-generation
    :type integer
    :documentation "The generation owning the current stable element refs.")
   (references
    :initform (make-hash-table :test #'equal)
    :reader nous-browser-runtime-references
    :type hash-table
    :documentation "Current stable element refs mapped to backend DOM node IDs.")
   (state
    :initform ':not-created
    :accessor nous-browser-runtime-state
    :type keyword
    :documentation "The local lifecycle view of the managed browser.")
   (lock
    :initform (make-lock "Autolith managed browser")
    :reader nous-browser-runtime-lock
    :documentation "The lock serializing lifecycle and browser actions."))
  (:documentation "One lazy Nous-managed browser session owned by one agent."))

(-> nous-browser-runtime-create (configuration non-empty-string) nous-browser-runtime)
(defun nous-browser-runtime-create (configuration logical-key)
  "Create a lazy managed browser runtime for one agent LOGICAL-KEY."
  (make-instance 'nous-browser-runtime
                 :configuration configuration
                 :logical-key logical-key))

(-> nous-browser-runtime--create-session (nous-browser-runtime) null)
(defun nous-browser-runtime--create-session (runtime)
  "Create RUNTIME's managed session exactly once."
  (when (nous-browser-runtime-session-id runtime)
    (return-from nous-browser-runtime--create-session nil))
  (setf (nous-browser-runtime-state runtime) ':creating)
  (multiple-value-bind (body status headers)
      (nous-browser--request
       (nous-browser-runtime-configuration runtime)
       :post "/browsers"
       (json-object "timeout" 5 "proxyCountryCode" "us")
       :idempotency-key (nous-browser-runtime-create-key runtime)
       :connect-timeout 2
       :read-timeout 30)
    (declare (ignore headers))
    (unless (<= 200 status 299)
      (unless (nous-browser--preserve-create-key-p status body)
        (setf (nous-browser-runtime-create-key runtime)
              (format nil "browser-use-session-create:~A" (make-identifier))))
      (setf (nous-browser-runtime-state runtime) ':failed)
      (error 'nous-browser-error
             :message (format nil "Failed to create managed browser: HTTP ~D ~A"
                              status (bounded-string body))
             :tool-name "browser"
             :stage ':create
             :status status))
    (let* ((document (nous-browser--document body ':create status))
           (session-id (json-get document "id"))
           (cdp-url (or (json-get document "cdpUrl")
                        (json-get document "connectUrl"))))
      (unless (and (non-empty-string-p session-id)
                   (non-empty-string-p cdp-url))
        (setf (nous-browser-runtime-state runtime) ':failed)
        (nous-browser--fail
         ':create
         "Managed browser create response requires id and cdpUrl/connectUrl."))
      (setf (nous-browser-runtime-session-id runtime) session-id
            (nous-browser-runtime-cdp-url runtime) cdp-url
            (nous-browser-runtime-state runtime) ':created)))
  nil)

(-> nous-browser-runtime--ensure-page (nous-browser-runtime) null)
(defun nous-browser-runtime--ensure-page (runtime)
  "Create, connect, and attach RUNTIME to one page target lazily."
  (nous-browser-runtime--create-session runtime)
  (unless (nous-browser-runtime-connection runtime)
    (setf (nous-browser-runtime-state runtime) ':connecting
          (nous-browser-runtime-connection runtime)
          (browser-cdp-connect (nous-browser-runtime-cdp-url runtime))))
  (unless (nous-browser-runtime-target-session-id runtime)
    (let* ((connection (nous-browser-runtime-connection runtime))
           (targets-result
             (browser-cdp-command connection "Target.getTargets" (json-object)))
           (targets (json-get targets-result "targetInfos"))
           (target
             (and (vectorp targets)
                  (find-if
                   (lambda (info)
                     (and (json-object-p info)
                          (string= (or (json-get info "type") "") "page")))
                   targets)))
           (target-id
             (or (and target (json-get target "targetId"))
                 (json-get
                  (browser-cdp-command
                   connection "Target.createTarget" (json-object "url" "about:blank"))
                  "targetId"))))
      (unless (non-empty-string-p target-id)
        (nous-browser--fail ':connection "CDP did not provide a page target."))
      (let ((session-id
              (json-get
               (browser-cdp-command
                connection "Target.attachToTarget"
                (json-object "targetId" target-id "flatten" t))
               "sessionId")))
        (unless (non-empty-string-p session-id)
          (nous-browser--fail ':connection "CDP did not attach to the page target."))
        (setf (nous-browser-runtime-target-id runtime) target-id
              (nous-browser-runtime-target-session-id runtime) session-id)
        (dolist (method '("Page.enable" "DOM.enable" "Runtime.enable"
                          "Accessibility.enable"))
          (browser-cdp-command connection method (json-object)
                               :session-id session-id))
        (setf (nous-browser-runtime-state runtime) ':ready))))
  nil)

(-> nous-browser-runtime-command
    (nous-browser-runtime string json-object) json-object)
(defun nous-browser-runtime-command (runtime method parameters)
  "Run one page-session CDP METHOD through RUNTIME."
  (nous-browser-runtime--ensure-page runtime)
  (browser-cdp-command (nous-browser-runtime-connection runtime)
                       method parameters
                       :session-id (nous-browser-runtime-target-session-id runtime)))

(-> nous-browser-runtime--ready-state (nous-browser-runtime) string)
(defun nous-browser-runtime--ready-state (runtime)
  "Return the page document.readyState string."
  (let* ((result
           (nous-browser-runtime-command
            runtime "Runtime.evaluate"
            (json-object "expression" "document.readyState"
                         "returnByValue" t)))
         (remote (json-get result "result"))
         (value (and (json-object-p remote) (json-get remote "value"))))
    (if (stringp value) value "")))

(-> nous-browser-runtime-wait-ready (nous-browser-runtime) null)
(defun nous-browser-runtime-wait-ready (runtime)
  "Wait boundedly until the current page is interactive or complete."
  (let ((deadline (+ (/ (get-internal-real-time) internal-time-units-per-second)
                     *nous-browser-action-wait-seconds*)))
    (loop
      (when (member (nous-browser-runtime--ready-state runtime)
                    '("interactive" "complete") :test #'string=)
        (return))
      (when (>= (/ (get-internal-real-time) internal-time-units-per-second)
                deadline)
        (return))
      (sleep 0.1)))
  nil)

(-> nous-browser-runtime-navigate (nous-browser-runtime string) string)
(defun nous-browser-runtime-navigate (runtime url)
  "Navigate RUNTIME to one validated public HTTP(S) URL."
  (unless (nous-web--public-url-p url)
    (nous-browser--fail ':action "Browser navigation requires a public HTTP(S) URL."))
  (with-lock-held ((nous-browser-runtime-lock runtime))
    (nous-browser-runtime--ensure-page runtime)
    (let* ((connection (nous-browser-runtime-connection runtime))
           (sequence (browser-cdp-event-sequence connection))
           (result (nous-browser-runtime-command
                    runtime "Page.navigate" (json-object "url" url))))
      (when (json-get result "errorText")
        (nous-browser--fail ':action "Navigation failed: ~A"
                            (json-get result "errorText")))
      (browser-cdp-wait-event
       connection "Page.loadEventFired" sequence
       :session-id (nous-browser-runtime-target-session-id runtime)
       :timeout *nous-browser-action-wait-seconds*)
      (nous-browser-runtime-wait-ready runtime)
      (clrhash (nous-browser-runtime-references runtime))
      (format nil "Navigated to ~A" url))))

(-> nous-browser--remote-value (json-object) t)
(defun nous-browser--remote-value (result)
  "Return Runtime.evaluate's by-value payload from RESULT."
  (let ((remote (json-get result "result")))
    (and (json-object-p remote) (json-get remote "value"))))

(-> nous-browser-runtime-current-url (nous-browser-runtime) string)
(defun nous-browser-runtime-current-url (runtime)
  "Return RUNTIME's current page URL."
  (let ((value
          (nous-browser--remote-value
           (nous-browser-runtime-command
            runtime "Runtime.evaluate"
            (json-object "expression" "location.href" "returnByValue" t)))))
    (if (stringp value) value "")))

(-> nous-browser--ax-value (t) string)
(defun nous-browser--ax-value (object)
  "Return an Accessibility property object's string value."
  (let ((value (and (json-object-p object) (json-get object "value"))))
    (if (stringp value) value "")))

(-> nous-browser-runtime-snapshot (nous-browser-runtime) string)
(defun nous-browser-runtime-snapshot (runtime)
  "Return a bounded accessibility snapshot with stable current-generation refs."
  (with-lock-held ((nous-browser-runtime-lock runtime))
    (let* ((result (nous-browser-runtime-command
                    runtime "Accessibility.getFullAXTree" (json-object)))
           (nodes (json-get result "nodes"))
           (references (nous-browser-runtime-references runtime))
           (generation (incf (nous-browser-runtime-snapshot-generation runtime)))
           (count 0))
      (clrhash references)
      (with-output-to-string (output)
        (format output "URL: ~A~%Snapshot: ~D~%"
                (nous-browser-runtime-current-url runtime) generation)
        (when (vectorp nodes)
          (loop for node across nodes
                while (< count *nous-browser-maximum-snapshot-nodes*)
                do (when (and (json-object-p node)
                              (not (json-get node "ignored")))
                     (let* ((role (nous-browser--ax-value (json-get node "role")))
                            (name (nous-browser--ax-value (json-get node "name")))
                            (backend-id (json-get node "backendDOMNodeId")))
                       (when (and (or (non-empty-string-p role)
                                      (non-empty-string-p name))
                                  (< (file-position output)
                                     *nous-browser-maximum-snapshot-characters*))
                         (incf count)
                         (if (integerp backend-id)
                             (let ((reference (format nil "e~D" count)))
                               (setf (gethash reference references) backend-id)
                               (format output "[~A] ~A~@[ \"~A\"~]~%"
                                       reference
                                       (if (non-empty-string-p role) role "node")
                                       (and (non-empty-string-p name)
                                           (bounded-string name :limit 300))))
                             (format output "    ~A~@[ \"~A\"~]~%"
                                     (if (non-empty-string-p role) role "node")
                                     (and (non-empty-string-p name)
                                         (bounded-string name :limit 300)))))))))))))

(-> nous-browser-runtime--backend-id (nous-browser-runtime string) integer)
(defun nous-browser-runtime--backend-id (runtime reference)
  "Resolve REFERENCE in RUNTIME's current snapshot or signal stale reference."
  (let ((backend-id (gethash reference (nous-browser-runtime-references runtime))))
    (unless (integerp backend-id)
      (error 'nous-browser-stale-reference
             :message (format nil "Unknown or stale browser element reference ~A; take a new snapshot."
                              reference)
             :tool-name "browser"
             :stage ':action
             :reference reference))
    backend-id))

(-> nous-browser-runtime--element-center
    (nous-browser-runtime string) (values real real))
(defun nous-browser-runtime--element-center (runtime reference)
  "Scroll REFERENCE into view and return the center of its content box."
  (let ((backend-id (nous-browser-runtime--backend-id runtime reference)))
    (nous-browser-runtime-command
     runtime "DOM.scrollIntoViewIfNeeded"
     (json-object "backendNodeId" backend-id))
    (let* ((result (nous-browser-runtime-command
                    runtime "DOM.getBoxModel"
                    (json-object "backendNodeId" backend-id)))
           (model (json-get result "model"))
           (content (and (json-object-p model) (json-get model "content"))))
      (unless (and (vectorp content) (= (length content) 8))
        (nous-browser--fail ':action "Element ~A has no clickable box." reference))
      (values (/ (+ (aref content 0) (aref content 2)
                    (aref content 4) (aref content 6)) 4)
              (/ (+ (aref content 1) (aref content 3)
                    (aref content 5) (aref content 7)) 4)))))

(-> nous-browser-runtime-click (nous-browser-runtime string) string)
(defun nous-browser-runtime-click (runtime reference)
  "Click current snapshot REFERENCE through CDP Input events."
  (with-lock-held ((nous-browser-runtime-lock runtime))
    (multiple-value-bind (x y)
        (nous-browser-runtime--element-center runtime reference)
      (dolist (type '("mousePressed" "mouseReleased"))
        (nous-browser-runtime-command
         runtime "Input.dispatchMouseEvent"
         (json-object "type" type "x" x "y" y
                      "button" "left" "clickCount" 1)))
      (sleep 0.2)
      (nous-browser-runtime-wait-ready runtime)
      (clrhash (nous-browser-runtime-references runtime))
      (format nil "Clicked ~A." reference))))

(-> nous-browser-runtime-type (nous-browser-runtime string string) string)
(defun nous-browser-runtime-type (runtime reference text)
  "Focus REFERENCE and insert secret-safe TEXT without returning it."
  (when (> (length text) 10000)
    (nous-browser--fail ':action "browser.type text exceeds 10000 characters."))
  (with-lock-held ((nous-browser-runtime-lock runtime))
    (multiple-value-bind (x y)
        (nous-browser-runtime--element-center runtime reference)
      (dolist (type '("mousePressed" "mouseReleased"))
        (nous-browser-runtime-command
         runtime "Input.dispatchMouseEvent"
         (json-object "type" type "x" x "y" y
                      "button" "left" "clickCount" 1)))
      (nous-browser-runtime-command
       runtime "Input.insertText" (json-object "text" text))
      (format nil "Typed text into ~A." reference))))

(-> nous-browser-runtime-press (nous-browser-runtime string) string)
(defun nous-browser-runtime-press (runtime key)
  "Dispatch one bounded keyboard KEY to the focused page element."
  (unless (<= 1 (length key) 64)
    (nous-browser--fail ':action "browser.press key must contain 1 to 64 characters."))
  (with-lock-held ((nous-browser-runtime-lock runtime))
    (nous-browser-runtime-command
     runtime "Input.dispatchKeyEvent"
     (json-object "type" "keyDown" "key" key))
    (nous-browser-runtime-command
     runtime "Input.dispatchKeyEvent"
     (json-object "type" "keyUp" "key" key))
    (sleep 0.1)
    (nous-browser-runtime-wait-ready runtime)
    (clrhash (nous-browser-runtime-references runtime))
    (format nil "Pressed ~A." key)))

(-> nous-browser-runtime-scroll (nous-browser-runtime integer) string)
(defun nous-browser-runtime-scroll (runtime amount)
  "Scroll the page vertically by bounded pixel AMOUNT."
  (unless (<= -10000 amount 10000)
    (nous-browser--fail ':action "browser.scroll amount must be between -10000 and 10000."))
  (with-lock-held ((nous-browser-runtime-lock runtime))
    (nous-browser-runtime-command
     runtime "Runtime.evaluate"
     (json-object "expression" (format nil "window.scrollBy(0, ~D)" amount)
                  "returnByValue" t))
    (clrhash (nous-browser-runtime-references runtime))
    (format nil "Scrolled ~D pixels." amount)))

(-> nous-browser-runtime-back (nous-browser-runtime) string)
(defun nous-browser-runtime-back (runtime)
  "Navigate the current page backward and wait boundedly."
  (with-lock-held ((nous-browser-runtime-lock runtime))
    (nous-browser-runtime-command
     runtime "Runtime.evaluate"
     (json-object "expression" "history.back()" "returnByValue" t))
    (sleep 0.2)
    (nous-browser-runtime-wait-ready runtime)
    (clrhash (nous-browser-runtime-references runtime))
    (format nil "Navigated back to ~A" (nous-browser-runtime-current-url runtime))))

(-> nous-browser-runtime-screenshot
    (nous-browser-runtime conversation) image-attachment)
(defun nous-browser-runtime-screenshot (runtime conversation)
  "Capture a native PNG screenshot and persist it as a private attachment."
  (with-lock-held ((nous-browser-runtime-lock runtime))
    (let* ((result (nous-browser-runtime-command
                    runtime "Page.captureScreenshot"
                    (json-object "format" "png" "fromSurface" t)))
           (data (json-get result "data")))
      (unless (non-empty-string-p data)
        (nous-browser--fail ':response "CDP screenshot response omitted PNG data."))
      (let* ((root (conversation-image-artifact-root conversation))
             (temporary (merge-pathnames
                         (make-pathname :name (format nil ".browser-~A" (make-identifier))
                                        :type "png")
                         root))
             (bytes (base64-string-to-usb8-array data)))
        (when (> (length bytes) *nous-browser-maximum-cdp-message-bytes*)
          (nous-browser--fail ':response "Browser screenshot exceeded the size limit."))
        (ensure-directories-exist temporary)
        (unwind-protect
             (progn
               (with-open-file (stream temporary
                                       :direction ':output
                                       :if-exists ':supersede
                                       :if-does-not-exist ':create
                                       :element-type '(unsigned-byte 8))
                 (write-sequence bytes stream))
               (image-input-prepare temporary root))
          (when (probe-file temporary) (delete-file temporary)))))))

(-> nous-browser-runtime-status ((option nous-browser-runtime)) list)
(defun nous-browser-runtime-status (runtime)
  "Return a portable lifecycle status without creating a browser."
  (if runtime
      (list :state (nous-browser-runtime-state runtime)
            :logical-key (nous-browser-runtime-logical-key runtime)
            :session-id (nous-browser-runtime-session-id runtime))
      (list :state ':not-created :logical-key nil :session-id nil)))

(-> nous-browser-runtime-close (nous-browser-runtime) null)
(defun nous-browser-runtime-close (runtime)
  "Close CDP and stop RUNTIME's managed Browser Use session idempotently."
  (with-lock-held ((nous-browser-runtime-lock runtime))
    (let ((connection (nous-browser-runtime-connection runtime))
          (session-id (nous-browser-runtime-session-id runtime)))
      (when connection
        (browser-cdp-close connection)
        (setf (nous-browser-runtime-connection runtime) nil))
      (when session-id
        (unwind-protect
             (multiple-value-bind (body status headers)
                 (nous-browser--request
                  (nous-browser-runtime-configuration runtime)
                  :patch
                  (format nil "/browsers/~A" session-id)
                  (json-object "action" "stop")
                  :connect-timeout 1
                  :read-timeout 10)
               (declare (ignore body headers))
               (unless (member status '(200 201 204))
                 (error 'nous-browser-error
                        :message (format nil "Failed to stop managed browser ~A: HTTP ~D."
                                         session-id status)
                        :tool-name "browser"
                        :stage ':stop
                        :status status
                        :session-id session-id)))
          (setf (nous-browser-runtime-session-id runtime) nil
                (nous-browser-runtime-cdp-url runtime) nil
                (nous-browser-runtime-target-id runtime) nil
                (nous-browser-runtime-target-session-id runtime) nil))))
    (clrhash (nous-browser-runtime-references runtime))
    (setf (nous-browser-runtime-state runtime) ':closed))
  nil)
