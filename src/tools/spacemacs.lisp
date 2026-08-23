(in-package #:autolith)

;;;; -- Spacemacs Tool --

(defparameter *spacemacs-tool-registry-version* 1
  "The readable live Spacemacs bridge record version.")

(defparameter *spacemacs-tool-timeout-seconds* 5
  "Maximum seconds one emacsclient bridge request may run.")

(defparameter *spacemacs-tool-output-limit* 32768
  "Maximum emacsclient output characters retained by one request.")

(defparameter *spacemacs-tool-maximum-text-characters* 131072
  "Maximum text accepted by one editor operation.")

(defparameter *spacemacs-tool-maximum-server-name-characters* 200
  "Maximum Emacs server name accepted by the tool.")

(defparameter *spacemacs-tool-maximum-pid*
  (1- (ash 1 (1- (sb-alien:alien-size sb-posix::pid-t))))
  "Maximum positive value representable by the platform's signed pid_t.")

(defparameter *spacemacs-tool-maximum-command-argument-octets* 126976
  "Maximum UTF-8 octets in the fixed emacsclient expression argument.

This stays below Linux's 128 KiB per-argument exec limit.")

(defparameter *spacemacs-tool-operations*
  '("status" "visit-file" "focus-buffer" "goto-line" "insert-text"
    "replace-region" "save-buffer" "message")
  "Closed editor operation set exposed to the model.")

(defclass spacemacs-command-tool (tool)
  ()
  (:documentation "Send one scoped operation to an opted-in live Spacemacs session."))

(defmethod tool-provider-round-trip-barrier-p ((tool spacemacs-command-tool))
  "Require the provider to inspect each editor operation before requesting another."
  (declare (ignore tool))
  t)

(defmethod tool-compact-result-visible-p ((tool spacemacs-command-tool))
  "Keep successful editor operations visible in compact presentation."
  (declare (ignore tool))
  t)

(-> spacemacs-tool-parameters () json-object)
(defun spacemacs-tool-parameters ()
  "Return the closed schema for one scoped Spacemacs command."
  (tool-object-schema
   (json-object
    "operation"
    (json-object
     "type" "string"
     "enum" (coerce *spacemacs-tool-operations* 'vector)
     "description" "The scoped editor operation to perform.")
    "path" (tool-string-property
            "Workspace-relative or absolute file for visit-file.")
    "buffer" (tool-string-property
              "Existing Emacs buffer name for focus-buffer.")
    "line" (json-object
            "type" "integer"
            "minimum" 1
            "description" "One-based destination line.")
    "column" (json-object
              "type" "integer"
              "minimum" 0
              "description" "Zero-based destination column; defaults to 0.")
    "text" (json-object
            "type" "string"
            "maxLength" *spacemacs-tool-maximum-text-characters*
            "description" "Text for insert-text, replace-region, or message.")
    "server" (json-object
              "type" "string"
              "maxLength" *spacemacs-tool-maximum-server-name-characters*
              "description"
              "Optional exact Emacs server name when more than one bridge is live."))
   '("operation")))


;;;; -- Bridge Discovery --

(-> spacemacs-tool--registry-directory (tool-context) pathname)
(defun spacemacs-tool--registry-directory (context)
  "Return CONTEXT's private live Spacemacs bridge directory."
  (merge-pathnames
   "spacemacs/"
   (configuration-state-root (tool-context-configuration context))))

(-> spacemacs-tool--proper-list-p (t) boolean)
(defun spacemacs-tool--proper-list-p (value)
  "Return true when VALUE is a finite proper list."
  (not
   (null
    (handler-case
        (let ((length (list-length value)))
          (and length t))
      (type-error () nil)))))

(-> spacemacs-tool--server-name-p (t) boolean)
(defun spacemacs-tool--server-name-p (value)
  "Return true when VALUE is one bounded portable Emacs server name."
  (and (non-empty-string-p value)
       (<= (length value) *spacemacs-tool-maximum-server-name-characters*)
       (every (lambda (character)
                (or (alphanumericp character)
                    (find character "._-")))
              value)))

(-> spacemacs-tool--pid-p (t) boolean)
(defun spacemacs-tool--pid-p (value)
  "Return true when VALUE fits the platform's positive signed pid_t range."
  (and (integerp value)
       (<= 1 value *spacemacs-tool-maximum-pid*)))

(-> spacemacs-tool--record-p (t) boolean)
(defun spacemacs-tool--record-p (record)
  "Return true when RECORD is one supported Spacemacs bridge record."
  (and (spacemacs-tool--proper-list-p record)
       (let ((properties (rest record)))
         (and (evenp (length properties))
              (eq (first record) ':autolith-spacemacs)
              (let ((version (getf properties :version)))
                (and (integerp version)
                     (= version *spacemacs-tool-registry-version*)))
              (spacemacs-tool--pid-p (getf properties :pid))
              (spacemacs-tool--server-name-p
               (getf properties :server-name))
              (typep (getf properties :created-at) '(integer 1))))))

(-> spacemacs-tool--read-record (pathname) (option list))
(defun spacemacs-tool--read-record (pathname)
  "Return PATHNAME's valid complete bridge record, or nil."
  (handler-case
      (multiple-value-bind (record complete-p)
          (snapshot-read pathname)
        (and complete-p
             (spacemacs-tool--record-p record)
             record))
    (error () nil)))

(-> spacemacs-tool--pid-live-p (integer) boolean)
(defun spacemacs-tool--pid-live-p (pid)
  "Return true when PID names a live local process."
  (and
   (spacemacs-tool--pid-p pid)
   (handler-case
       (progn
         (sb-posix:kill pid 0)
         t)
     (sb-posix:syscall-error (condition)
       (not (= (sb-posix:syscall-errno condition) sb-posix:esrch)))
     (error () nil))))

(-> spacemacs-tool--records (tool-context) list)
(defun spacemacs-tool--records (context)
  "Return distinct live bridge records newest first for CONTEXT."
  (let ((records
          (sort
           (loop for pathname in
                 (uiop:directory-files
                  (spacemacs-tool--registry-directory context) "*.sexp")
                 for record = (spacemacs-tool--read-record pathname)
                 when (and record
                           (spacemacs-tool--pid-live-p
                            (getf (rest record) :pid)))
                   collect record)
           #'>
           :key (lambda (record) (getf (rest record) :created-at))))
        (seen (make-hash-table :test #'equal)))
    (loop for record in records
          for server = (getf (rest record) :server-name)
          unless (gethash server seen)
            collect (progn
                      (setf (gethash server seen) t)
                      record))))


(-> spacemacs-tool--server (tool-context json-object) string)
(defun spacemacs-tool--server (context arguments)
  "Return the requested or uniquely discovered live Emacs server."
  (let ((requested (tool-argument arguments "server"))
        (records (spacemacs-tool--records context)))
    (when (and requested
               (not (spacemacs-tool--server-name-p requested)))
      (error 'tool-error
             :message "spacemacs.command received an invalid server name."
             :tool-name "spacemacs.command"))
    (cond
      ((null records)
       (error 'tool-error
              :message
              "No live Spacemacs bridge was found. Enable the Autolith Spacemacs layer and its editor bridge."
              :tool-name "spacemacs.command"))
      (requested
       (if (find requested records
                 :test #'string=
                 :key (lambda (record)
                        (getf (rest record) :server-name)))
           requested
           (error 'tool-error
                  :message
                  (format nil
                          "No live Spacemacs bridge uses server ~S. Available servers: ~{~A~^, ~}."
                          requested
                          (mapcar (lambda (record)
                                    (getf (rest record) :server-name))
                                  records))
                  :tool-name "spacemacs.command")))
      ((rest records)
       (error 'tool-error
              :message
              (format nil
                      "More than one Spacemacs bridge is live. Retry with server set to one of: ~{~A~^, ~}."
                      (mapcar (lambda (record)
                                (getf (rest record) :server-name))
                              records))
              :tool-name "spacemacs.command"))
      (t
       (getf (rest (first records)) :server-name)))))


;;;; -- Request Validation --

(-> spacemacs-tool--integer
    (json-object string &key (:required boolean) (:minimum integer)
     (:default (option integer)))
    (option integer))
(defun spacemacs-tool--integer
    (arguments name &key required (minimum 0) default)
  "Return integer NAME from ARGUMENTS under the requested bounds."
  (let ((value (tool-argument arguments name)))
    (cond
      ((null value)
       (if required
           (error 'tool-error
                  :message (format nil "spacemacs.command requires ~A." name)
                  :tool-name "spacemacs.command")
           default))
      ((and (integerp value) (>= value minimum))
       value)
      (t
       (error 'tool-error
              :message
              (format nil "spacemacs.command argument ~A must be an integer of at least ~D."
                      name minimum)
              :tool-name "spacemacs.command")))))

(-> spacemacs-tool--text (json-object string &key (:nonempty-p boolean)) string)
(defun spacemacs-tool--text (arguments name &key nonempty-p)
  "Return bounded string NAME from ARGUMENTS."
  (let ((value (tool-argument arguments name :required t)))
    (unless (and (stringp value)
                 (or (not nonempty-p) (plusp (length value))))
      (error 'tool-error
             :message (format nil "spacemacs.command requires string ~A." name)
             :tool-name "spacemacs.command"))
    (when (> (length value) *spacemacs-tool-maximum-text-characters*)
      (error 'tool-error
             :message
             (format nil "spacemacs.command argument ~A exceeds the ~D-character limit."
                     name *spacemacs-tool-maximum-text-characters*)
             :tool-name "spacemacs.command"))
    value))

(-> spacemacs-tool--workspace-root (tool-context) pathname)
(defun spacemacs-tool--workspace-root (context)
  "Return CONTEXT's canonical workspace directory."
  (workspace-tool--canonical-path
   (configuration-working-directory (tool-context-configuration context))))

(-> spacemacs-tool--regular-file-p (pathname) boolean)
(defun spacemacs-tool--regular-file-p (path)
  "Return true when PATH names an existing regular file."
  (handler-case
      (not
       (null
        (sb-posix:s-isreg
         (sb-posix:stat-mode (sb-posix:stat (namestring path))))))
    (error () nil)))

(-> spacemacs-tool--workspace-file (tool-context json-object) pathname)
(defun spacemacs-tool--workspace-file (context arguments)
  "Return ARGUMENTS' existing regular file confined to CONTEXT's workspace."
  (let* ((root (spacemacs-tool--workspace-root context))
         (path
          (workspace-tool--canonical-path
           (merge-pathnames
            (pathname (spacemacs-tool--text arguments "path" :nonempty-p t))
            root))))
    (unless (spacemacs-tool--regular-file-p path)
      (error 'tool-error
             :message (format nil "Spacemacs path is not a regular file: ~A" path)
             :tool-name "spacemacs.command"))
    (unless (uiop:subpathp path root)
      (error 'tool-error
             :message (format nil "Spacemacs path is outside workspace ~A: ~A"
                              root path)
             :tool-name "spacemacs.command"))
    path))

(-> spacemacs-tool--request (tool-context json-object) json-object)
(defun spacemacs-tool--request (context arguments)
  "Return one validated JSON editor request from ARGUMENTS."
  (let* ((operation (spacemacs-tool--text arguments "operation" :nonempty-p t))
         (root (spacemacs-tool--workspace-root context))
         (request
          (json-object
           "operation" operation
           "workspace" (namestring root))))
    (unless (member operation *spacemacs-tool-operations* :test #'string=)
      (error 'tool-error
             :message (format nil "Unsupported Spacemacs operation ~S." operation)
             :tool-name "spacemacs.command"))
    (flet ((put (name value)
             (setf (gethash name request) value)))
      (cond
        ((string= operation "status")
         nil)
        ((string= operation "visit-file")
         (put "path" (namestring (spacemacs-tool--workspace-file context arguments)))
         (put "line" (spacemacs-tool--integer
                      arguments "line" :minimum 1 :default 1))
         (put "column" (spacemacs-tool--integer
                        arguments "column" :minimum 0 :default 0)))
        ((string= operation "focus-buffer")
         (put "buffer" (spacemacs-tool--text
                        arguments "buffer" :nonempty-p t)))
        ((string= operation "goto-line")
         (put "line" (spacemacs-tool--integer
                      arguments "line" :required t :minimum 1))
         (put "column" (spacemacs-tool--integer
                        arguments "column" :minimum 0 :default 0)))
        ((member operation '("insert-text" "replace-region" "message")
                 :test #'string=)
         (put "text" (spacemacs-tool--text arguments "text")))
        ((string= operation "save-buffer")
         nil)))
    request))


;;;; -- emacsclient Boundary --

(-> spacemacs-tool--emacsclient-expression (json-object) string)
(defun spacemacs-tool--emacsclient-expression (request)
  "Return the fixed dispatcher expression carrying REQUEST as Base64 JSON."
  (let* ((encoded
           (usb8-array-to-base64-string (json-encode-utf8 request)))
         (expression
           (format nil "(autolith-editor-dispatch-json \"~A\")" encoded))
         (octets
           (sb-ext:string-to-octets expression :external-format ':utf-8)))
    (when (> (length octets)
             *spacemacs-tool-maximum-command-argument-octets*)
      (error 'tool-error
             :message
             (format nil
                     "The Spacemacs request exceeds the ~D-octet emacsclient argument limit."
                     *spacemacs-tool-maximum-command-argument-octets*)
             :tool-name "spacemacs.command"))
    expression))

(-> spacemacs-tool--read-emacsclient-value (string) string)
(defun spacemacs-tool--read-emacsclient-value (source)
  "Return one strictly decoded string value printed by emacsclient SOURCE."
  (flet ((whitespace-p (character)
           (find character '(#\Space #\Tab #\Newline #\Return)))
         (malformed ()
           (error 'tool-error
                  :message "emacsclient returned a malformed bridge response."
                  :tool-name "spacemacs.command")))
    (let ((start (position-if-not #'whitespace-p source))
          (end (position-if-not #'whitespace-p source :from-end t)))
      (unless (and start
                   end
                   (< start end)
                   (char= (char source start) #\")
                   (char= (char source end) #\"))
        (malformed))
      (with-output-to-string (stream)
        (loop with escaped-p = nil
              for position from (1+ start) below end
              for character = (char source position)
              do (cond
                   (escaped-p
                    (unless (find character '(#\\ #\"))
                      (malformed))
                    (write-char character stream)
                    (setf escaped-p nil))
                   ((char= character #\\)
                    (setf escaped-p t))
                   ((char= character #\")
                    (malformed))
                   (t
                    (write-char character stream)))
              finally (when escaped-p
                        (malformed)))))))

(-> spacemacs-tool--emacsclient-program () string)
(defun spacemacs-tool--emacsclient-program ()
  "Return the configured emacsclient program name."
  (let ((program (or (uiop:getenv "AUTOLITH_EMACSCLIENT") "emacsclient")))
    (unless (non-empty-string-p program)
      (error 'tool-error
             :message "AUTOLITH_EMACSCLIENT must name an executable."
             :tool-name "spacemacs.command"))
    program))

(-> spacemacs-tool--invoke (tool-context string json-object) tool-result)
(defun spacemacs-tool--invoke (context server request)
  "Invoke SERVER with REQUEST through a bounded direct emacsclient process."
  (let* ((configuration (tool-context-configuration context))
         (result
           (run-sandboxed
            (spacemacs-tool--emacsclient-program)
            (list "--alternate-editor=false"
                  "--socket-name" server
                  "--eval" (spacemacs-tool--emacsclient-expression request))
            :policy (external-sandbox-policy)
            :working-directory (configuration-working-directory configuration)
            :timeout *spacemacs-tool-timeout-seconds*
            :merge-output-p nil
            :output-limit *spacemacs-tool-output-limit*
            :error-output-limit *spacemacs-tool-output-limit*))
         (output (sandbox-result-output result))
         (error-output (sandbox-result-error-output result)))
    (cond
      ((sandbox-result-timed-out-p result)
       (tool-failure
        (format nil "The Spacemacs request timed out after ~D seconds."
                *spacemacs-tool-timeout-seconds*)))
      ((not (zerop (sandbox-result-exit-code result)))
       (tool-failure
        (with-output-to-string (stream)
          (format stream "emacsclient exited ~D."
                  (sandbox-result-exit-code result))
          (unless (zerop (length output))
            (format stream "~%Standard output:~%~A" output))
          (unless (zerop (length error-output))
            (format stream "~%Standard error:~%~A" error-output))
          (when (sandbox-result-output-truncated-p result)
            (format stream "~%[standard output truncated]"))
          (when (sandbox-result-error-output-truncated-p result)
            (format stream "~%[standard error truncated]")))))
      ((sandbox-result-output-truncated-p result)
       (tool-failure "The emacsclient bridge response exceeded its output limit."))
      (t
       (handler-case
           (let* ((json (spacemacs-tool--read-emacsclient-value output))
                  (response (json-decode json)))
             (unless (json-object-p response)
               (error "The Spacemacs bridge returned non-object JSON."))
             (if (eq (json-get response "ok") t)
                 (tool-success (json-encode response))
                 (let ((message (json-get response "error")))
                   (tool-failure
                    (if (stringp message)
                        message
                        "The Spacemacs operation failed.")))))
         (error (condition)
           (tool-failure
            (format nil "The Spacemacs bridge returned a malformed response: ~A"
                    condition))))))))

(defparameter *spacemacs-tool-invocation-function* #'spacemacs-tool--invoke
  "Function performing one authorized Spacemacs bridge invocation.")

(defmethod tool-execute ((tool spacemacs-command-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Authorize and perform one scoped command in a live Spacemacs session."
  (if (eq (tool-context-authorize-tool context tool arguments) ':deny)
      (tool-failure "The user denied this Spacemacs command.")
      (funcall *spacemacs-tool-invocation-function*
               context
               (spacemacs-tool--server context arguments)
               (spacemacs-tool--request context arguments))))
