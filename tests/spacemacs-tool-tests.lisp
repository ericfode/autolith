(in-package #:autolith)

;;;; -- Spacemacs Tool Test Support --

(-> spacemacs-tool-test--tool () spacemacs-command-tool)
(defun spacemacs-tool-test--tool ()
  "Return one standalone Spacemacs command tool for focused tests."
  (make-instance 'spacemacs-command-tool
                 :namespace "spacemacs"
                 :name "command"
                 :description "Test scoped Spacemacs commands."
                 :parameters (spacemacs-tool-parameters)))

(-> spacemacs-tool-test--context
    (configuration &key (:authorization (option function)))
    tool-context)
(defun spacemacs-tool-test--context (configuration &key authorization)
  "Return a test tool context for CONFIGURATION and AUTHORIZATION."
  (make-instance 'tool-context
                 :configuration configuration
                 :worker nil
                 :conversation
                 (conversation-create
                  configuration
                  :identifier (format nil "spacemacs-~A" (make-identifier)))
                 :tool-authorization-function authorization))

(-> spacemacs-tool-test--tool-error-p (function) boolean)
(defun spacemacs-tool-test--tool-error-p (function)
  "Return true when FUNCTION signals TOOL-ERROR."
  (handler-case
      (progn
        (funcall function)
        nil)
    (tool-error ()
      t)))

(-> spacemacs-tool-test--write-record
    (configuration string list)
    pathname)
(defun spacemacs-tool-test--write-record (configuration name record)
  "Write bridge RECORD named NAME below CONFIGURATION's state root."
  (let* ((directory
           (merge-pathnames "spacemacs/"
                            (configuration-state-root configuration)))
         (pathname (merge-pathnames name directory)))
    (uiop:ensure-all-directories-exist (list directory))
    (with-open-file (stream pathname
                            :direction ':output
                            :if-exists ':supersede
                            :if-does-not-exist ':create
                            :external-format ':utf-8)
      (with-standard-io-syntax
        (write record :stream stream)
        (terpri stream)))
    pathname))


;;;; -- Registry and Authorization --

(-> spacemacs-tool-test--registry () null)
(defun spacemacs-tool-test--registry ()
  "Test default registration, schema closure, presentation, and child scope."
  (let ((registry (make-default-tool-registry)))
    (unwind-protect
        (let* ((tool (tool-registry-find registry "spacemacs" "command"))
               (parameters (and tool (tool-parameters tool)))
               (properties (and parameters
                                (json-get parameters "properties")))
               (operation (and properties
                               (json-get properties "operation")))
               (definition
                 (task-agent-definition-create
                  :name "spacemacs-child-boundary"
                  :description "Exercise editor tool child confinement."
                  :instructions "Use every child-safe tool."
                  :tools ':all
                  :source ':test)))
          (test-assert (typep tool 'spacemacs-command-tool)
                       "the default registry contains spacemacs.command")
          (test-assert
           (and (string= (tool-namespace-description "spacemacs")
                         "Scoped operations in opted-in live Spacemacs sessions.")
                (eq (json-get parameters "additionalProperties") false)
                (equalp (json-get parameters "required") #("operation"))
                (equalp (json-get operation "enum")
                        (coerce *spacemacs-tool-operations* 'vector)))
           "spacemacs.command has one closed operation schema")
          (test-assert (tool-provider-round-trip-barrier-p tool)
                       "Spacemacs commands require a provider round trip")
          (test-assert (tool-compact-result-visible-p tool)
                       "Spacemacs commands remain visible in compact output")
          (test-assert
           (and (not (tool-child-safe-p tool))
                (not (task--definition-allows-tool-p definition tool)))
           "child agents cannot access spacemacs.command by default"))
      (tool-registry-close-runtime-state registry)))
  nil)

(-> spacemacs-tool-test--authorization () null)
(defun spacemacs-tool-test--authorization ()
  "Test denial and authorized fake invocation at the external tool boundary."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (tool (spacemacs-tool-test--tool)))
    (unwind-protect
        (progn
          (let* ((invoked-p nil)
                (*spacemacs-tool-invocation-function*
                  (lambda (&rest arguments)
                    (declare (ignore arguments))
                    (setf invoked-p t)
                    (tool-success "unexpected"))))
            (let* ((context
                     (spacemacs-tool-test--context
                      configuration
                      :authorization
                      (lambda (authorized-tool arguments)
                        (declare (ignore authorized-tool arguments))
                        ':deny)))
                   (result
                     (tool-execute tool context
                                   (json-object "operation" "status"))))
              (test-assert
               (and (not (tool-result-success-p result))
                    (not invoked-p)
                    (search "denied" (tool-result-content result)
                            :test #'char-equal))
               "denied Spacemacs commands never reach discovery or invocation")))
          (spacemacs-tool-test--write-record
           configuration
           "live.sexp"
           (list ':autolith-spacemacs
                 ':version 1
                 ':pid (sb-posix:getpid)
                 ':server-name "bridge-1"
                 ':created-at (get-universal-time)))
          (let* ((captured-server nil)
                (captured-request nil)
                (*spacemacs-tool-invocation-function*
                  (lambda (actual-context server request)
                    (declare (ignore actual-context))
                    (setf captured-server server
                          captured-request request)
                    (tool-success "fake bridge result"))))
            (let* ((context
                     (spacemacs-tool-test--context
                      configuration
                      :authorization
                      (lambda (authorized-tool arguments)
                        (declare (ignore authorized-tool arguments))
                        ':allow)))
                   (result
                     (tool-execute tool context
                                   (json-object "operation" "status"))))
              (test-assert
               (and (tool-result-success-p result)
                    (string= (tool-result-content result)
                             "fake bridge result")
                    (string= captured-server "bridge-1")
                    (string= (json-get captured-request "operation") "status")
                    (equal (pathname (json-get captured-request "workspace"))
                           (spacemacs-tool--workspace-root context)))
               "authorized Spacemacs commands use the discovered bridge and validated request"))))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)


;;;; -- Request Validation and Confinement --

(-> spacemacs-tool-test--requests () null)
(defun spacemacs-tool-test--requests ()
  "Test every operation request and the principal malformed variants."
  (let* ((base-configuration (test-configuration))
         (root (test-configuration-root base-configuration))
         (workspace (merge-pathnames "workspace/" root))
         (pathname (merge-pathnames "inside.txt" workspace)))
    (unwind-protect
        (progn
          (uiop:ensure-all-directories-exist (list workspace))
          (with-open-file (stream pathname
                                  :direction ':output
                                  :if-exists ':supersede
                                  :if-does-not-exist ':create)
            (write-line "inside" stream))
          (let* ((configuration
                   (configuration--clone
                    base-configuration
                    :working-directory workspace))
                 (context (spacemacs-tool-test--context configuration))
                 (cases
                   (list
                    (list "status"
                          (json-object "operation" "status")
                          '("operation" "workspace"))
                    (list "visit-file"
                          (json-object "operation" "visit-file"
                                       "path" "inside.txt"
                                       "line" 2
                                       "column" 3)
                          '("operation" "workspace" "path" "line" "column"))
                    (list "focus-buffer"
                          (json-object "operation" "focus-buffer"
                                       "buffer" "example.txt")
                          '("operation" "workspace" "buffer"))
                    (list "goto-line"
                          (json-object "operation" "goto-line" "line" 4)
                          '("operation" "workspace" "line" "column"))
                    (list "insert-text"
                          (json-object "operation" "insert-text" "text" "")
                          '("operation" "workspace" "text"))
                    (list "replace-region"
                          (json-object "operation" "replace-region" "text" "")
                          '("operation" "workspace" "text"))
                    (list "save-buffer"
                          (json-object "operation" "save-buffer")
                          '("operation" "workspace"))
                    (list "message"
                          (json-object "operation" "message" "text" "hello")
                          '("operation" "workspace" "text")))))
            (dolist (case cases)
              (destructuring-bind (operation arguments expected-keys) case
                (let ((request (spacemacs-tool--request context arguments)))
                  (test-assert
                   (and (string= (json-get request "operation") operation)
                        (= (hash-table-count request) (length expected-keys))
                        (every (lambda (key)
                                 (nth-value 1 (gethash key request)))
                               expected-keys))
                   (format nil "~A produces only its closed request fields"
                           operation)))))
            (let ((visit
                    (spacemacs-tool--request
                     context
                     (json-object "operation" "visit-file"
                                  "path" "inside.txt")))
                  (goto
                    (spacemacs-tool--request
                     context
                     (json-object "operation" "goto-line" "line" 9))))
              (test-assert
               (and (equal (pathname (json-get visit "path"))
                           (truename pathname))
                    (= (json-get visit "line") 1)
                    (= (json-get visit "column") 0)
                    (= (json-get goto "line") 9)
                    (= (json-get goto "column") 0))
               "file and point requests apply canonical paths and bounded defaults"))
            (dolist
                (invalid
                 (list
                  (list "unsupported operation"
                        (lambda ()
                          (spacemacs-tool--request
                           context (json-object "operation" "eval"))))
                  (list "missing visit path"
                        (lambda ()
                          (spacemacs-tool--request
                           context (json-object "operation" "visit-file"))))
                  (list "empty buffer name"
                        (lambda ()
                          (spacemacs-tool--request
                           context
                           (json-object "operation" "focus-buffer"
                                        "buffer" ""))))
                  (list "missing destination line"
                        (lambda ()
                          (spacemacs-tool--request
                           context (json-object "operation" "goto-line"))))
                  (list "zero destination line"
                        (lambda ()
                          (spacemacs-tool--request
                           context
                           (json-object "operation" "goto-line" "line" 0))))
                  (list "missing inserted text"
                        (lambda ()
                          (spacemacs-tool--request
                           context (json-object "operation" "insert-text"))))
                  (list "oversized message"
                        (lambda ()
                          (spacemacs-tool--request
                           context
                           (json-object
                            "operation" "message"
                            "text"
                            (make-string
                             (1+ *spacemacs-tool-maximum-text-characters*)
                             :initial-element #\x)))))))
              (test-assert
               (spacemacs-tool-test--tool-error-p (second invalid))
               (format nil "spacemacs.command rejects ~A" (first invalid))))))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> spacemacs-tool-test--path-confinement () null)
(defun spacemacs-tool-test--path-confinement ()
  "Test workspace escapes and confined non-regular files."
  (let* ((base-configuration (test-configuration))
         (root (test-configuration-root base-configuration))
         (workspace (merge-pathnames "workspace/" root))
         (outside (merge-pathnames "outside.txt" root))
         (escape (merge-pathnames "escape.txt" workspace))
         (fifo (merge-pathnames "pipe" workspace)))
    (unwind-protect
        (progn
          (uiop:ensure-all-directories-exist (list workspace))
          (with-open-file (stream outside
                                  :direction ':output
                                  :if-exists ':supersede
                                  :if-does-not-exist ':create)
            (write-line "outside" stream))
          (sb-posix:symlink (namestring outside) (namestring escape))
          (sb-posix:mkfifo (namestring fifo) #o600)
          (let* ((configuration
                   (configuration--clone
                    base-configuration
                    :working-directory workspace))
                 (context (spacemacs-tool-test--context configuration)))
            (dolist (path (list (namestring outside) (namestring escape)))
              (test-assert
               (spacemacs-tool-test--tool-error-p
                (lambda ()
                  (spacemacs-tool--request
                   context
                   (json-object "operation" "visit-file" "path" path))))
               (format nil "Spacemacs rejects workspace escape ~A" path)))
            (test-assert
             (spacemacs-tool-test--tool-error-p
              (lambda ()
                (spacemacs-tool--request
                 context
                 (json-object "operation" "visit-file"
                              "path" (namestring fifo)))))
             "Spacemacs rejects a confined FIFO as a visit target")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)


;;;; -- Discovery and Wire Boundary --

(-> spacemacs-tool-test--discovery () null)
(defun spacemacs-tool-test--discovery ()
  "Test record validation, dead process filtering, sorting, and deduplication."
  (test-assert
   (spacemacs-tool--record-p
    '(:autolith-spacemacs :version 1 :pid 10
      :server-name "bridge" :created-at 100))
   "a complete bridge registry record validates")
  (dolist
      (record
       (list
        '(:autolith-spacemacs :version "1" :pid 10
          :server-name "bridge" :created-at 100)
        '(:autolith-spacemacs :version 1 :pid 10
          :server-name "bad/name" :created-at 100)
        '(:autolith-spacemacs :version 1 :pid 10 :server-name)
        (list ':autolith-spacemacs
              ':version 1
              ':pid (1+ *spacemacs-tool-maximum-pid*)
              ':server-name "bridge"
              ':created-at 100)))
    (test-assert (not (spacemacs-tool--record-p record))
                 "malformed bridge records fail closed without signaling"))
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (context (spacemacs-tool-test--context configuration)))
    (unwind-protect
        (progn
          (spacemacs-tool-test--write-record
           configuration "old.sexp"
           '(:autolith-spacemacs :version 1 :pid 10
             :server-name "duplicate" :created-at 100))
          (spacemacs-tool-test--write-record
           configuration "new.sexp"
           '(:autolith-spacemacs :version 1 :pid 11
             :server-name "duplicate" :created-at 300))
          (spacemacs-tool-test--write-record
           configuration "other.sexp"
           '(:autolith-spacemacs :version 1 :pid 20
             :server-name "other" :created-at 200))
          (spacemacs-tool-test--write-record
           configuration "dead.sexp"
           '(:autolith-spacemacs :version 1 :pid 30
             :server-name "dead" :created-at 400))
          (spacemacs-tool-test--write-record
           configuration "invalid.sexp"
           '(:autolith-spacemacs :version 2 :pid 40
             :server-name "invalid" :created-at 500))
          (test-call-with-function-replacements
           (list
            (list 'spacemacs-tool--pid-live-p
                  (lambda (pid)
                    (not (null (member pid '(10 11 20)))))))
           (lambda ()
             (let ((records (spacemacs-tool--records context)))
               (test-assert
                (and (equal (mapcar
                             (lambda (record)
                               (getf (rest record) :server-name))
                             records)
                            '("duplicate" "other"))
                     (= (getf (rest (first records)) :pid) 11))
                "discovery filters dead records, keeps newest order, and deduplicates server names")))))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> spacemacs-tool-test--preference-context () null)
(defun spacemacs-tool-test--preference-context ()
  "Test request-local editor preference only for a visible live bridge."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation
          (conversation-create configuration
                               :identifier
                               (format nil "spacemacs-context-~A"
                                       (make-identifier))))
         (tool-namespaces
          (vector
           (json-object
            "name" "spacemacs"
            "tools" (vector (json-object "name" "command")))))
         (request
          (make-instance 'request-context
                         :configuration configuration
                         :conversation conversation
                         :tool-namespaces tool-namespaces))
         (hidden-request
          (make-instance 'request-context
                         :configuration configuration
                         :conversation conversation
                         :tool-namespaces #()))
         (compaction-request
          (make-instance 'request-context
                         :configuration configuration
                         :conversation conversation
                         :tool-namespaces tool-namespaces
                         :compaction-p t)))
    (unwind-protect
        (progn
          (test-assert (spacemacs-tool--visible-p request)
                       "the exact spacemacs.command namespace is visible")
          (test-assert (not (spacemacs-tool--visible-p hidden-request))
                       "a request without spacemacs.command is not guided")
          (test-assert (null (spacemacs-command-preference-context request))
                       "no editor preference is emitted without a live bridge")
          (spacemacs-tool-test--write-record
           configuration "live.sexp"
           (list ':autolith-spacemacs
                 ':version *spacemacs-tool-registry-version*
                 ':pid (sb-posix:getpid)
                 ':server-name "context-bridge"
                 ':created-at (get-universal-time)))
          (let ((contribution
                  (spacemacs-command-preference-context request))
                (registration
                  (context--registration-find
                   "spacemacs-command-preference")))
            (test-assert
             (and contribution
                  (string=
                   (context-contribution-identifier contribution)
                   "spacemacs-command-preference")
                  (eq (context-contribution-class contribution) ':mandatory)
                  (eq (context-contribution-lifetime contribution)
                      ':while-relevant))
             "a visible live bridge activates mandatory editor-link preference")
            (test-assert
             (and registration
                  (eq (getf registration :source) ':built-in))
             "the editor preference contributor has built-in provenance"))
          (test-assert
           (null (spacemacs-command-preference-context hidden-request))
           "an unavailable editor tool never receives preference advice")
          (test-assert
           (null (spacemacs-command-preference-context compaction-request))
           "compaction requests omit editor preference advice")
          (test-call-with-function-replacements
           (list
            (list 'spacemacs-tool--records-for-configuration
                  (lambda (actual-configuration)
                    (declare (ignore actual-configuration))
                    (error "broken Spacemacs registry"))))
           (lambda ()
             (test-assert
              (null (spacemacs-command-preference-context request))
              "registry failures never break provider context assembly"))))
      (uiop:delete-directory-tree root
                                  :validate t
                                  :if-does-not-exist ':ignore)))
  nil)

(-> spacemacs-tool-test--server-selection () null)
(defun spacemacs-tool-test--server-selection ()
  "Test zero, one, multiple, explicit, and invalid server selection."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (context (spacemacs-tool-test--context configuration))
         (one '(:autolith-spacemacs :version 1 :pid 10
                :server-name "one" :created-at 100))
         (two '(:autolith-spacemacs :version 1 :pid 20
                :server-name "two" :created-at 200)))
    (unwind-protect
        (progn
          (test-call-with-function-replacements
           (list (list 'spacemacs-tool--records
                       (lambda (actual-context)
                         (declare (ignore actual-context))
                         nil)))
           (lambda ()
             (test-assert
              (spacemacs-tool-test--tool-error-p
               (lambda ()
                 (spacemacs-tool--server context (json-object))))
              "zero bridge records require an enabled live bridge")))
          (test-call-with-function-replacements
           (list (list 'spacemacs-tool--records
                       (lambda (actual-context)
                         (declare (ignore actual-context))
                         (list one))))
           (lambda ()
             (test-assert
              (string= (spacemacs-tool--server context (json-object)) "one")
              "one bridge record is selected automatically")))
          (test-call-with-function-replacements
           (list (list 'spacemacs-tool--records
                       (lambda (actual-context)
                         (declare (ignore actual-context))
                         (list two one))))
           (lambda ()
             (test-assert
              (spacemacs-tool-test--tool-error-p
               (lambda ()
                 (spacemacs-tool--server context (json-object))))
              "multiple bridges require explicit selection")
             (test-assert
              (string= (spacemacs-tool--server
                        context (json-object "server" "two"))
                       "two")
              "an explicit published bridge resolves ambiguity")
             (test-assert
              (spacemacs-tool-test--tool-error-p
               (lambda ()
                 (spacemacs-tool--server
                  context (json-object "server" "missing"))))
              "an explicit unpublished bridge is rejected")
             (test-assert
              (spacemacs-tool-test--tool-error-p
               (lambda ()
                 (spacemacs-tool--server
                  context (json-object "server" "bad/name"))))
              "an invalid explicit server name is rejected"))))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> spacemacs-tool-test--wire-boundary () null)
(defun spacemacs-tool-test--wire-boundary ()
  "Test the fixed dispatcher expression and safe emacsclient result reader."
  (let* ((request
           (json-object "operation" "message"
                        "workspace" "/tmp/workspace/"
                        "text" "quote: \" newline:\n snowman: ☃"))
         (expression (spacemacs-tool--emacsclient-expression request))
         (prefix "(autolith-editor-dispatch-json \"")
         (suffix "\")"))
    (test-assert
     (and (uiop:string-prefix-p prefix expression)
           (uiop:string-suffix-p expression suffix))
     "emacsclient receives only the fixed dispatcher form")
    (let* ((encoded
             (subseq expression
                     (length prefix)
                     (- (length expression) (length suffix))))
           (decoded
             (sb-ext:octets-to-string
              (base64-string-to-usb8-array encoded)
              :external-format ':utf-8))
           (round-trip (json-decode decoded)))
      (test-assert
       (and (every (lambda (character)
                     (or (alphanumericp character)
                         (find character "+/=")))
                   encoded)
            (string= (json-get round-trip "operation") "message")
            (string= (json-get round-trip "text")
                     (json-get request "text")))
       "Base64 JSON preserves request text without Lisp or shell interpolation")))
  (let* ((json (json-encode (json-object "ok" t)))
         (printed (write-to-string json)))
    (test-assert
     (string= (spacemacs-tool--read-emacsclient-value printed) json)
     "the emacsclient reader accepts one safely printed string value"))
  (dolist (source '("" "NIL" "\"ok\" trailing" "#.(error \"unsafe\")"
                    "#100000000A()" "\"{\"ok\":true}\""))
    (test-assert
     (spacemacs-tool-test--tool-error-p
      (lambda ()
        (spacemacs-tool--read-emacsclient-value source)))
     (format nil "the emacsclient reader rejects malformed response ~S" source)))
  (test-assert
   (spacemacs-tool-test--tool-error-p
    (lambda ()
      (spacemacs-tool--emacsclient-expression
       (json-object "operation" "message"
                    "workspace" "/tmp/workspace/"
                    "text" (make-string 100000 :initial-element #\x)))))
   "emacsclient requests are bounded by their encoded UTF-8 argument size")
  nil)

(-> spacemacs-tool-test--invoke () null)
(defun spacemacs-tool-test--invoke ()
  "Test bounded emacsclient outcomes without starting an editor process."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (context (spacemacs-tool-test--context configuration))
         (request
           (json-object "operation" "status"
                        "workspace" (namestring root))))
    (labels
        ((result (&key
                    (exit-code 0)
                    (output "")
                    (output-truncated-p nil)
                    (error-output "")
                    (error-output-truncated-p nil)
                    (timed-out-p nil))
           (make-instance 'cl-exec-sandbox:sandbox-result
                          :exit-code exit-code
                          :output output
                          :output-truncated-p output-truncated-p
                          :error-output error-output
                          :error-output-truncated-p error-output-truncated-p
                          :timed-out-p timed-out-p
                          :real-seconds 0.0d0))
         (invoke-with (sandbox-result)
           (let (captured-arguments)
             (values
              (test-call-with-function-replacements
               (list
                (list 'run-sandboxed
                      (lambda (&rest arguments)
                        (setf captured-arguments arguments)
                        sandbox-result)))
               (lambda ()
                 (spacemacs-tool--invoke context "bridge" request)))
              captured-arguments))))
      (unwind-protect
          (progn
            (multiple-value-bind (tool-result arguments)
                (invoke-with
                 (result
                  :output
                  (write-to-string
                   (json-encode
                    (json-object "ok" t "operation" "status")))
                  :error-output "harmless warning"))
              (test-assert
               (and (tool-result-success-p tool-result)
                    (member "--alternate-editor=false" (second arguments)
                            :test #'string=)
                    (member :merge-output-p arguments)
                    (null (getf (cddr arguments) :merge-output-p)))
               "successful protocol output disables fallback and ignores separate warnings"))
            (let ((tool-result
                    (invoke-with (result :exit-code 137 :timed-out-p t))))
              (test-assert
               (and (not (tool-result-success-p tool-result))
                    (search "timed out" (tool-result-content tool-result)
                            :test #'char-equal))
               "emacsclient timeouts return a bounded tool failure"))
            (let ((tool-result
                    (invoke-with
                     (result :exit-code 2
                             :error-output "socket unavailable"))))
              (test-assert
               (and (not (tool-result-success-p tool-result))
                    (search "exited 2" (tool-result-content tool-result))
                    (search "socket unavailable"
                            (tool-result-content tool-result)))
               "emacsclient exit failures include separate standard error"))
            (let ((tool-result
                    (invoke-with
                     (result :output "partial"
                             :output-truncated-p t))))
              (test-assert
               (and (not (tool-result-success-p tool-result))
                    (search "output limit" (tool-result-content tool-result)))
               "truncated bridge output fails before protocol parsing"))
            (let ((tool-result
                    (invoke-with
                     (result :output (write-to-string "not-json")))))
              (test-assert
               (and (not (tool-result-success-p tool-result))
                    (search "malformed response"
                            (tool-result-content tool-result)))
               "malformed bridge JSON returns a tool failure"))
            (let ((tool-result
                    (invoke-with
                     (result
                      :output
                      (write-to-string
                       (json-encode
                        (json-object "ok" false
                                     "error" "bridge disabled")))))))
              (test-assert
               (and (not (tool-result-success-p tool-result))
                    (string= "bridge disabled"
                             (tool-result-content tool-result)))
               "bridge rejections preserve their bounded error message")))
        (uiop:delete-directory-tree root
                                    :validate t
                                    :if-does-not-exist ':ignore))))
  nil)


;;;; -- Entry Point --

(-> test-spacemacs-tool () null)
(defun test-spacemacs-tool ()
  "Test the scoped Spacemacs command tool end to end without a live editor."
  (spacemacs-tool-test--registry)
  (spacemacs-tool-test--authorization)
  (spacemacs-tool-test--requests)
  (spacemacs-tool-test--path-confinement)
  (spacemacs-tool-test--discovery)
  (spacemacs-tool-test--preference-context)
  (spacemacs-tool-test--server-selection)
  (spacemacs-tool-test--invoke)
  (spacemacs-tool-test--wire-boundary)
  nil)
