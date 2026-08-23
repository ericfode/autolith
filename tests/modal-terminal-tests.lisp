(in-package #:autolith)

;;;; -- Managed Modal Terminal Tests --

(-> test-managed-modal-terminal () null)
(defun test-managed-modal-terminal ()
  "Test managed Modal wire contracts, bounds, timeout cancellation, and isolation."
  (let* ((base (test-configuration))
         (configuration (configuration-with-terminal-route base ':modal))
         (requests nil)
         (exec-id nil)
         (*managed-modal-credential-function*
           (lambda (backend)
             (declare (ignore backend))
             "test-nous-token"))
         (*managed-modal-poll-interval-seconds* 0)
         (*managed-modal-http-request-function*
           (lambda (method url headers content connect-timeout read-timeout)
             (declare (ignore connect-timeout read-timeout))
             (let* ((path (quri:uri-path (quri:uri url)))
                    (payload (and content (plusp (length content))
                                  (json-decode content))))
               (push (list method path headers payload) requests)
               (cond
                 ((string= path "/v1/sandboxes")
                  (values "{\"id\":\"sandbox-1\"}" 200 nil))
                 ((and (eq method ':post)
                       (string= path "/v1/sandboxes/sandbox-1/execs"))
                  (setf exec-id (json-get payload "execId"))
                  (values (json-encode
                           (json-object "execId" exec-id "status" "running"))
                          200 nil))
                 ((and (eq method ':get)
                       (string= path
                                (format nil
                                        "/v1/sandboxes/sandbox-1/execs/~A"
                                        exec-id)))
                  (values
                   (json-encode
                    (json-object "status" "completed"
                                 "output" "remote-output"
                                 "returncode" 7))
                   200 nil))
                 ((and (>= (length path) 10)
                       (string= path "/terminate"
                                :start1 (- (length path) 10)))
                  (values "{}" 200 nil))
                 (t
                  (error "Unexpected managed Modal request ~S ~A" method path)))))))
    (let* ((backend
             (terminal-execution-backend-create
              configuration "autolith-test-session"))
           (result
             (terminal-execution-backend-run
              backend "printf remote" #P"/root/" ':sandboxed 42 65536)))
      (test-assert (tool-result-success-p result)
                   "managed Modal returns completed remote commands")
      (test-assert (search "exit 7" (tool-result-content result))
                   "managed Modal preserves remote return codes")
      (test-assert (search "remote-output" (tool-result-content result))
                   "managed Modal returns remote output")
      (let* ((ordered (nreverse requests))
             (create (first ordered))
             (start (second ordered))
             (create-payload (fourth create))
             (start-payload (fourth start)))
        (test-assert
         (and (eq (first create) ':post)
              (string= (second create) "/v1/sandboxes")
              (string= (json-get create-payload "image")
                       "nikolaik/python-nodejs:python3.11-nodejs20")
              (string= (json-get create-payload "cwd") "/root")
              (= (json-get create-payload "cpu") 1)
              (= (json-get create-payload "memoryMiB") 5120)
              (= (json-get create-payload "timeoutMs") 3600000)
              (= (json-get create-payload "idleTimeoutMs") 300000)
              (eq (json-get create-payload "persistentFilesystem") t)
              (string= (json-get create-payload "logicalKey")
                       "autolith-test-session"))
         "managed Modal creates the exact persistent sandbox contract")
        (test-assert
         (and (string= (json-get start-payload "command") "printf remote")
              (string= (json-get start-payload "cwd") "/root/")
              (= (json-get start-payload "timeoutMs") 42000))
         "managed Modal starts execs with command, cwd, and timeout")
        (test-assert
         (and (assoc "Authorization" (third create) :test #'string=)
              (string=
               (rest (assoc "Authorization" (third create) :test #'string=))
               "Bearer test-nous-token")
              (assoc "x-idempotency-key" (third create) :test #'string=))
         "managed Modal uses independent Nous bearer and idempotency headers"))
      (terminal-execution-backend-close backend)
      (test-assert
       (some (lambda (request)
               (and (eq (first request) ':post)
                    (string=
                     (second request)
                     "/v1/sandboxes/sandbox-1/terminate")
                    (eq (json-get (fourth request) "snapshotBeforeTerminate") t)))
             requests)
       "managed Modal termination snapshots the persistent filesystem"))
    (let ((bounded
            (managed-modal--result
             (json-object "output" "123456789" "returncode" 0)
             5)))
      (test-assert
       (and (search "12345" (tool-result-content bounded))
            (not (search "6789" (tool-result-content bounded)))
            (search "truncated after 5 characters"
                    (tool-result-content bounded)))
       "managed Modal bounds returned command output"))
    (let ((time 0)
          (timeout-requests nil))
      (let ((*managed-modal-time-function*
              (lambda () (prog1 time (incf time 100))))
            (*managed-modal-http-request-function*
              (lambda (method url headers content connect-timeout read-timeout)
                (declare (ignore headers connect-timeout read-timeout))
                (let ((path (quri:uri-path (quri:uri url))))
                  (push (list method path content) timeout-requests)
                  (cond
                    ((string= path "/v1/sandboxes")
                     (values "{\"id\":\"sandbox-timeout\"}" 200 nil))
                    ((string= path "/v1/sandboxes/sandbox-timeout/execs")
                     (let ((id (json-get (json-decode content) "execId")))
                       (values
                        (json-encode
                         (json-object "execId" id "status" "running"))
                        200 nil)))
                    ((search "/cancel" path)
                     (values "{}" 200 nil))
                    (t
                     (error "Unexpected timeout request ~S ~A" method path)))))))
        (let* ((backend
                 (terminal-execution-backend-create
                  configuration "timeout-session"))
               (result
                 (terminal-execution-backend-run
                  backend "sleep forever" #P"/root/" ':sandboxed 1 100)))
          (test-assert
           (and (not (tool-result-success-p result))
                (search "timed out after 1s" (tool-result-content result))
                (some (lambda (request)
                        (search "/cancel" (second request)))
                      timeout-requests))
           "managed Modal cancels execs after the bounded client timeout"))))
    (let* ((request-count 0)
           (*managed-modal-http-request-function*
             (lambda (&rest arguments)
               (declare (ignore arguments))
               (incf request-count)
               (error "Denied commands must not reach Modal.")))
           (registry (make-default-tool-registry))
           (conversation (conversation-create configuration
                                              :identifier "modal-denied"))
           (backend (terminal-execution-backend-create
                     configuration "denied-session"))
           (result
             (tool-registry-execute-call
              registry
              (json-object "namespace" "shell"
                           "name" "run"
                           "arguments"
                           (json-encode
                            (json-object "command" "touch /root/denied")))
              (make-instance 'tool-context
                             :configuration configuration
                             :worker nil
                             :conversation conversation
                             :terminal-backend backend
                             :command-authorization-function
                             (lambda (command directory)
                               (declare (ignore command directory))
                               ':deny)))))
      (test-assert
       (and (not (tool-result-success-p result))
            (zerop request-count))
       "shell.run denies commands before managed Modal backend dispatch"))
    (let* ((left (agent-create :configuration configuration
                               :terminal-logical-key "child-left"))
           (right (agent-create :configuration configuration
                                :terminal-logical-key "child-right")))
      (test-assert
       (and (not (eq (agent-terminal-execution-backend left)
                     (agent-terminal-execution-backend right)))
            (string= (managed-modal-terminal-logical-key
                      (agent-terminal-backend left))
                     "child-left")
            (string= (managed-modal-terminal-logical-key
                      (agent-terminal-backend right))
                     "child-right"))
       "task agents inherit the Modal route with isolated sandbox identities"))
    (let* ((agent (agent-create :configuration configuration
                                :terminal-logical-key "status-session"))
           (application (make-instance 'application
                                       :configuration configuration
                                       :agent agent))
           (text (terminal-ui--raw-spans-text
                  (application--terminal-backend-fields application))))
      (test-assert
       (and (search "modal state" text)
            (search "not-created" text)
            (search "status-session" text))
       "status exposes the Modal identity and lazy sandbox state"))
    (let* ((pointer-root
             (merge-pathnames "recovery-session-pointers/"
                              (configuration-state-root configuration)))
           (pointer (merge-pathnames "launcher-a.sexp" pointer-root))
           (previous (uiop:getenv "AUTOLITH_RECOVERY_SESSION_POINTER")))
      (unwind-protect
           (progn
             (ensure-directories-exist pointer)
             (sb-posix:setenv "AUTOLITH_RECOVERY_SESSION_POINTER"
                             (namestring pointer) 1)
             (test-assert
              (string= (application--terminal-logical-key configuration)
                       (application--terminal-logical-key configuration))
              "launcher recovery derives the same opaque Modal logical identity"))
        (if previous
            (sb-posix:setenv "AUTOLITH_RECOVERY_SESSION_POINTER" previous 1)
            (sb-posix:unsetenv "AUTOLITH_RECOVERY_SESSION_POINTER"))))
  nil))
