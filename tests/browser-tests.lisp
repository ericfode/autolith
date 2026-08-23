(in-package #:autolith)

;;;; -- Nous Managed Browser Tests --

(defclass test-browser-cdp-transport (browser-cdp-transport)
  ((connection
    :initarg :connection
    :reader test-browser-cdp-transport-connection)
   (messages
    :initform nil
    :accessor test-browser-cdp-transport-messages)
   (closed-p
    :initform nil
    :accessor test-browser-cdp-transport-closed-p)
   (request-event-function
    :initarg :request-event-function
    :initform nil
    :reader test-browser-cdp-transport-request-event-function))
  (:documentation "A synchronous mocked CDP transport recording browser commands."))

(-> test-browser--cdp-result (string json-object) json-object)
(defun test-browser--cdp-result (method parameters)
  "Return the mocked CDP result for METHOD and PARAMETERS."
  (declare (ignore parameters))
  (cond
    ((string= method "Target.getTargets")
     (json-object
      "targetInfos"
      (vector (json-object "targetId" "page-1" "type" "page"))))
    ((string= method "Target.attachToTarget")
     (json-object "sessionId" "cdp-session-1"))
    ((string= method "Runtime.evaluate")
     (json-object "result" (json-object "value" "complete")))
    ((string= method "Accessibility.getFullAXTree")
     (json-object
      "nodes"
      (vector
       (json-object "ignored" nil
                    "role" (json-object "value" "button")
                    "name" (json-object "value" "Continue")
                    "backendDOMNodeId" 41)
       (json-object "ignored" nil
                    "role" (json-object "value" "textbox")
                    "name" (json-object "value" "Password")
                    "backendDOMNodeId" 42))))
    ((string= method "DOM.getBoxModel")
     (json-object "model"
                  (json-object "content" #(0 0 100 0 100 40 0 40))))
    ((string= method "Page.captureScreenshot")
     (json-object
      "data"
      "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mNk+A8AAQUBAScY42YAAAAASUVORK5CYII="))
    (t
     (json-object))))

(defmethod browser-cdp-transport-send
    ((transport test-browser-cdp-transport) (message string))
  "Record MESSAGE and immediately return its correlated mocked response."
  (let* ((document (json-decode message))
         (method (json-get document "method"))
         (identifier (json-get document "id"))
         (connection (test-browser-cdp-transport-connection transport)))
    (push document (test-browser-cdp-transport-messages transport))
    (browser-cdp-handle-message
     connection
     (json-encode
      (json-object "id" identifier
                   "result"
                   (test-browser--cdp-result method (json-get document "params")))))
    (let ((event-function
            (test-browser-cdp-transport-request-event-function transport)))
      (when event-function
        (funcall event-function connection document)))
    (when (string= method "Page.navigate")
      (browser-cdp-handle-message
       connection
       (json-encode
        (let ((event (json-object "method" "Page.loadEventFired"
                                  "params" (json-object))))
          (when (json-get document "sessionId")
            (setf (gethash "sessionId" event)
                  (json-get document "sessionId")))
          event)))))
  nil)

(defmethod browser-cdp-transport-close ((transport test-browser-cdp-transport))
  "Record closure of mocked TRANSPORT."
  (setf (test-browser-cdp-transport-closed-p transport) t)
  nil)

(-> test-browser--emit-request-paused
    (browser-cdp-connection json-object &key (:url string)
                            (:resource-type string))
    null)
(defun test-browser--emit-request-paused
    (connection command &key url resource-type)
  "Emit one flattened Fetch.requestPaused event for COMMAND."
  (browser-cdp-handle-message
   connection
   (json-encode
    (json-object
     "method" "Fetch.requestPaused"
     "sessionId" (json-get command "sessionId")
     "params"
     (json-object "requestId" (make-identifier)
                  "request" (json-object "url" url)
                  "resourceType" resource-type))))
  nil)

(defclass test-browser-delayed-transport (browser-cdp-transport)
  ((messages
    :initform nil
    :accessor test-browser-delayed-transport-messages)
   (lock
    :initform (make-lock "test delayed browser transport")
    :reader test-browser-delayed-transport-lock))
  (:documentation "A mocked transport whose responses are delivered explicitly."))

(defmethod browser-cdp-transport-send
    ((transport test-browser-delayed-transport) (message string))
  "Record MESSAGE for explicit out-of-order response delivery."
  (with-lock-held ((test-browser-delayed-transport-lock transport))
    (push (json-decode message) (test-browser-delayed-transport-messages transport)))
  nil)

(defmethod browser-cdp-transport-close ((transport test-browser-delayed-transport))
  "Close a delayed mock without additional state."
  (declare (ignore transport))
  nil)

(-> test-browser--wait-message-count
    (test-browser-delayed-transport integer) null)
(defun test-browser--wait-message-count (transport count)
  "Wait briefly for TRANSPORT to record COUNT messages."
  (loop repeat 100
        when (with-lock-held ((test-browser-delayed-transport-lock transport))
               (>= (length (test-browser-delayed-transport-messages transport)) count))
          do (return)
        do (sleep 0.01))
  nil)

(-> test-browser--request-header (string list) (option string))
(defun test-browser--request-header (name headers)
  "Return case-insensitive header NAME from HEADERS."
  (rest (assoc name headers :test #'string-equal)))

(-> test-nous-managed-browser () null)
(defun test-nous-managed-browser ()
  "Test managed lifecycle, CDP correlation/actions, routing, permissions, and isolation."
  (let* ((base (test-configuration))
         (configuration (configuration-with-browser-route base ':nous))
         (requests nil)
         (create-attempt 0)
         (*nous-browser-credential-function*
           (lambda (configuration)
             (declare (ignore configuration))
             "test-nous-browser-token"))
         (*nous-browser-http-request-function*
           (lambda (method url headers content connect-timeout read-timeout)
             (declare (ignore connect-timeout read-timeout))
             (let ((path (quri:uri-path (quri:uri url)))
                   (payload (json-decode content)))
               (push (list method path headers payload) requests)
               (cond
                 ((and (eq method ':post) (string= path "/browsers"))
                  (incf create-attempt)
                  (if (= create-attempt 1)
                      (values "{\"error\":{\"message\":\"temporary\"}}" 503 nil)
                      (values
                       "{\"id\":\"browser-1\",\"cdpUrl\":\"wss://cdp.example/session\"}"
                       200 nil)))
                 ((and (eq method ':patch)
                       (string= path "/browsers/browser-1"))
                  (values "{}" 204 nil))
                 (t
                  (error "Unexpected browser request ~S ~A" method path)))))))
    (let ((runtime (nous-browser-runtime-create configuration "primary-browser")))
      (handler-case
          (progn
            (nous-browser-runtime--create-session runtime)
            (error "Expected first create to fail."))
        (nous-browser-error (condition)
          (test-assert (= (nous-browser-error-status condition) 503)
                       "managed browser surfaces retryable create failures")))
      (let ((retained-key (nous-browser-runtime-create-key runtime)))
        (nous-browser-runtime--create-session runtime)
        (let* ((ordered (nreverse requests))
               (first-create (first ordered))
               (second-create (second ordered)))
          (test-assert
           (and (string= retained-key (nous-browser-runtime-create-key runtime))
                (string=
                 (test-browser--request-header "X-Idempotency-Key"
                                               (third first-create))
                 (test-browser--request-header "X-Idempotency-Key"
                                               (third second-create)))
                (string=
                 (test-browser--request-header "X-Browser-Use-API-Key"
                                               (third second-create))
                 "test-nous-browser-token")
                (= (json-get (fourth second-create) "timeout") 5)
                (string= (json-get (fourth second-create) "proxyCountryCode") "us"))
           "managed browser preserves the create key and exact gateway contract")))
      (let ((*nous-browser-cdp-connect-function*
              (lambda (connection url)
                (declare (ignore url))
                (make-instance 'test-browser-cdp-transport
                               :connection connection))))
        (test-assert
         (search "Navigated"
                 (nous-browser-runtime-navigate
                  runtime "https://93.184.216.34/"))
         "managed browser navigates through CDP without fallback")
        (let ((snapshot (nous-browser-runtime-snapshot runtime)))
          (test-assert
           (and (search "[e1] button" snapshot)
                (search "[e2] textbox" snapshot))
           "accessibility snapshots expose stable element refs"))
        (test-assert (search "Clicked e1" (nous-browser-runtime-click runtime "e1"))
                     "browser.click dispatches CDP mouse events")
        (nous-browser-runtime-snapshot runtime)
        (test-assert
         (string= (nous-browser-runtime-type runtime "e2" "top-secret")
                  "Typed text into e2.")
         "browser.type does not echo sensitive text")
        (test-assert (search "Pressed Enter" (nous-browser-runtime-press runtime "Enter"))
                     "browser.press dispatches bounded key events")
        (test-assert (search "Scrolled 500" (nous-browser-runtime-scroll runtime 500))
                     "browser.scroll executes through Runtime.evaluate")
        (test-assert (search "Navigated back" (nous-browser-runtime-back runtime))
                     "browser.back executes and waits boundedly")
        (let ((attachment
                (nous-browser-runtime-screenshot
                 runtime
                 (conversation-create configuration :identifier "browser-image"))))
          (test-assert
           (and (typep attachment 'image-attachment)
                (probe-file (image-attachment-pathname attachment)))
           "browser.screenshot returns a native image attachment"))
        (let* ((transport
                 (browser-cdp-connection-transport
                  (nous-browser-runtime-connection runtime)))
               (documents
                 (reverse
                  (test-browser-cdp-transport-messages transport)))
               (methods
                 (mapcar (lambda (document) (json-get document "method"))
                         documents)))
          (test-assert
           (every (lambda (method) (member method methods :test #'string=))
                  '("Fetch.enable" "Page.navigate" "Accessibility.getFullAXTree"
                    "Input.dispatchMouseEvent" "Input.insertText"
                    "Input.dispatchKeyEvent" "Page.captureScreenshot"))
           "practical browser actions issue the expected CDP commands")
          (test-assert
           (and (< (position "Fetch.enable" methods :test #'string=)
                   (position "Page.enable" methods :test #'string=))
                (< (position "Fetch.enable" methods :test #'string=)
                   (position "Page.navigate" methods :test #'string=)))
           "Fetch interception is enabled before the page is used")))
      (handler-case
          (progn
            (nous-browser-runtime-click runtime "stale-ref")
            (error "Expected stale browser reference rejection."))
        (nous-browser-stale-reference ()
          nil))
      (nous-browser-runtime-close runtime)
      (test-assert
       (some (lambda (request)
               (and (eq (first request) ':patch)
                    (string= (second request) "/browsers/browser-1")
                    (string= (json-get (fourth request) "action") "stop")))
             requests)
       "managed browser cleanup sends the exact stop PATCH"))
    (labels ((run-request-policy-case (case)
               (let* ((transport nil)
                      (request-emitted-p nil)
                      (blocked-condition nil)
                      (*nous-browser-cdp-connect-function*
                        (lambda (connection url)
                          (declare (ignore url))
                          (setf transport
                                (make-instance
                                 'test-browser-cdp-transport
                                 :connection connection
                                 :request-event-function
                                 (lambda (connection command)
                                   (when (and (not request-emitted-p)
                                              (funcall (getf case :trigger)
                                                       command))
                                     (setf request-emitted-p t)
                                     (test-browser--emit-request-paused
                                      connection command
                                      :url (getf case :url)
                                      :resource-type
                                      (getf case :resource-type)))))))))
                 (let ((runtime
                         (nous-browser-runtime-create
                          configuration
                          (format nil "request-policy-~A" (getf case :name)))))
                   (unwind-protect
                        (if (getf case :blocked-p)
                            (handler-case
                                (progn
                                  (funcall (getf case :action) runtime)
                                  (error "Expected browser request blocking for ~A."
                                         (getf case :name)))
                              (nous-browser-request-blocked (condition)
                                (setf blocked-condition condition)))
                            (funcall (getf case :action) runtime))
                     (nous-browser-runtime-close runtime)))
                 (test-assert request-emitted-p
                              "request policy test emitted its paused request")
                 (let ((commands
                         (test-browser-cdp-transport-messages transport)))
                   (if (getf case :blocked-p)
                       (test-assert
                        (and blocked-condition
                             (string= (nous-browser-request-blocked-url
                                       blocked-condition)
                                      (getf case :url))
                             (string= (or (nous-browser-request-blocked-resource-type
                                           blocked-condition)
                                          "")
                                      (getf case :resource-type))
                             (some
                              (lambda (command)
                                (and (string= (or (json-get command "method") "")
                                              "Fetch.failRequest")
                                     (string= (or (json-get
                                                   (json-get command "params")
                                                   "errorReason")
                                                  "")
                                              "BlockedByClient")))
                              commands))
                        (format nil "~A requests are failed with a typed policy condition"
                                (getf case :name)))
                       (test-assert
                        (some
                         (lambda (command)
                           (string= (or (json-get command "method") "")
                                    "Fetch.continueRequest"))
                         commands)
                        "public requests continue through Fetch interception"))))))
      (dolist
          (case
           (list
            (list
             :name "redirect"
             :trigger
             (lambda (command)
               (string= (or (json-get command "method") "") "Page.navigate"))
             :url "http://127.0.0.1/redirected"
             :resource-type "Document"
             :blocked-p t
             :action
             (lambda (runtime)
               (nous-browser-runtime-navigate runtime "https://93.184.216.34/")))
            (list
             :name "clicked-link"
             :trigger
             (lambda (command)
               (and (string= (or (json-get command "method") "")
                             "Input.dispatchMouseEvent")
                    (string= (or (json-get (json-get command "params") "type") "")
                             "mouseReleased")))
             :url "http://[::1]/clicked"
             :resource-type "Document"
             :blocked-p t
             :action
             (lambda (runtime)
               (nous-browser-runtime-snapshot runtime)
               (nous-browser-runtime-click runtime "e1")))
            (list
             :name "history"
             :trigger
             (lambda (command)
               (and (string= (or (json-get command "method") "")
                             "Runtime.evaluate")
                    (string= (or (json-get (json-get command "params")
                                           "expression")
                                 "")
                             "history.back()")))
             :url "http://169.254.169.254/latest/meta-data/"
             :resource-type "Document"
             :blocked-p t
             :action #'nous-browser-runtime-back)
            (list
             :name "subresource"
             :trigger
             (lambda (command)
               (string= (or (json-get command "method") "") "Page.navigate"))
             :url "http://10.0.0.1/private.js"
             :resource-type "Script"
             :blocked-p t
             :action
             (lambda (runtime)
               (nous-browser-runtime-navigate runtime "https://93.184.216.34/")))
            (list
             :name "public"
             :trigger
             (lambda (command)
               (string= (or (json-get command "method") "") "Page.navigate"))
             :url "https://93.184.216.34/resource.js"
             :resource-type "Script"
             :blocked-p nil
             :action
             (lambda (runtime)
               (nous-browser-runtime-navigate runtime "https://93.184.216.34/")))))
        (run-request-policy-case case)))
    (let* ((connection (make-instance 'browser-cdp-connection))
           (transport (make-instance 'test-browser-delayed-transport))
           (left nil)
           (right nil))
      (setf (browser-cdp-connection-transport connection) transport)
      (let ((left-thread
              (make-thread
               (lambda ()
                 (setf left
                       (json-get
                        (browser-cdp-command connection "Test.left" (json-object))
                        "value")))))
            (right-thread
              (make-thread
               (lambda ()
                 (setf right
                       (json-get
                        (browser-cdp-command connection "Test.right" (json-object))
                        "value"))))))
        (test-browser--wait-message-count transport 2)
        (let ((messages
                (with-lock-held ((test-browser-delayed-transport-lock transport))
                  (copy-list (test-browser-delayed-transport-messages transport)))))
          (dolist (message messages)
            (browser-cdp-handle-message
             connection
             (json-encode
              (json-object
               "id" (json-get message "id")
               "result"
               (json-object "value"
                            (if (string= (json-get message "method") "Test.left")
                                "left-result"
                                "right-result")))))))
        (join-thread left-thread)
        (join-thread right-thread)
        (test-assert
         (and (string= left "left-result") (string= right "right-result"))
         "CDP request IDs correlate out-of-order concurrent responses")))
      (let* ((connection (make-instance 'browser-cdp-connection))
             (transport (make-instance 'test-browser-delayed-transport))
             (runtime (nous-browser-runtime-create configuration "close-broadcast"))
             (left-stage nil)
             (right-stage nil))
        (setf (browser-cdp-connection-transport connection) transport
              (nous-browser-runtime-connection runtime) connection
              (nous-browser-runtime-target-session-id runtime) "cdp-session-1")
        (nous-browser-runtime--start-request-interception runtime)
        (let ((left-thread
                (make-thread
                 (lambda ()
                   (handler-case
                       (browser-cdp-command
                        connection "Test.wait-left" (json-object) :timeout 2)
                     (nous-browser-error (condition)
                       (setf left-stage (nous-browser-error-stage condition)))))))
              (right-thread
                (make-thread
                 (lambda ()
                   (handler-case
                       (browser-cdp-command
                        connection "Test.wait-right" (json-object) :timeout 2)
                     (nous-browser-error (condition)
                       (setf right-stage (nous-browser-error-stage condition))))))))
          (unwind-protect
               (progn
                 (test-browser--wait-message-count transport 2)
                 (browser-cdp-handle-message
                  connection
                  (json-encode
                   (json-object
                    "method" "Fetch.requestPaused"
                    "sessionId" "cdp-session-1"
                    "params"
                    (json-object
                     "requestId" "close-request"
                     "request" (json-object "url" "https://93.184.216.34/")
                     "resourceType" "Document"))))
                 (test-browser--wait-message-count transport 3)
                 (browser-cdp-close connection)
                 (join-thread left-thread)
                 (join-thread right-thread)
                 (nous-browser-runtime--stop-request-interception runtime)
                 (let ((request-failure
                         (nous-browser-runtime-request-failure runtime)))
                   (test-assert
                    (and (eq left-stage ':connection)
                         (eq right-stage ':connection)
                         (typep request-failure 'nous-browser-error)
                         (eq (nous-browser-error-stage request-failure) ':connection))
                    "CDP close wakes concurrent commands and the active Fetch worker")))
            (ignore-errors (browser-cdp-close connection))
            (ignore-errors
              (nous-browser-runtime--stop-request-interception runtime)))))
    (let ((connection (make-instance 'browser-cdp-connection)))
      (browser-cdp-handle-message
       connection
       (make-string (1+ *nous-browser-maximum-cdp-message-bytes*)
                    :initial-element #\x))
      (test-assert (browser-cdp-connection-closed-p connection)
                   "oversized CDP messages close the connection"))
    (let ((disabled (make-default-tool-registry :configuration base))
          (enabled (make-default-tool-registry :configuration configuration)))
      (test-assert
       (and (null (tool-registry-find disabled "browser" "navigate"))
            (tool-registry-find enabled "browser" "navigate"))
       "browser schemas are advertised only for enabled sessions"))
      (let* ((registry (make-default-tool-registry :configuration configuration))
             (tool (tool-registry-find registry "browser" "type"))
             (arguments (json-object "ref" "e2" "text" "permission-secret"))
             (presentation
               (tool-authorization-presentation-arguments tool arguments))
             (entry
               (application--tool-authorization-request-entry tool arguments)))
        (multiple-value-bind (text text-present-p) (gethash "text" presentation)
          (declare (ignore text))
          (test-assert
           (and (not text-present-p)
                (string= (json-get presentation "ref") "e2")
                (= (json-get presentation "text_length") 17)
                (string= (json-get arguments "text") "permission-secret")
                (null (search "permission-secret" entry))
                (search "text_length" entry)
                (search "e2" entry))
           "browser.type approval presentation redacts text without changing execution arguments")))
    (let* ((agent (agent-create :configuration configuration
                                :browser-logical-key "permission-browser"))
           (conversation (conversation-create configuration
                                              :identifier "browser-denied"))
           (registry (make-default-tool-registry :configuration configuration))
           (result
             (tool-registry-execute-call
              registry
              (json-object "namespace" "browser" "name" "snapshot"
                           "arguments" "{}")
              (make-instance 'tool-context
                             :configuration configuration
                             :worker nil
                             :conversation conversation
                             :agent agent
                             :tool-authorization-function
                             (lambda (tool arguments)
                               (declare (ignore tool arguments))
                               ':deny)))))
      (test-assert
       (and (not (tool-result-success-p result))
            (null (agent-browser-runtime-state agent)))
       "permission denial prevents browser session creation"))
    (let* ((left (agent-create :configuration configuration
                               :browser-logical-key "child-browser-left"))
           (right (agent-create :configuration configuration
                                :browser-logical-key "child-browser-right"))
           (left-runtime (agent-browser-runtime left))
           (right-runtime (agent-browser-runtime right)))
      (test-assert
       (and (not (eq left-runtime right-runtime))
            (not (string= (nous-browser-runtime-create-key left-runtime)
                          (nous-browser-runtime-create-key right-runtime)))
            (string= (nous-browser-runtime-logical-key left-runtime)
                     "child-browser-left")
            (string= (nous-browser-runtime-logical-key right-runtime)
                     "child-browser-right"))
       "child agents inherit the route with isolated browser sessions"))
    (let* ((agent (agent-create :configuration configuration
                                :browser-logical-key "status-browser"))
           (application (make-instance 'application
                                       :configuration configuration
                                       :agent agent))
           (text (terminal-ui--raw-spans-text
                  (application--browser-runtime-fields application))))
      (test-assert
       (and (search "browser state" text)
            (search "not-created" text)
            (search "status-browser" text))
       "status shows managed browser lifecycle and identity")))
  nil)
