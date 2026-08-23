(in-package #:autolith)

;;;; -- Managed Browser Tools --

(defclass browser-tool (tool)
  ()
  (:documentation "A Nous-managed browser operation executed through an agent-local CDP session."))

(defclass browser-navigate-tool (browser-tool) ()
  (:documentation "Navigate a managed browser to a public HTTP(S) URL."))

(defclass browser-snapshot-tool (browser-tool) ()
  (:documentation "Return a bounded accessibility snapshot with stable element refs."))

(defclass browser-click-tool (browser-tool) ()
  (:documentation "Click an element from the current browser snapshot."))

(defclass browser-type-tool (browser-tool) ()
  (:documentation "Type sensitive text into an element without durable argument persistence."))

(defclass browser-press-tool (browser-tool) ()
  (:documentation "Press a key in the managed browser."))

(defclass browser-scroll-tool (browser-tool) ()
  (:documentation "Scroll the managed browser page vertically."))

(defclass browser-back-tool (browser-tool) ()
  (:documentation "Navigate the managed browser page backward."))

(defclass browser-screenshot-tool (browser-tool) ()
  (:documentation "Capture the managed browser viewport as a native image attachment."))

(defmethod tool-child-safe-p ((tool browser-tool))
  "Permit browser tools only when an explicit child policy grants their namespace."
  (declare (ignore tool))
  t)

(defmethod tool-conversation-persistence ((tool browser-type-tool))
  "Keep typed text out of durable conversation history."
  (declare (ignore tool))
  ':next-response)

(defmethod tool-authorization-presentation-arguments
    ((tool browser-type-tool) arguments)
  "Remove typed text from permission presentation while retaining its length."
  (declare (ignore tool))
  (let ((presentation (json-object)))
    (maphash
     (lambda (name value)
       (unless (string= name "text")
         (setf (gethash name presentation) value)))
     arguments)
    (let ((text (json-get arguments "text")))
      (setf (gethash "text_length" presentation)
            (if (stringp text) (length text) 0)))
    presentation))

(-> browser-tool--authorized-p (browser-tool tool-context json-object) boolean)
(defun browser-tool--authorized-p (tool context arguments)
  "Return true when CONTEXT authorizes external browser TOOL ARGUMENTS."
  (eq (tool-context-authorize-tool context tool arguments) ':allow))

(-> browser-tool--runtime (tool-context) nous-browser-runtime)
(defun browser-tool--runtime (context)
  "Return CONTEXT's lazy agent-local managed browser runtime."
  (let ((agent (tool-context-agent context)))
    (unless (typep agent 'agent)
      (nous-browser--fail ':action "Browser tools require an active agent context."))
    (agent-browser-runtime agent)))

(-> browser-tool--string (json-object string &key (:maximum (option integer))) string)
(defun browser-tool--string (arguments name &key maximum)
  "Return required string NAME from ARGUMENTS with optional MAXIMUM length."
  (let ((value (tool-argument arguments name :required t)))
    (unless (non-empty-string-p value)
      (error 'tool-error
             :message (format nil "browser.~A requires a non-empty string." name)
             :tool-name "browser"))
    (when (and maximum (> (length value) maximum))
      (error 'tool-error
             :message (format nil "browser.~A exceeds ~D characters." name maximum)
             :tool-name "browser"))
    value))

(defmethod tool-execute
    ((tool browser-navigate-tool) (context tool-context) (arguments hash-table))
  "Authorize and navigate the current agent's managed browser."
  (if (browser-tool--authorized-p tool context arguments)
      (tool-success
       (nous-browser-runtime-navigate
        (browser-tool--runtime context)
        (browser-tool--string arguments "url" :maximum 2048)))
      (tool-failure "browser.navigate was denied.")))

(defmethod tool-execute
    ((tool browser-snapshot-tool) (context tool-context) (arguments hash-table))
  "Authorize and return the current accessibility snapshot."
  (if (browser-tool--authorized-p tool context arguments)
      (tool-success
       (nous-browser-runtime-snapshot (browser-tool--runtime context)))
      (tool-failure "browser.snapshot was denied.")))

(defmethod tool-execute
    ((tool browser-click-tool) (context tool-context) (arguments hash-table))
  "Authorize and click one current snapshot reference."
  (if (browser-tool--authorized-p tool context arguments)
      (tool-success
       (nous-browser-runtime-click
        (browser-tool--runtime context)
        (browser-tool--string arguments "ref" :maximum 64)))
      (tool-failure "browser.click was denied.")))

(defmethod tool-execute
    ((tool browser-type-tool) (context tool-context) (arguments hash-table))
  "Authorize and type text without returning or durably persisting its contents."
  (if (browser-tool--authorized-p tool context arguments)
      (tool-success
       (nous-browser-runtime-type
        (browser-tool--runtime context)
        (browser-tool--string arguments "ref" :maximum 64)
        (browser-tool--string arguments "text" :maximum 10000)))
      (tool-failure "browser.type was denied.")))

(defmethod tool-execute
    ((tool browser-press-tool) (context tool-context) (arguments hash-table))
  "Authorize and press one key."
  (if (browser-tool--authorized-p tool context arguments)
      (tool-success
       (nous-browser-runtime-press
        (browser-tool--runtime context)
        (browser-tool--string arguments "key" :maximum 64)))
      (tool-failure "browser.press was denied.")))

(defmethod tool-execute
    ((tool browser-scroll-tool) (context tool-context) (arguments hash-table))
  "Authorize and scroll by a bounded pixel amount."
  (if (browser-tool--authorized-p tool context arguments)
      (let ((amount (tool-argument arguments "amount" :required t)))
        (unless (integerp amount)
          (error 'tool-error
                 :message "browser.scroll amount must be an integer."
                 :tool-name "browser.scroll"))
        (tool-success
         (nous-browser-runtime-scroll (browser-tool--runtime context) amount)))
      (tool-failure "browser.scroll was denied.")))

(defmethod tool-execute
    ((tool browser-back-tool) (context tool-context) (arguments hash-table))
  "Authorize and navigate backward."
  (if (browser-tool--authorized-p tool context arguments)
      (tool-success
       (nous-browser-runtime-back (browser-tool--runtime context)))
      (tool-failure "browser.back was denied.")))

(defmethod tool-execute
    ((tool browser-screenshot-tool) (context tool-context) (arguments hash-table))
  "Authorize and return a native private screenshot attachment."
  (if (browser-tool--authorized-p tool context arguments)
      (let ((attachment
              (nous-browser-runtime-screenshot
               (browser-tool--runtime context)
               (tool-context-conversation context))))
        (tool-success "Captured the current browser viewport."
                      :content-blocks
                      (list "Current browser viewport:" attachment)))
      (tool-failure "browser.screenshot was denied.")))


;;;; -- Registration --

(-> browser-tools-register (tool-registry) tool-registry)
(defun browser-tools-register (registry)
  "Register the complete Nous-managed browser namespace in REGISTRY."
  (let ((empty (tool-object-schema (json-object) nil)))
    (dolist
        (specification
         (list
          (list 'browser-navigate-tool "navigate"
                "Navigate the managed browser to one public absolute HTTP(S) URL."
                (tool-object-schema
                 (json-object
                  "url" (tool-string-property "Public absolute HTTP(S) URL."))
                 '("url")))
          (list 'browser-snapshot-tool "snapshot"
                "Return a bounded accessibility snapshot. Element refs are valid only until the next navigation or action."
                empty)
          (list 'browser-click-tool "click"
                "Click one element reference from the latest browser.snapshot."
                (tool-object-schema
                 (json-object
                  "ref" (tool-string-property "Stable ref from the latest snapshot, such as e3."))
                 '("ref")))
          (list 'browser-type-tool "type"
                "Type text into one element from the latest snapshot. Typed text is not echoed in results or durable history."
                (tool-object-schema
                 (json-object
                  "ref" (tool-string-property "Stable ref from the latest snapshot.")
                  "text" (tool-string-property "Text to insert; it is treated as sensitive."))
                 '("ref" "text")))
          (list 'browser-press-tool "press"
                "Press one keyboard key in the focused page element."
                (tool-object-schema
                 (json-object
                  "key" (tool-string-property "CDP key value, for example Enter, Tab, or Escape."))
                 '("key")))
          (list 'browser-scroll-tool "scroll"
                "Scroll vertically by a bounded signed pixel amount."
                (let ((amount (tool-integer-property
                               "Signed pixels: positive scrolls down, negative scrolls up.")))
                  (setf (gethash "minimum" amount) -10000
                        (gethash "maximum" amount) 10000)
                  (tool-object-schema (json-object "amount" amount) '("amount"))))
          (list 'browser-back-tool "back"
                "Navigate the current page backward."
                empty)
          (list 'browser-screenshot-tool "screenshot"
                "Capture the current viewport as a native image attachment."
                empty)))
      (destructuring-bind (class name description parameters) specification
        (tool-registry-register
         registry
         (make-instance class
                        :namespace "browser"
                        :name name
                        :description description
                        :parameters parameters)))))
  registry)
