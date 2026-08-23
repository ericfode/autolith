;;; autolith-test.el --- Tests for the Autolith Emacs package -*- lexical-binding: t; -*-

;; Copyright (c) 2026 Eric Fode
;; SPDX-License-Identifier: ISC

(require 'ert)
(require 'cl-lib)
(require 'autolith)

(defun autolith-test--editor-dispatch (request)
  "Round-trip REQUEST through the fixed Base64 JSON editor dispatcher."
  (let ((autolith-editor-bridge-mode t))
    (json-parse-string
     (autolith-editor-dispatch-json
      (base64-encode-string (json-encode request) t))
     :object-type 'alist
     :array-type 'list
     :null-object nil
     :false-object :false)))

(ert-deftest autolith-test-registry-record-parses-bounded-fields ()
  (let ((pathname (make-temp-file "autolith-endpoint-" nil ".sexp")))
    (unwind-protect
        (progn
          (with-temp-file pathname
            (insert
             "(:LOCALGROUP-ENDPOINT :VERSION 1 :SESSION-ID \"Ab3dE9x\" "
             ":PID 1234 :ADDRESS \"127.0.0.1\" :PORT 5555 "
             ":TOKEN \"secret\" :CREATED-AT 3996451062)\n"))
          (let ((record (autolith--registry-record pathname)))
            (should (equal "Ab3dE9x" (plist-get record :session-id)))
            (should (= 1234 (plist-get record :pid)))
            (should (= 3996451062 (plist-get record :created-at)))
            (should (equal pathname (plist-get record :pathname)))))
      (delete-file pathname))))

(ert-deftest autolith-test-registry-record-rejects-other-forms ()
  (let ((pathname (make-temp-file "autolith-endpoint-" nil ".sexp")))
    (unwind-protect
        (progn
          (with-temp-file pathname
            (insert
             "(:OTHER-ENDPOINT :SESSION-ID \"Ab3dE9x\" :PID 1234 "
             ":CREATED-AT 3996451062)\n"))
          (should-not (autolith--registry-record pathname)))
      (delete-file pathname))))

(ert-deftest autolith-test-registry-record-rejects-truncated-string-field ()
  (let ((pathname (make-temp-file "autolith-endpoint-" nil ".sexp")))
    (unwind-protect
        (progn
          (with-temp-file pathname
            (insert "(:LOCALGROUP-ENDPOINT :SESSION-ID"))
          (should-not (autolith--registry-record pathname)))
      (delete-file pathname))))

(ert-deftest autolith-test-session-records-filter-and-sort ()
  (let ((directory (make-temp-file "autolith-localgroup-" t)))
    (unwind-protect
        (let ((autolith-localgroup-directory directory))
          (with-temp-file (expand-file-name "old.sexp" directory)
            (insert
             "(:LOCALGROUP-ENDPOINT :SESSION-ID \"old1234\" :PID 11 "
             ":CREATED-AT 3996451000)\n"))
          (with-temp-file (expand-file-name "new.sexp" directory)
            (insert
             "(:LOCALGROUP-ENDPOINT :SESSION-ID \"new1234\" :PID 22 "
             ":CREATED-AT 3996452000)\n"))
          (with-temp-file (expand-file-name "dead.sexp" directory)
            (insert
             "(:LOCALGROUP-ENDPOINT :SESSION-ID \"dead123\" :PID 33 "
             ":CREATED-AT 3996453000)\n"))
          (cl-letf (((symbol-function 'autolith--pid-live-p)
                     (lambda (pid) (memq pid '(11 22)))))
            (should
             (equal '("new1234" "old1234")
                    (mapcar
                     (lambda (record) (plist-get record :session-id))
                     (autolith--session-records))))))
      (delete-directory directory t))))

(ert-deftest autolith-test-region-line-range-excludes-next-line-at-end ()
  (with-temp-buffer
    (insert "one\ntwo\nthree\n")
    (should (equal '(1 . 2)
                   (autolith--region-line-range (point-min) 9)))))

(ert-deftest autolith-test-markdown-fence-grows-past-source-run ()
  (should (equal "````" (autolith--markdown-fence "x ``` y")))
  (should (equal "`````" (autolith--markdown-fence "x ```` y"))))

(ert-deftest autolith-test-context-message-carries-source-snapshot ()
  (with-temp-buffer
    (setq buffer-file-name "/repo/src/example.el")
    (emacs-lisp-mode)
    (insert "(defun example ()\n  42)\n")
    (set-buffer-modified-p t)
    (cl-letf (((symbol-function 'autolith--project-root)
               (lambda () "/repo/")))
      (let ((message
             (autolith--context-message
              (point-min) (point-max) "Explain this function.")))
        (should (string-match-p "Path: `src/example.el`" message))
        (should (string-match-p "Lines: 1-2" message))
        (should (string-match-p "Major mode: `emacs-lisp-mode`" message))
        (should (string-match-p "Buffer modified: yes" message))
        (should (string-match-p "```elisp" message))
        (should (string-match-p "(defun example ()" message))
        (should (string-match-p "Question:\nExplain this function\\." message))))))

(ert-deftest autolith-test-attach-arguments-are-explicit ()
  (should
   (equal '("localgroup" "attach" "Ab3dE9x" "--take-over")
          (autolith--attach-arguments "Ab3dE9x" 'take-over)))
  (should
   (equal '("localgroup" "attach" "Ab3dE9x" "--read-only")
          (autolith--attach-arguments "Ab3dE9x" 'read-only))))

(ert-deftest autolith-test-attach-rejects-different-existing-mode ()
  (let* ((session-id "Ab3dE9x")
         (buffer (generate-new-buffer " *autolith-attachment-mode*")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local autolith-attachment-mode 'read-only))
          (puthash session-id buffer autolith--chat-buffers)
          (should-error (autolith-attach session-id 'take-over)
                        :type 'user-error))
      (remhash session-id autolith--chat-buffers)
      (kill-buffer buffer))))

(ert-deftest autolith-test-message-limit-fails-before-process-start ()
  (let ((autolith-message-character-limit 3))
    (should-error (autolith--send-message "Ab3dE9x" "four")
                  :type 'user-error)))

(ert-deftest autolith-test-message-byte-limit-fails-before-process-start ()
  (let ((autolith-message-character-limit 10)
        (autolith--message-argument-byte-limit 5))
    (should-error (autolith--send-message "Ab3dE9x" "☃☃")
                  :type 'user-error)))

(ert-deftest autolith-test-message-process-startup-cleans-buffer ()
  (let (created-buffer)
    (cl-letf (((symbol-function 'autolith--program)
               (lambda () "/bin/false"))
              ((symbol-function 'make-process)
               (lambda (&rest arguments)
                 (setq created-buffer (plist-get arguments :buffer))
                 (error "process startup failed"))))
      (should-error (autolith--send-message "Ab3dE9x" "hello"))
      (should-not (buffer-live-p created-buffer)))))

(ert-deftest autolith-test-send-buffer-widens-temporarily ()
  (with-temp-buffer
    (insert "one\ntwo\nthree\n")
    (narrow-to-region 5 9)
    (let (captured)
      (cl-letf (((symbol-function 'autolith--send-context-range)
                 (lambda (beginning end question choose-session)
                   (declare (ignore question choose-session))
                   (setq captured
                         (buffer-substring-no-properties beginning end)))))
        (autolith-send-buffer "inspect"))
      (should (equal "one\ntwo\nthree\n" captured))
      (should (buffer-narrowed-p)))))

(ert-deftest autolith-test-editor-server-names-are-portable ()
  (dolist (name '("server" "autolith.1" "editor_test" "bridge-name"))
    (should (autolith-editor--server-name-p name)))
  (dolist (name '(nil "" "has space" "path/name" "bad:server"))
    (should-not (autolith-editor--server-name-p name))))

(ert-deftest autolith-test-editor-publishes-private-record ()
  (let ((directory (make-temp-file "autolith-editor-registry-" t))
        (autolith-editor--registry-pathname nil))
    (unwind-protect
        (let ((autolith-editor-registry-directory directory))
          (cl-letf (((symbol-function 'autolith-editor--ensure-server)
                     (lambda () "bridge-1"))
                    ((symbol-function 'autolith-editor--watch-server)
                     #'ignore))
            (autolith-editor--publish))
          (should (file-exists-p autolith-editor--registry-pathname))
          (should (= #o700 (logand #o777 (file-modes directory))))
          (should (= #o600
                     (logand #o777
                             (file-modes autolith-editor--registry-pathname))))
          (with-temp-buffer
            (insert-file-contents autolith-editor--registry-pathname)
            (should (string-match-p
                     "\\`(:AUTOLITH-SPACEMACS :VERSION 1 :PID [0-9]+ "
                     (buffer-string)))
            (should (string-match-p
                     ":SERVER-NAME \"bridge-1\" :CREATED-AT [0-9]+)"
                     (buffer-string)))
            (should (string-suffix-p ")\n" (buffer-string))))
          (let ((pathname autolith-editor--registry-pathname))
            (autolith-editor--unpublish)
            (should-not (file-exists-p pathname))))
      (autolith-editor--unpublish)
      (delete-directory directory t))))

(ert-deftest autolith-test-editor-stops-only-bridge-owned-server ()
  (let ((autolith-editor--owns-server t)
        stopped)
    (cl-letf (((symbol-function 'autolith-editor--server-live-p)
               (lambda () t))
              ((symbol-function 'server-force-delete)
               (lambda () (setq stopped t))))
      (autolith-editor--stop-owned-server)
      (should stopped)
      (should-not autolith-editor--owns-server)))
  (let ((autolith-editor--owns-server nil)
        stopped)
    (cl-letf (((symbol-function 'autolith-editor--server-live-p)
               (lambda () t))
              ((symbol-function 'server-force-delete)
               (lambda () (setq stopped t))))
      (autolith-editor--stop-owned-server)
      (should-not stopped))))

(ert-deftest autolith-test-editor-dispatch-round-trips-and-inserts ()
  (let* ((workspace (make-temp-file "autolith-editor-workspace-" t))
         (pathname (expand-file-name "example.txt" workspace)))
    (unwind-protect
        (progn
          (with-temp-file pathname
            (insert "a"))
          (with-temp-buffer
            (setq buffer-file-name pathname)
            (insert "a")
            (goto-char (point-max))
            (let ((response
                   (autolith-test--editor-dispatch
                    `(("operation" . "insert-text")
                      ("workspace" . ,workspace)
                      ("text" . "b")))))
              (should (eq t (alist-get "ok" response nil nil #'string=)))
              (should (equal "insert-text"
                             (alist-get "operation" response nil nil #'string=)))
              (should (equal "ab" (buffer-string))))))
      (delete-directory workspace t))))

(ert-deftest autolith-test-editor-dispatch-reports-closed-operation-errors ()
  (with-temp-buffer
    (let ((response
           (autolith-test--editor-dispatch
            `(("operation" . "eval")
              ("workspace" . ,default-directory)))))
      (should (eq :false (alist-get "ok" response nil nil #'string=)))
      (should (string-match-p
               "Unsupported editor operation"
               (alist-get "error" response nil nil #'string=))))))

(ert-deftest autolith-test-editor-dispatch-rejects-disabled-bridge ()
  (let* ((autolith-editor-bridge-mode nil)
         (encoded
          (base64-encode-string
           (json-encode `(("operation" . "status")
                          ("workspace" . ,default-directory)))
           t))
         (response
          (json-parse-string
           (autolith-editor-dispatch-json encoded)
           :object-type 'alist
           :array-type 'list
           :null-object nil
           :false-object :false)))
    (should (eq :false (alist-get "ok" response nil nil #'string=)))
    (should (string-match-p
             "editor bridge is disabled"
             (alist-get "error" response nil nil #'string=)))))

(ert-deftest autolith-test-editor-dispatch-rejects-oversized-encoding ()
  (let* ((autolith-editor--encoded-request-limit 4)
         (response
          (json-parse-string
           (autolith-editor-dispatch-json "AAAAA")
           :object-type 'alist
           :array-type 'list
           :null-object nil
           :false-object :false)))
    (should (eq :false (alist-get "ok" response nil nil #'string=)))
    (should (string-match-p
             "Encoded editor request exceeds"
             (alist-get "error" response nil nil #'string=)))))

(ert-deftest autolith-test-editor-region-state-bounds-copy ()
  (with-temp-buffer
    (insert "abcdefghij")
    (let ((autolith-editor-text-limit 3)
          (transient-mark-mode t))
      (goto-char (point-min))
      (set-mark (point-max))
      (setq mark-active t)
      (let ((state (autolith-editor--region-state)))
        (should (equal "abc" (alist-get "text" state nil nil #'string=)))
        (should (= (point-max)
                   (alist-get "end" state nil nil #'string=)))))))

(ert-deftest autolith-test-editor-rejects-state-outside-workspace ()
  (let ((workspace (make-temp-file "autolith-editor-workspace-" t))
        (outside (make-temp-file "autolith-editor-outside-")))
    (unwind-protect
        (with-temp-buffer
          (setq buffer-file-name outside)
          (insert "secret")
          (let ((transient-mark-mode t))
            (goto-char (point-min))
            (set-mark (point-max))
            (setq mark-active t)
            (dolist (request
                     (list `(("operation" . "status")
                             ("workspace" . ,workspace))
                           `(("operation" . "message")
                             ("workspace" . ,workspace)
                             ("text" . "hello"))))
              (let ((response (autolith-test--editor-dispatch request)))
                (should (eq :false
                            (alist-get "ok" response nil nil #'string=)))
                (should (string-match-p
                         "outside workspace"
                         (alist-get "error" response nil nil #'string=)))))))
      (delete-file outside)
      (delete-directory workspace t))))

(ert-deftest autolith-test-editor-visit-rejects-symlink-escape ()
  (let* ((workspace (make-temp-file "autolith-editor-workspace-" t))
         (outside-directory (make-temp-file "autolith-editor-outside-" t))
         (outside (expand-file-name "outside.txt" outside-directory))
         (link (expand-file-name "escape.txt" workspace)))
    (unwind-protect
        (progn
          (with-temp-file outside
            (insert "outside"))
          (make-symbolic-link outside link t)
          (let ((response
                 (autolith-test--editor-dispatch
                  `(("operation" . "visit-file")
                    ("workspace" . ,workspace)
                    ("path" . ,link)))))
            (should (eq :false (alist-get "ok" response nil nil #'string=)))
            (should (string-match-p
                     "outside workspace"
                     (alist-get "error" response nil nil #'string=)))))
      (delete-directory workspace t)
      (delete-directory outside-directory t))))

(ert-deftest autolith-test-editor-rejects-mutation-outside-workspace ()
  (let ((workspace (make-temp-file "autolith-editor-workspace-" t))
        (outside (make-temp-file "autolith-editor-outside-")))
    (unwind-protect
        (with-temp-buffer
          (setq buffer-file-name outside)
          (insert "secret")
          (should-error
           (autolith-editor--insert-text
            `(("workspace" . ,workspace) ("text" . "!"))))
          (should (equal "secret" (buffer-string))))
      (delete-file outside)
      (delete-directory workspace t))))

(ert-deftest autolith-test-editor-rejects-focus-outside-workspace ()
  (let ((workspace (make-temp-file "autolith-editor-workspace-" t))
        (outside (make-temp-file "autolith-editor-outside-"))
        (buffer (generate-new-buffer " *autolith-outside*")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq buffer-file-name outside))
          (should-error
           (autolith-editor--focus-buffer
            `(("workspace" . ,workspace)
              ("buffer" . ,(buffer-name buffer))))))
      (kill-buffer buffer)
      (delete-file outside)
      (delete-directory workspace t))))

(ert-deftest autolith-test-editor-empty-replacement-deletes-region ()
  (let* ((workspace (make-temp-file "autolith-editor-workspace-" t))
         (pathname (expand-file-name "example.txt" workspace)))
    (unwind-protect
        (progn
          (with-temp-file pathname
            (insert "abc"))
          (with-temp-buffer
            (setq buffer-file-name pathname)
            (insert "abc")
            (let ((transient-mark-mode t))
              (goto-char 2)
              (set-mark 4)
              (setq mark-active t)
              (autolith-editor--replace-region
               `(("workspace" . ,workspace) ("text" . ""))))
            (should (equal "a" (buffer-string)))
            (should-not mark-active)))
      (delete-directory workspace t))))


(ert-deftest autolith-test-editor-replacement-rolls-back-on-hook-error ()
  (let* ((workspace (make-temp-file "autolith-editor-workspace-" t))
         (pathname (expand-file-name "example.txt" workspace)))
    (unwind-protect
        (progn
          (with-temp-file pathname
            (insert "abc"))
          (with-temp-buffer
            (setq buffer-file-name pathname)
            (insert "abc")
            (let ((transient-mark-mode t)
                  (fail t))
              (goto-char 2)
              (set-mark 4)
              (setq mark-active t)
              (add-hook
               'after-change-functions
               (lambda (&rest arguments)
                 (declare (ignore arguments))
                 (when fail
                   (setq fail nil)
                   (error "hook failure")))
               nil t)
              (should-error
               (autolith-editor--replace-region
                `(("workspace" . ,workspace) ("text" . "x"))))
              (should (equal "abc" (buffer-string))))))
      (delete-directory workspace t))))
;;; autolith-test.el ends here
