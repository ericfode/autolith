;;; autolith.el --- Attach Spacemacs to Autolith -*- lexical-binding: t; -*-

;; Copyright (c) 2026 Eric Fode
;; SPDX-License-Identifier: ISC
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, processes, ai

;;; Commentary:

;; This package attaches an Emacs terminal buffer to an authenticated Autolith
;; localgroup session and sends source selections through the same durable input
;; path.  It has no dependencies outside Emacs and the Autolith executable.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'project)
(require 'subr-x)
(require 'tabulated-list)
(require 'server)
(require 'term)

(defgroup autolith nil
  "Connect Emacs to local Autolith sessions."
  :group 'tools
  :prefix "autolith-")

(defcustom autolith-executable "autolith"
  "Autolith executable used for attachment and message submission."
  :type 'file
  :group 'autolith)

(defcustom autolith-localgroup-directory nil
  "Override the directory containing Autolith localgroup endpoint records.

When nil, derive it from XDG_STATE_HOME or ~/.local/state/."
  :type '(choice (const :tag "Use the XDG state directory" nil)
                 directory)
  :group 'autolith)

(defcustom autolith-default-attach-mode 'take-over
  "Terminal ownership requested by `autolith-attach'."
  :type '(choice (const :tag "Take over the controlling terminal" take-over)
                 (const :tag "Observe without terminal control" read-only))
  :group 'autolith)

(defcustom autolith-message-character-limit 131072
  "Maximum characters submitted in one message."
  :type 'integer
  :group 'autolith)

(defcustom autolith-chat-buffer-format "*Autolith %s*"
  "Format string for an attached Autolith terminal buffer.

The single substitution receives the displayed session identifier."
  :type 'string
  :group 'autolith)

(defcustom autolith-enable-editor-bridge t
  "Whether the layer exposes scoped editor commands through emacsclient."
  :type 'boolean
  :group 'autolith)

(defcustom autolith-editor-server-name nil
  "Optional Emacs server name dedicated to the Autolith editor bridge.

When nil, reuse the current server name or start its default server.  When a
server is already active in this Emacs process, its name must match this value."
  :type '(choice (const :tag "Use the current Emacs server name" nil)
                 string)
  :group 'autolith)

(defcustom autolith-editor-registry-directory nil
  "Override the directory publishing the active Spacemacs bridge.

When nil, use the spacemacs directory below Autolith's XDG state root."
  :type '(choice (const :tag "Use the XDG state directory" nil)
                 directory)
  :group 'autolith)

(defcustom autolith-editor-text-limit 131072
  "Maximum text accepted by one command from Autolith."
  :type 'integer
  :group 'autolith)

(defconst autolith-editor--server-name-limit 200
  "Maximum portable Emacs server name length accepted by the bridge.")

(defconst autolith--message-argument-byte-limit 126976
  "Maximum UTF-8 bytes in one portable direct process argument.")

(defconst autolith-editor--encoded-request-limit 126976
  "Maximum Base64 request bytes decoded by the editor dispatcher.")

(defconst autolith--common-lisp-universal-time-offset 2208988800
  "Seconds between Common Lisp universal time and Unix time.")

(defvar autolith--chat-buffers (make-hash-table :test #'equal)
  "Attached terminal buffers keyed by canonical localgroup session ID.")

(defvar autolith-last-session-id nil
  "Most recently selected Autolith localgroup session ID.")

(defvar-local autolith-session-id nil
  "Canonical localgroup session ID owned by the current chat buffer.")

(defvar-local autolith-attachment-mode nil
  "Attachment mode used by the current chat buffer.")

(defvar autolith-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-d") #'autolith-detach)
    (define-key map (kbd "C-c C-s") #'autolith-send-prompt)
    (define-key map (kbd "C-c C-l") #'autolith-list-sessions)
    map)
  "Keymap active in attached Autolith terminal buffers.")

(define-minor-mode autolith-chat-mode
  "Mark a terminal buffer as an attached Autolith conversation."
  :lighter " AL"
  :keymap autolith-chat-mode-map)

(defun autolith--program ()
  "Return the executable pathname configured for Autolith."
  (let ((program
         (if (file-name-absolute-p autolith-executable)
             (and (file-executable-p autolith-executable)
                  autolith-executable)
           (executable-find autolith-executable))))
    (unless program
      (user-error "Cannot find the Autolith executable %S"
                  autolith-executable))
    program))

(defun autolith--state-directory ()
  "Return the effective XDG state directory."
  (file-name-as-directory
   (expand-file-name
    (or (getenv "XDG_STATE_HOME")
        (expand-file-name ".local/state" "~")))))

(defun autolith--registry-directory ()
  "Return the effective localgroup endpoint directory."
  (file-name-as-directory
   (expand-file-name
    (or autolith-localgroup-directory
        (expand-file-name "autolith/localgroup"
                          (autolith--state-directory))))))

(defun autolith--skip-record-space (text position)
  "Return the first non-whitespace position in TEXT at or after POSITION."
  (while (and (< position (length text))
              (memq (aref text position) '(?\s ?\t ?\n ?\r)))
    (setq position (1+ position)))
  position)

(defun autolith--registry-field-position (text name)
  "Return the value position for field NAME in endpoint record TEXT."
  (let ((case-fold-search t))
    (when (string-match (format ":%s\\_>" (regexp-quote name)) text)
      (autolith--skip-record-space text (match-end 0)))))

(defun autolith--registry-string-field (text name)
  "Return string field NAME from endpoint record TEXT."
  (when-let ((position (autolith--registry-field-position text name)))
    (when (and (< position (length text))
               (eq (aref text position) ?\"))
      (let ((end (string-search "\"" text (1+ position))))
        (and end (substring text (1+ position) end))))))

(defun autolith--registry-integer-field (text name)
  "Return integer field NAME from endpoint record TEXT."
  (when-let ((position (autolith--registry-field-position text name)))
    (let ((end position))
      (while (and (< end (length text))
                  (<= ?0 (aref text end) ?9))
        (setq end (1+ end)))
      (and (> end position)
           (string-to-number (substring text position end))))))

(defun autolith--registry-record (pathname)
  "Read a bounded endpoint record from PATHNAME.

Return a validated plist without invoking the Emacs Lisp reader."
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents pathname nil 0 4096)
        (let* ((text (buffer-string))
               (case-fold-search t)
               (session-id (autolith--registry-string-field text "SESSION-ID"))
               (pid (autolith--registry-integer-field text "PID"))
               (created-at
                (autolith--registry-integer-field text "CREATED-AT")))
          (when (and (string-match-p
                      "\\`[[:space:]]*(:LOCALGROUP-ENDPOINT\\_>" text)
                     session-id
                     (string-match-p "\\`[[:alnum:]]+\\'" session-id)
                     (integerp pid)
                     (> pid 0)
                     (integerp created-at)
                     (> created-at 0))
            (list :session-id session-id
                  :pid pid
                  :created-at created-at
                  :pathname pathname))))
    (file-error nil)))

(defun autolith--pid-live-p (pid)
  "Return non-nil when local process PID exists."
  (and (integerp pid)
       (process-attributes pid)))

(defun autolith--session-records ()
  "Return live localgroup endpoint records, newest first."
  (let ((directory (autolith--registry-directory)))
    (if (file-directory-p directory)
        (sort
         (delq nil
               (mapcar
                (lambda (pathname)
                  (let ((record (autolith--registry-record pathname)))
                    (and record
                         (autolith--pid-live-p (plist-get record :pid))
                         record)))
                (directory-files directory t "\\.sexp\\'" t)))
         (lambda (left right)
           (> (plist-get left :created-at)
              (plist-get right :created-at))))
      nil)))

(defun autolith--session-record (session-id)
  "Return the live endpoint record for SESSION-ID."
  (cl-find session-id (autolith--session-records)
           :key (lambda (record) (plist-get record :session-id))
           :test #'string=))

(defun autolith--display-session-id (session-id)
  "Return SESSION-ID with Autolith's visual separator."
  (if (= (length session-id) 7)
      (concat (substring session-id 0 1)
              "-"
              (substring session-id 1))
    session-id))

(defun autolith--session-time (created-at)
  "Return a local display time for Common Lisp universal time CREATED-AT."
  (if (> created-at autolith--common-lisp-universal-time-offset)
      (format-time-string
       "%Y-%m-%d %H:%M"
       (seconds-to-time
        (- created-at autolith--common-lisp-universal-time-offset)))
    "unknown"))

(defun autolith--session-label (record)
  "Return a completion label for endpoint RECORD."
  (format "%-9s  pid %-7d  %s"
          (autolith--display-session-id (plist-get record :session-id))
          (plist-get record :pid)
          (autolith--session-time (plist-get record :created-at))))

(defun autolith-read-session ()
  "Prompt for one live Autolith localgroup session ID."
  (let ((records (autolith--session-records)))
    (unless records
      (user-error "No live Autolith localgroup sessions were found"))
    (let* ((candidates
            (mapcar (lambda (record)
                      (cons (autolith--session-label record)
                            (plist-get record :session-id)))
                    records))
           (choice
            (completing-read "Autolith session: " candidates nil t)))
      (cdr (assoc choice candidates)))))

(defun autolith-select-session ()
  "Select the default Autolith session for subsequent submissions."
  (interactive)
  (setq autolith-last-session-id (autolith-read-session))
  (message "Selected Autolith session %s"
           (autolith--display-session-id autolith-last-session-id)))

(defun autolith--chat-buffer (session-id)
  "Return the live attached buffer for SESSION-ID."
  (let ((buffer (gethash session-id autolith--chat-buffers)))
    (and (buffer-live-p buffer) buffer)))

(defun autolith--remove-chat-buffer ()
  "Remove the current chat buffer from attachment bookkeeping."
  (when autolith-session-id
    (let ((registered (gethash autolith-session-id autolith--chat-buffers)))
      (when (eq registered (current-buffer))
        (remhash autolith-session-id autolith--chat-buffers)))))

(defun autolith--attachment-sentinel (process event)
  "Run the terminal sentinel and clean up PROCESS after EVENT."
  (let ((previous (process-get process 'autolith-previous-sentinel)))
    (when previous
      (funcall previous process event)))
  (unless (process-live-p process)
    (let ((buffer (process-buffer process))
          (session-id (process-get process 'autolith-session-id)))
      (when (eq (gethash session-id autolith--chat-buffers) buffer)
        (remhash session-id autolith--chat-buffers)))))

(defun autolith--attach-arguments (session-id mode)
  "Return Autolith attachment arguments for SESSION-ID and MODE."
  (append (list "localgroup" "attach" session-id)
          (pcase mode
            ('take-over '("--take-over"))
            ('read-only '("--read-only"))
            (_ (error "Unsupported Autolith attachment mode %S" mode)))))

;;;###autoload
(defun autolith-attach (session-id &optional mode)
  "Attach a terminal buffer to Autolith SESSION-ID using MODE.

Interactively, a prefix argument requests a read-only attachment.  Without a
prefix argument, use `autolith-default-attach-mode'."
  (interactive
   (list (autolith-read-session)
         (if current-prefix-arg
             'read-only
           autolith-default-attach-mode)))
  (setq mode (or mode autolith-default-attach-mode))
  (let ((existing (autolith--chat-buffer session-id)))
    (if existing
        (if (eq mode (buffer-local-value 'autolith-attachment-mode existing))
            (pop-to-buffer existing)
          (user-error
           "Autolith session %s is already attached using %S; detach it before requesting %S"
           (autolith--display-session-id session-id)
           (buffer-local-value 'autolith-attachment-mode existing)
           mode))
      (let* ((display-id (autolith--display-session-id session-id))
             (buffer-name (format autolith-chat-buffer-format display-id))
             (term-name
              (string-remove-suffix
               "*"
               (string-remove-prefix "*" buffer-name)))
             (buffer
              (apply #'make-term
                     term-name
                     (autolith--program)
                     nil
                     (autolith--attach-arguments session-id mode)))
             (process (get-buffer-process buffer)))
        (unless process
          (kill-buffer buffer)
          (user-error "Autolith attachment did not start"))
        (with-current-buffer buffer
          (term-mode)
          (term-char-mode)
          (setq-local autolith-session-id session-id
                      autolith-attachment-mode mode
                      header-line-format
                      (format " Autolith %s  %s  C-c C-d detach  C-c C-s send"
                              display-id mode))
          (autolith-chat-mode 1)
          (add-hook 'kill-buffer-hook #'autolith--remove-chat-buffer nil t))
        (process-put process 'autolith-session-id session-id)
        (process-put process 'autolith-previous-sentinel
                     (process-sentinel process))
        (set-process-sentinel process #'autolith--attachment-sentinel)
        (set-process-query-on-exit-flag process nil)
        (puthash session-id buffer autolith--chat-buffers)
        (setq autolith-last-session-id session-id)
        (pop-to-buffer buffer)))))

;;;###autoload
(defun autolith-focus-chat (&optional choose-session)
  "Focus an attached Autolith buffer.

With CHOOSE-SESSION or an interactive prefix argument, prompt for the session."
  (interactive "P")
  (let* ((session-id
          (if choose-session
              (autolith-read-session)
            autolith-last-session-id))
         (buffer (and session-id (autolith--chat-buffer session-id))))
    (cond
     (buffer
      (pop-to-buffer buffer))
     (session-id
      (autolith-attach session-id))
     (t
      (call-interactively #'autolith-attach)))))

;;;###autoload
(defun autolith-detach ()
  "Close the current Autolith attachment buffer."
  (interactive)
  (unless autolith-session-id
    (user-error "The current buffer is not an Autolith attachment"))
  (let ((process (get-buffer-process (current-buffer))))
    (when (process-live-p process)
      (delete-process process)))
  (kill-buffer (current-buffer)))

(defun autolith--target-session (choose-session)
  "Return a live submission target, prompting when CHOOSE-SESSION is non-nil."
  (let ((session-id
         (cond
          (choose-session
           (autolith-read-session))
          ((and autolith-session-id
                (autolith--session-record autolith-session-id))
           autolith-session-id)
          ((and autolith-last-session-id
                (autolith--session-record autolith-last-session-id))
           autolith-last-session-id)
          (t
           (autolith-read-session)))))
    (setq autolith-last-session-id session-id)
    session-id))

(defun autolith--tell-sentinel (process event)
  "Report completion of an Autolith tell PROCESS after EVENT."
  (when (memq (process-status process) '(exit signal))
    (let ((buffer (process-buffer process))
          (session-id (process-get process 'autolith-session-id)))
      (if (and (eq (process-status process) 'exit)
               (zerop (process-exit-status process)))
          (progn
            (when (buffer-live-p buffer)
              (kill-buffer buffer))
            (message "Sent to Autolith %s"
                     (autolith--display-session-id session-id)))
        (when (buffer-live-p buffer)
          (display-buffer buffer))
        (message "Autolith submission failed: %s" (string-trim event))))))

(defun autolith--send-message (session-id message)
  "Send MESSAGE to Autolith SESSION-ID through localgroup tell."
  (when (> (length message) autolith-message-character-limit)
    (user-error "Autolith message has %d characters; limit is %d"
                (length message)
                autolith-message-character-limit))
  (when (string-match-p "\0" message)
    (user-error "Autolith messages cannot contain NUL characters"))
  (let ((byte-count (length (encode-coding-string message 'utf-8-unix))))
    (when (> byte-count autolith--message-argument-byte-limit)
      (user-error "Autolith message has %d UTF-8 bytes; argument limit is %d"
                  byte-count autolith--message-argument-byte-limit)))
  (let* ((program (autolith--program))
         (buffer
          (generate-new-buffer
           (format " *Autolith tell %s*" session-id)))
         process)
    (condition-case condition
        (progn
          (setq process
                (make-process
                 :name (format "autolith-tell-%s" session-id)
                 :buffer buffer
                 :command (list program "localgroup" "tell" session-id message)
                 :coding 'utf-8-unix
                 :connection-type 'pipe
                 :noquery t
                 :sentinel #'autolith--tell-sentinel))
          (process-put process 'autolith-session-id session-id)
          process)
      (error
       (when (and (processp process) (process-live-p process))
         (delete-process process))
       (when (buffer-live-p buffer)
         (kill-buffer buffer))
       (signal (car condition) (cdr condition))))))

;;;###autoload
(defun autolith-send-prompt (prompt &optional choose-session)
  "Send PROMPT to an Autolith session.

With CHOOSE-SESSION or an interactive prefix argument, prompt for the target."
  (interactive (list (read-string "Ask Autolith: ") current-prefix-arg))
  (when (string-empty-p (string-trim prompt))
    (user-error "The Autolith prompt is empty"))
  (autolith--send-message
   (autolith--target-session choose-session)
   prompt))

(defun autolith--project-root ()
  "Return the current project root, when available."
  (when-let ((project (project-current nil)))
    (expand-file-name (project-root project))))

(defun autolith--source-path ()
  "Return a stable display path for the current source buffer."
  (if buffer-file-name
      (let* ((pathname (expand-file-name buffer-file-name))
             (root (autolith--project-root))
             (relative (and root (file-relative-name pathname root))))
        (if (and relative
                 (not (string-prefix-p "../" relative))
                 (not (string= relative "..")))
            relative
          pathname))
    (format "buffer:%s" (buffer-name))))

(defun autolith--source-language ()
  "Return a Markdown language label for the current major mode."
  (cond
   ((derived-mode-p 'emacs-lisp-mode) "elisp")
   ((derived-mode-p 'lisp-mode) "common-lisp")
   ((derived-mode-p 'clojure-mode) "clojure")
   ((derived-mode-p 'scheme-mode) "scheme")
   ((derived-mode-p 'python-mode) "python")
   ((derived-mode-p 'js-mode 'js-ts-mode) "javascript")
   ((derived-mode-p 'typescript-mode 'typescript-ts-mode) "typescript")
   ((derived-mode-p 'c-mode 'c-ts-mode) "c")
   ((derived-mode-p 'c++-mode 'c++-ts-mode) "cpp")
   ((derived-mode-p 'rust-mode 'rust-ts-mode) "rust")
   (t (string-remove-suffix "-mode" (symbol-name major-mode)))))

(defun autolith--region-line-range (beginning end)
  "Return one-based line numbers spanning BEGINNING through END."
  (save-restriction
    (widen)
    (cons (line-number-at-pos beginning t)
          (line-number-at-pos
           (if (> end beginning) (1- end) end)
           t))))

(defun autolith--markdown-fence (text)
  "Return a Markdown fence longer than every backtick run in TEXT."
  (let ((maximum 0)
        (run 0))
    (dotimes (position (length text))
      (if (eq (aref text position) ?`)
          (progn
            (setq run (1+ run))
            (setq maximum (max maximum run)))
        (setq run 0)))
    (make-string (max 3 (1+ maximum)) ?`)))

(defun autolith--context-message (beginning end question)
  "Return a source-context message for BEGINNING, END, and QUESTION."
  (let ((payload-characters (+ (- end beginning) (length question))))
    (when (> payload-characters autolith-message-character-limit)
      (user-error
       "Autolith source and question have %d characters before the envelope; limit is %d"
       payload-characters autolith-message-character-limit)))
  (let* ((text (buffer-substring-no-properties beginning end))
         (lines (autolith--region-line-range beginning end))
         (path (replace-regexp-in-string "[\n\r]" " "
                                         (autolith--source-path)))
         (language (autolith--source-language))
         (fence (autolith--markdown-fence text))
         (request
          (if (string-empty-p (string-trim question))
              "Review this source selection."
            question)))
    (format
     (concat "Selected source context from Emacs:\n\n"
             "Path: `%s`\n"
             "Lines: %d-%d\n"
             "Major mode: `%s`\n"
             "Buffer modified: %s\n\n"
             "%s%s\n%s\n%s\n\n"
             "Question:\n%s")
     path
     (car lines)
     (cdr lines)
     major-mode
     (if (buffer-modified-p) "yes" "no")
     fence language text fence request)))

(defun autolith--send-context-range
    (beginning end question choose-session)
  "Send source from BEGINNING to END with QUESTION to Autolith.

CHOOSE-SESSION controls whether to prompt for a target."
  (autolith--send-message
   (autolith--target-session choose-session)
   (autolith--context-message beginning end question)))

;;;###autoload
(defun autolith-send-region
    (beginning end question &optional choose-session)
  "Send the active region from BEGINNING to END with QUESTION.

With CHOOSE-SESSION or an interactive prefix argument, prompt for the target."
  (interactive
   (progn
     (unless (use-region-p)
       (user-error "Select source code before calling autolith-send-region"))
     (list (region-beginning)
           (region-end)
           (read-string "Ask Autolith about the selection: ")
           current-prefix-arg)))
  (autolith--send-context-range beginning end question choose-session))

;;;###autoload
(defun autolith-send-dwim (question &optional choose-session)
  "Send the active region or current definition with QUESTION.

With CHOOSE-SESSION or an interactive prefix argument, prompt for the target."
  (interactive
   (list (read-string "Ask Autolith about this code: ")
         current-prefix-arg))
  (let ((bounds
         (if (use-region-p)
             (cons (region-beginning) (region-end))
           (bounds-of-thing-at-point 'defun))))
    (unless bounds
      (user-error "No active region or definition at point"))
    (autolith--send-context-range
     (car bounds) (cdr bounds) question choose-session)))

;;;###autoload
;;;###autoload
(defun autolith-send-buffer (question &optional choose-session)
  "Send the complete current buffer with QUESTION.

With CHOOSE-SESSION or an interactive prefix argument, prompt for the target."
  (interactive
   (list (read-string "Ask Autolith about this buffer: ")
         current-prefix-arg))
  (save-restriction
    (widen)
    (autolith--send-context-range
     (point-min) (point-max) question choose-session)))

(define-derived-mode autolith-sessions-mode tabulated-list-mode
  "Autolith-Sessions"
  "List live Autolith localgroup sessions."
  (setq tabulated-list-format
        [("Session" 11 t)
         ("PID" 9 t)
         ("Started" 17 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key '("Started" . t))
  (tabulated-list-init-header))

(defun autolith-sessions-refresh ()
  "Refresh the current Autolith session list."
  (interactive)
  (unless (derived-mode-p 'autolith-sessions-mode)
    (user-error "The current buffer is not an Autolith session list"))
  (setq tabulated-list-entries
        (mapcar
         (lambda (record)
           (let ((session-id (plist-get record :session-id)))
             (list session-id
                   (vector
                    (autolith--display-session-id session-id)
                    (number-to-string (plist-get record :pid))
                    (autolith--session-time
                     (plist-get record :created-at))))))
         (autolith--session-records)))
  (tabulated-list-print t))

(defun autolith-sessions-attach (&optional read-only)
  "Attach to the session at point.

With READ-ONLY or an interactive prefix argument, observe without control."
  (interactive "P")
  (let ((session-id (tabulated-list-get-id)))
    (unless session-id
      (user-error "No Autolith session on this row"))
    (autolith-attach session-id
                     (if read-only
                         'read-only
                       autolith-default-attach-mode))))

(define-key autolith-sessions-mode-map (kbd "RET")
            #'autolith-sessions-attach)
(define-key autolith-sessions-mode-map (kbd "g")
            #'autolith-sessions-refresh)

;;;###autoload
(defun autolith-list-sessions ()
  "Display live Autolith localgroup sessions."
  (interactive)
  (let ((buffer (get-buffer-create "*Autolith Sessions*")))
    (with-current-buffer buffer
      (autolith-sessions-mode)
      (autolith-sessions-refresh))
    (pop-to-buffer buffer)))

;;;; Editor bridge

(defvar autolith-editor--registry-pathname nil
  "Endpoint record owned by the active editor bridge.")

(defvar autolith-editor--watched-server-process nil
  "Emacs server process watched by the active editor bridge.")

(defvar autolith-editor--previous-server-sentinel nil
  "Server sentinel replaced while the editor bridge is active.")

(defvar autolith-editor--owns-server nil
  "Non-nil when the bridge started the current Emacs server.")

(defvar autolith-editor-bridge-mode nil
  "Non-nil when scoped Autolith editor operations are published.")

(defun autolith-editor--registry-directory ()
  "Return the directory containing live Spacemacs bridge records."
  (file-name-as-directory
   (expand-file-name
    (or autolith-editor-registry-directory
        (expand-file-name "autolith/spacemacs"
                          (autolith--state-directory))))))

(defun autolith-editor--server-live-p ()
  "Return non-nil when this Emacs process owns a live server."
  (and (processp server-process)
       (process-live-p server-process)))

(defun autolith-editor--server-name-p (value)
  "Return non-nil when VALUE is one portable bridge server name."
  (and (stringp value)
       (<= 1 (length value) autolith-editor--server-name-limit)
       (string-match-p "\\`[[:alnum:]_.-]+\\'" value)))

(defun autolith-editor--ensure-server ()
  "Start or reuse this Emacs process's server and return its name."
  (let* ((live-p (autolith-editor--server-live-p))
         (name
          (if live-p
              (progn
                (when (and autolith-editor-server-name
                           (not (equal autolith-editor-server-name server-name)))
                  (user-error
                   "The active Emacs server is %S, not requested bridge server %S"
                   server-name autolith-editor-server-name))
                server-name)
            (or autolith-editor-server-name server-name))))
    (unless (autolith-editor--server-name-p name)
      (user-error "Invalid Autolith bridge server name %S" name))
    (unless live-p
      (setq server-name name)
      (when (server-running-p server-name)
        (user-error "Another Emacs process already owns server %S" server-name))
      (server-start)
      (setq autolith-editor--owns-server t)
      (unless (autolith-editor--server-live-p)
        (error "Emacs server %S did not start" server-name)))
    name))

(defun autolith-editor--server-exit-sentinel (process event)
  "Run the original server sentinel and unpublish a dead PROCESS after EVENT."
  (unwind-protect
      (when autolith-editor--previous-server-sentinel
        (funcall autolith-editor--previous-server-sentinel process event))
    (when (and (eq process autolith-editor--watched-server-process)
               (not (process-live-p process)))
      (setq autolith-editor--watched-server-process nil
            autolith-editor--previous-server-sentinel nil
            autolith-editor--owns-server nil
            autolith-editor-bridge-mode nil)
      (remove-hook 'kill-emacs-hook #'autolith-editor--unpublish)
      (autolith-editor--unpublish))))

(defun autolith-editor--unwatch-server ()
  "Restore the sentinel of the server process watched by the bridge."
  (when (and (processp autolith-editor--watched-server-process)
             (process-live-p autolith-editor--watched-server-process)
             (eq (process-sentinel autolith-editor--watched-server-process)
                 #'autolith-editor--server-exit-sentinel))
    (set-process-sentinel autolith-editor--watched-server-process
                          autolith-editor--previous-server-sentinel))
  (setq autolith-editor--watched-server-process nil
        autolith-editor--previous-server-sentinel nil))

(defun autolith-editor--stop-owned-server ()
  "Stop the Emacs server when it was started by the editor bridge."
  (when autolith-editor--owns-server
    (setq autolith-editor--owns-server nil)
    (when (autolith-editor--server-live-p)
      (server-force-delete))))

(defun autolith-editor--watch-server ()
  "Watch the active Emacs server so dead endpoints are unpublished."
  (unless (autolith-editor--server-live-p)
    (error "The Autolith bridge has no live Emacs server"))
  (unless (eq server-process autolith-editor--watched-server-process)
    (autolith-editor--unwatch-server)
    (setq autolith-editor--watched-server-process server-process
          autolith-editor--previous-server-sentinel
          (process-sentinel server-process))
    (set-process-sentinel server-process
                          #'autolith-editor--server-exit-sentinel)))

(defun autolith-editor--publish ()
  "Publish this Emacs process's private bridge discovery record."
  (let* ((directory (autolith-editor--registry-directory))
         (pathname
          (expand-file-name (format "%d.sexp" (emacs-pid)) directory))
         (temporary nil)
         (server (autolith-editor--ensure-server)))
    (autolith-editor--watch-server)
    (make-directory directory t)
    (set-file-modes directory #o700)
    (setq temporary (make-temp-file (expand-file-name ".bridge-" directory)))
    (unwind-protect
        (progn
          (with-temp-file temporary
            (insert
             (format
              (concat "(:AUTOLITH-SPACEMACS :VERSION 1 :PID %d "
                      ":SERVER-NAME %S :CREATED-AT %d)\n")
              (emacs-pid) server (floor (float-time)))))
          (set-file-modes temporary #o600)
          (rename-file temporary pathname t)
          (setq temporary nil
                autolith-editor--registry-pathname pathname))
      (when (and temporary (file-exists-p temporary))
        (delete-file temporary)))))

(defun autolith-editor--unpublish ()
  "Remove the bridge discovery record owned by this Emacs process."
  (when (and autolith-editor--registry-pathname
             (file-exists-p autolith-editor--registry-pathname))
    (delete-file autolith-editor--registry-pathname))
  (setq autolith-editor--registry-pathname nil))

;;;###autoload
(define-minor-mode autolith-editor-bridge-mode
  "Expose scoped editor operations to authorized Autolith tools."
  :global t
  :group 'autolith
  (if autolith-editor-bridge-mode
      (condition-case condition
          (progn
            (autolith-editor--publish)
            (add-hook 'kill-emacs-hook #'autolith-editor--unpublish))
        (error
         (remove-hook 'kill-emacs-hook #'autolith-editor--unpublish)
         (autolith-editor--unpublish)
         (autolith-editor--unwatch-server)
         (autolith-editor--stop-owned-server)
         (setq autolith-editor-bridge-mode nil)
         (signal (car condition) (cdr condition))))
    (remove-hook 'kill-emacs-hook #'autolith-editor--unpublish)
    (autolith-editor--unpublish)
    (autolith-editor--unwatch-server)
    (autolith-editor--stop-owned-server)))

(defun autolith-editor--request-value (request name)
  "Return NAME from decoded editor REQUEST."
  (alist-get name request nil nil #'string=))

(defun autolith-editor--required-string (request name)
  "Return required nonempty string NAME from REQUEST."
  (let ((value (autolith-editor--request-value request name)))
    (unless (and (stringp value) (not (string-empty-p value)))
      (error "Editor command requires nonempty string %s" name))
    value))

(defun autolith-editor--required-text (request name)
  "Return required string NAME from REQUEST, permitting empty text."
  (let ((value (autolith-editor--request-value request name)))
    (unless (stringp value)
      (error "Editor command requires string %s" name))
    value))

(defun autolith-editor--optional-integer (request name default minimum)
  "Return integer NAME from REQUEST or DEFAULT, bounded below by MINIMUM."
  (let ((value (autolith-editor--request-value request name)))
    (cond
     ((null value)
      default)
     ((and (integerp value) (>= value minimum))
      value)
     (t
      (error "Editor command %s must be an integer of at least %d"
             name minimum)))))

(defun autolith-editor--required-integer (request name minimum)
  "Return required integer NAME from REQUEST, bounded below by MINIMUM."
  (let ((value (autolith-editor--request-value request name)))
    (unless (and (integerp value) (>= value minimum))
      (error "Editor command %s must be an integer of at least %d"
             name minimum))
    value))

(defun autolith-editor--bounded-text (request name)
  "Return required text NAME from REQUEST within the configured limit."
  (let ((text (autolith-editor--required-text request name)))
    (when (> (length text) autolith-editor-text-limit)
      (error "Editor command %s exceeds the %d-character limit"
             name autolith-editor-text-limit))
    text))

(defun autolith-editor--boolean (value)
  "Return VALUE in the Boolean representation expected by `json-serialize'."
  (if value t :false))

(defun autolith-editor--project-root ()
  "Return the current project root as a string, when available."
  (when-let ((project (project-current nil)))
    (expand-file-name (project-root project))))

(defun autolith-editor--region-state ()
  "Return the active region state as a JSON-ready alist, or nil."
  (when (use-region-p)
    (let* ((beginning (region-beginning))
           (end (region-end))
           (text-end (min end
                          (+ beginning (max 0 autolith-editor-text-limit))))
           (text (buffer-substring-no-properties beginning text-end)))
      `(("beginning" . ,beginning)
        ("end" . ,end)
        ("start_line" . ,(line-number-at-pos beginning t))
        ("end_line" . ,(line-number-at-pos
                         (if (> end beginning) (1- end) end) t))
       ("text" . ,text)))))

(defun autolith-editor--buffer-state ()
  "Return the selected buffer's JSON-ready editor state."
  `(("buffer" . ,(buffer-name))
    ("file" . ,buffer-file-name)
    ("major_mode" . ,(symbol-name major-mode))
    ("point" . ,(point))
    ("line" . ,(line-number-at-pos (point) t))
    ("column" . ,(current-column))
    ("modified" . ,(autolith-editor--boolean (buffer-modified-p)))
    ("read_only" . ,(autolith-editor--boolean buffer-read-only))
    ("project_root" . ,(autolith-editor--project-root))
    ("region" . ,(autolith-editor--region-state))))

(defun autolith-editor--workspace-root (request)
  "Return REQUEST's canonical workspace directory."
  (let* ((workspace (autolith-editor--required-string request "workspace"))
         (root (file-name-as-directory (file-truename workspace))))
    (unless (file-directory-p root)
      (error "Editor workspace is not a directory: %s" workspace))
    root))

(defun autolith-editor--workspace-file (request)
  "Return REQUEST's canonical regular file inside its workspace."
  (let* ((root (autolith-editor--workspace-root request))
         (path (file-truename
                (autolith-editor--required-string request "path"))))
    (unless (file-regular-p path)
      (error "Editor path is not a regular file: %s" path))
    (unless (file-in-directory-p path root)
      (error "Editor path is outside workspace %s: %s" root path))
    path))

(defun autolith-editor--workspace-buffer-file (request &optional buffer)
  "Return BUFFER's canonical regular file inside REQUEST's workspace."
  (with-current-buffer (or buffer (current-buffer))
    (unless buffer-file-name
      (error "The selected buffer does not visit a file"))
    (let ((root (autolith-editor--workspace-root request))
          (path (file-truename buffer-file-name)))
      (unless (file-regular-p path)
        (error "The selected path is not a regular file: %s" path))
      (unless (file-in-directory-p path root)
        (error "The selected file is outside workspace %s: %s" root path))
      path)))

(defun autolith-editor--goto-line (line column)
  "Move point to one-based LINE and zero-based COLUMN."
  (goto-char (point-min))
  (when (/= (forward-line (1- line)) 0)
    (error "Editor line %d is outside the current buffer" line))
  (move-to-column column))

(defun autolith-editor--visit-file (request)
  "Visit REQUEST's workspace file and optional location."
  (let ((path (autolith-editor--workspace-file request))
        (line (autolith-editor--optional-integer request "line" 1 1))
        (column (autolith-editor--optional-integer request "column" 0 0)))
    (find-file path)
    (autolith-editor--goto-line line column)))

(defun autolith-editor--focus-buffer (request)
  "Switch to REQUEST's existing workspace file buffer."
  (let* ((name (autolith-editor--required-string request "buffer"))
         (buffer (get-buffer name)))
    (unless buffer
      (error "No Emacs buffer is named %S" name))
    (autolith-editor--workspace-buffer-file request buffer)
    (switch-to-buffer buffer)))

(defun autolith-editor--move-point (request)
  "Move point in the selected workspace buffer according to REQUEST."
  (autolith-editor--workspace-buffer-file request)
  (autolith-editor--goto-line
   (autolith-editor--required-integer request "line" 1)
   (autolith-editor--optional-integer request "column" 0 0)))

(defun autolith-editor--insert-text (request)
  "Insert REQUEST's bounded text at point in the selected workspace buffer."
  (autolith-editor--workspace-buffer-file request)
  (when buffer-read-only
    (error "The selected buffer is read-only"))
  (atomic-change-group
    (insert (autolith-editor--bounded-text request "text"))))

(defun autolith-editor--replace-region (request)
  "Replace the active workspace region with REQUEST's bounded text."
  (autolith-editor--workspace-buffer-file request)
  (unless (use-region-p)
    (error "The selected buffer has no active region"))
  (when buffer-read-only
    (error "The selected buffer is read-only"))
  (let ((beginning (region-beginning))
        (end (region-end))
        (text (autolith-editor--bounded-text request "text")))
    (atomic-change-group
      (delete-region beginning end)
      (goto-char beginning)
      (insert text))
    (deactivate-mark)))

(defun autolith-editor--save-buffer (request)
  "Save the selected workspace file buffer."
  (autolith-editor--workspace-buffer-file request)
  (save-buffer))

(defun autolith-editor--dispatch (request)
  "Execute one validated editor REQUEST and return a JSON-ready response."
  (unless autolith-editor-bridge-mode
    (error "The Autolith editor bridge is disabled"))
  (let ((operation (autolith-editor--required-string request "operation")))
    (pcase operation
      ("status"
       (autolith-editor--workspace-buffer-file request))
      ("visit-file" (autolith-editor--visit-file request))
      ("focus-buffer" (autolith-editor--focus-buffer request))
      ("goto-line" (autolith-editor--move-point request))
      ("insert-text" (autolith-editor--insert-text request))
      ("replace-region" (autolith-editor--replace-region request))
      ("save-buffer" (autolith-editor--save-buffer request))
      ("message"
       (autolith-editor--workspace-buffer-file request)
       (message "%s" (autolith-editor--bounded-text request "text")))
      (_
       (error "Unsupported editor operation %S" operation)))
    `(("ok" . t)
      ("operation" . ,operation)
      ("state" . ,(autolith-editor--buffer-state)))))

(defun autolith-editor-dispatch-json (encoded-request)
  "Dispatch one Base64-encoded JSON request from an Autolith tool.

Return a JSON string.  This is the only function the tool invokes through
emacsclient; it accepts no Lisp forms or command symbols."
  (let ((json-false :false))
    (condition-case condition
        (progn
          (unless (stringp encoded-request)
            (error "Editor request must be a Base64 string"))
          (when (> (string-bytes encoded-request)
                   autolith-editor--encoded-request-limit)
            (error "Encoded editor request exceeds the %d-byte limit"
                   autolith-editor--encoded-request-limit))
          (let* ((json
                  (decode-coding-string
                   (base64-decode-string encoded-request)
                   'utf-8-unix))
                 (request
                  (json-parse-string
                   json
                   :object-type 'alist
                   :array-type 'list
                   :null-object nil
                   :false-object :false)))
            (json-encode (autolith-editor--dispatch request))))
      (error
       (json-encode
        `(("ok" . :false)
          ("error" . ,(error-message-string condition))))))))

(provide 'autolith)

;;; autolith.el ends here
