;;; packages.el --- Autolith layer packages -*- lexical-binding: nil; -*-

;; Copyright (c) 2026 Eric Fode
;; SPDX-License-Identifier: ISC

(defconst autolith-packages
  '((autolith :location local)))

(defun autolith/init-autolith ()
  "Initialize the local Autolith Emacs package."
  (use-package autolith
    :demand t
    :commands (autolith-attach
               autolith-focus-chat
               autolith-list-sessions
               autolith-select-session
               autolith-send-prompt
               autolith-send-region
               autolith-send-dwim
               autolith-send-buffer)
    :init
    (spacemacs/declare-prefix "aA" "autolith")
    (spacemacs/set-leader-keys
      "aAa" 'autolith-attach
      "aAc" 'autolith-focus-chat
      "aAl" 'autolith-list-sessions
      "aAs" 'autolith-select-session
      "aAp" 'autolith-send-prompt
      "aAr" 'autolith-send-region
      "aAd" 'autolith-send-dwim
      "aAb" 'autolith-send-buffer)
    :config
    (autolith-editor-bridge-mode
     (if autolith-enable-editor-bridge 1 -1))))

;;; packages.el ends here
