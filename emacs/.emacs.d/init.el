(add-to-list 'load-path "~/etsi/emacs-packages/undo-tree")
(add-to-list 'load-path "~/etsi/emacs-packages/goto-chg")
(add-to-list 'load-path "~/etsi/emacs-packages/evil")

(require 'evil)
(evil-mode t)

; Newbie (basic?) settings.
(setq auto-save-default nil)
(setq create-lockfiles nil)

; Does not seem to work...
(set-face-italic-p 'italic nil)
(set-face-bold-p 'bold nil)

; Enable line numbers.
(global-linum-mode t)
