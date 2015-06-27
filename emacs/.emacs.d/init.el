(add-to-list 'load-path "~/etsi/emacs-packages/undo-tree")
(add-to-list 'load-path "~/etsi/emacs-packages/goto-chg")
(add-to-list 'load-path "~/etsi/emacs-packages/evil")
(add-to-list 'custom-theme-load-path "~/etsi/emacs-packages/sunburst")

(setq evil-want-C-u-scroll t)
(require 'evil)
(evil-mode t)

;; Theme.
(load-theme 'sunburst t)
(custom-set-faces
 `(linum ((t (:foreground, "#666"))))
 `(hl-line ((t (:background, "#222")))))
(set-face-attribute 'vertical-border nil :foreground "#111")
(set-face-attribute 'fringe nil :background "#111")
(set-face-attribute 'lazy-highlight nil :foreground "#111")

;; Basic settings.
(setq auto-save-default nil)
(setq create-lockfiles nil)

(set-face-italic-p 'italic nil)
(mapc
 (lambda (face)
   (set-face-attribute face nil :weight 'normal :underline nil))
 (face-list))

(global-linum-mode t)
(global-hl-line-mode t)
