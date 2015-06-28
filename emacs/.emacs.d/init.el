;; TODO: Vim config like evil mode keybindings.
;; TODO: Keymap M-x.
;; TODO: Show trailing spaces/lines, tabs etc.
;; TODO: Filesystem navigation, opening files, managing buffers etc.
;; TODO: tpope's repeat.vim functionality?
;; TODO: surround.vim evil plugin.
;; TODO: Folding.
;; TODO: Autocompletion.
;; TODO: Snippets.
;; TODO: Magit.
;; TODO: Statusbar colors and customisation.
;; TODO: Learn elisp.

;; Plugins.
(add-to-list 'load-path "~/etsi/emacs-packages/undo-tree")
(add-to-list 'load-path "~/etsi/emacs-packages/goto-chg")
(add-to-list 'load-path "~/etsi/emacs-packages/evil")
(add-to-list 'load-path "~/etsi/emacs-packages/fill-column-indicator")
(add-to-list 'custom-theme-load-path "~/etsi/emacs-packages/themes/sunburst")

(setq evil-want-C-u-scroll t)
; (setq evil-cross-lines t)
(setq evil-shift-width 2)

(add-hook 'python-mode-hook
  (function (lambda ()
    (setq evil-shift-width python-indent))))

(require 'evil)
(require 'fill-column-indicator)

(evil-mode t)

;; Theme.
(load-theme 'sunburst t)
(custom-set-faces
 `(linum ((t (:foreground, "#666"))))
 `(hl-line ((t (:background, "#222")))))
(set-face-attribute 'vertical-border nil :foreground "#111")
(set-face-attribute 'fringe nil :background "#111")
(set-face-attribute 'lazy-highlight nil :foreground "#111")
(setq fci-rule-color "#222")

;; Appearance.
(set-face-italic-p 'italic nil)
(mapc
 (lambda (face)
   (set-face-attribute face nil :weight 'normal :underline nil))
 (face-list))

(global-linum-mode t)
(global-hl-line-mode t)
(fringe-mode '(1 . 0))

(setq-default fill-column 80)
(define-globalized-minor-mode my-global-fci-mode fci-mode turn-on-fci-mode)
(my-global-fci-mode t)

;; Basic.
(setq auto-save-default nil)
(setq create-lockfiles nil)
(setq require-final-newline t)

(setq-default auto-fill-function 'do-auto-fill)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

(setq recenter-positions '(0.25))
(mouse-avoidance-mode 'banish)
(custom-set-variables
 '(mouse-avoidance-banish-position
   '((frame-or-window . frame)
     (side . right)
     (side-pos . 0)
     (top-or-bottom . bottom)
     (top-or-bottom-pos . 0))))

;; Keybindings.
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'evil-exit-emacs-state)

(define-key evil-normal-state-map "\\" 'evil-repeat-find-char-reverse)

(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)

(define-key evil-normal-state-map "gs" 'evil-write)
(define-key evil-normal-state-map " " 'evil-ex)

(define-key evil-normal-state-map ",l" 'ibuffer)
(define-key evil-normal-state-map ",x" 'execute-extended-command)
(define-key evil-normal-state-map ",z" 'recenter-top-bottom)
