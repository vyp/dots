(defun my-minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it. Then it
takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(defun my-exit-evil-command-window ()
  "Exit evil command window."
  (interactive)
  ;; For whatever reason, moving to the previous window and back again means
  ;; after quitting this command window, it will properly go back to the
  ;; previous window. Otherwise it seems to go to some other unintuitive window.
  (other-window -1)
  (other-window 1)
  (kill-this-buffer)
  (evil-window-delete))

;; Escape quits emacs things as a vim user would expect.
(define-key evil-motion-state-map           [escape] 'keyboard-quit)
(define-key evil-normal-state-map           [escape] 'keyboard-quit)
(define-key evil-visual-state-map           [escape] 'keyboard-quit)
(define-key minibuffer-local-map            [escape] 'my-minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map         [escape] 'my-minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'my-minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'my-minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map    [escape] 'my-minibuffer-keyboard-quit)
(global-set-key [escape] 'evil-exit-emacs-state)
(evil-define-key 'normal evil-command-window-mode-map [escape] 'my-exit-evil-command-window)

(define-key evil-motion-state-map "\\" 'evil-switch-to-windows-last-buffer)
(define-key evil-normal-state-map "\\" 'evil-switch-to-windows-last-buffer)

(define-key evil-motion-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-motion-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-motion-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-motion-state-map (kbd "C-l") 'evil-window-right)
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)

(define-key evil-motion-state-map   "H" 'evil-first-non-blank)
(define-key evil-normal-state-map   "H" 'evil-first-non-blank)
(define-key evil-operator-state-map "H" 'evil-jump-item)
(define-key evil-visual-state-map   "H" 'evil-first-non-blank)
(define-key evil-motion-state-map   "L" 'evil-end-of-line)
(define-key evil-normal-state-map   "L" 'evil-end-of-line)
(define-key evil-operator-state-map "L" 'evil-end-of-line)
(define-key evil-visual-state-map   "L" 'evil-end-of-line)
(define-key evil-motion-state-map   "M" 'evil-jump-item)
(define-key evil-normal-state-map   "M" 'evil-jump-item)
(define-key evil-operator-state-map "M" 'evil-jump-item)
(define-key evil-visual-state-map   "M" 'evil-jump-item)

(define-key evil-motion-state-map (vconcat "z" [return]) 'evil-scroll-line-to-top)
(define-key evil-normal-state-map (vconcat "z" [return]) 'evil-scroll-line-to-top)
(define-key evil-visual-state-map (vconcat "z" [return]) 'evil-scroll-line-to-top)

(defun my-evil-yank-to-end-of-line ()
  "Yank from cursor position to end of line."
  (interactive)
  (evil-yank (point) (point-at-eol)))

(defun my-insert-two-spaces ()
  (interactive)
  (insert "  "))

(defun my-insert-one-space ()
  (interactive)
  (insert " "))

(define-key evil-motion-state-map "+" 'text-scale-increase)
(define-key evil-normal-state-map "+" 'text-scale-increase)
(define-key evil-motion-state-map "-" 'text-scale-decrease)
(define-key evil-normal-state-map "-" 'text-scale-decrease)
(define-key evil-motion-state-map "'" 'evil-ex)
(define-key evil-normal-state-map "'" 'evil-ex)
(define-key evil-visual-state-map "'" 'evil-ex)
(define-key evil-normal-state-map "gs" 'evil-write)
(define-key evil-motion-state-map "Y"  'my-evil-yank-to-end-of-line)
(define-key evil-normal-state-map "Y"  'my-evil-yank-to-end-of-line)
(define-key evil-normal-state-map (kbd "C-SPC") 'my-insert-two-spaces)
(define-key evil-insert-state-map (kbd "C-SPC") 'my-insert-two-spaces)
(define-key evil-normal-state-map (kbd "M-SPC") 'my-insert-one-space)
(define-key evil-insert-state-map (kbd "M-SPC") 'my-insert-one-space)

;; Leader layer.
(defun my-evil-edit-dot-emacs ()
  "Edit emacs init file."
  (interactive)
  (evil-edit "~/ui/emacs/.emacs.d/init.el"))

(defun my-delete-whitespace-around-point ()
  (interactive)
  (just-one-space 0))

(defun my-recenter-quarter-ish ()
  (interactive)
  (recenter 15))

;; Required for the following motion state maps that start with space.
(define-key evil-motion-state-map " " nil)

(define-key evil-motion-state-map (kbd "U") 'recenter-top-bottom)
(define-key evil-normal-state-map (kbd "U") 'recenter-top-bottom)
(define-key evil-motion-state-map (kbd "SPC SPC") 'my-recenter-quarter-ish)
(define-key evil-normal-state-map (kbd "SPC SPC") 'my-recenter-quarter-ish)
(define-key evil-motion-state-map (kbd "SPC ei")  'my-evil-edit-dot-emacs)
(define-key evil-normal-state-map (kbd "SPC ei")  'my-evil-edit-dot-emacs)
(define-key evil-normal-state-map (kbd "SPC ds")  'just-one-space)
(define-key evil-normal-state-map
  (kbd "SPC dw") 'my-delete-whitespace-around-point)
(define-key evil-normal-state-map (kbd "SPC f")   'fill-paragraph)
(define-key evil-motion-state-map (kbd "SPC h")   'help)
(define-key evil-normal-state-map (kbd "SPC h")   'help)
(define-key evil-normal-state-map (kbd "SPC ;")   'evil-command-window-ex)
(define-key evil-visual-state-map (kbd "SPC ;")   'evil-command-window-ex)
(define-key evil-motion-state-map (kbd "SPC we")  'balance-windows)
(define-key evil-normal-state-map (kbd "SPC we")  'balance-windows)
(define-key evil-motion-state-map (kbd "SPC wo")  'delete-other-windows)
(define-key evil-normal-state-map (kbd "SPC wo")  'delete-other-windows)
(define-key evil-motion-state-map (kbd "SPC x")   'execute-extended-command)
(define-key evil-normal-state-map (kbd "SPC x")   'execute-extended-command)
(define-key evil-visual-state-map (kbd "SPC x")   'execute-extended-command)

;; Generic special.
(define-key evil-motion-state-map (kbd "SPC l") 'ibuffer)
(define-key evil-normal-state-map (kbd "SPC l") 'ibuffer)
(define-key evil-normal-state-map "gcc"         'evilnc-comment-or-uncomment-lines)
(define-key evil-visual-state-map "gc"          'comment-or-uncomment-region)
;; (define-key evil-normal-state-map "gm"          'magit-status)

(provide 'my-custom-evil-keybindings)
