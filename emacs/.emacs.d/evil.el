(use-package evil
  :demand t
  :preface
  (defun my/evil-yank-to-end-of-line ()
    (interactive)
    (evil-yank (point) (point-at-eol)))

  (defun my/insert-two-spaces ()
    (interactive)
    (insert "  "))

  :init
  (setq evil-cross-lines         t
        evil-shift-width         2
        evil-split-window-below  t
        evil-vsplit-window-right t
        evil-want-C-u-scroll     t
        my/leader                "SPC")

  :config
  ;; Move up one line when doing `q:` and `q/`.
  (advice-add 'evil-command-window-ex :after #'evil-previous-line)
  (advice-add 'evil-command-window-search-forward :after #'evil-previous-line)

  ;; Disable hl-line-mode in evil visual state.
  (add-hook 'evil-visual-state-entry-hook
            (lambda () (setq-local global-hl-line-mode nil)))
  (add-hook 'evil-visual-state-exit-hook
            (lambda () (setq-local global-hl-line-mode t)))

  (evil-mode t)

  :general
  ;; TODO: Make "ESC" quit minibuffer completion stuff?

  ('insert
   "C-SPC" 'my/insert-two-spaces)

  ('motion
   ;; Has to be unset so that Emacs can make prefix bindings with non-prefix
   ;; keys. Make sure this is the same as `my/leader`.
   "SPC" nil

   ;; TODO: Figure out how to make this work.
   ; "z RET" 'evil-scroll-line-to-top

   "\\" 'evil-switch-to-windows-last-buffer
   "'"  'evil-ex
   "+"  'text-scale-increase
   "-"  'text-scale-decrease
   "H"  'evil-first-non-blank
   "L"  'evil-end-of-line
   "M"  'evil-jump-item
   "Y"  'my/evil-yank-to-end-of-line)

  ('motion
   :prefix my/leader
   "SPC" 'execute-extended-command
   ;; TODO: Figure out why some keybindings, such as "SPC SPC" and "-" do not
   ;; work in Buffer Menu mode.
   "l"   'buffer-menu)

  ('normal
   "gs" 'evil-write
   ;; For some reason needs to be in normal state map as well, despite already
   ;; being in motion state map.
   "Y"  'my/evil-yank-to-end-of-line))
