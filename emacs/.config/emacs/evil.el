(use-package evil
  :demand t
  :preface
  (defun my/insert-two-spaces ()
    (interactive)
    (insert "  "))

  :init
  (setq evil-cross-lines t
        evil-shift-width 2
        evil-split-window-below t
        evil-vsplit-window-right t
        evil-want-C-u-scroll t
        evil-want-integration t
        evil-want-keybinding nil
        evil-want-Y-yank-to-eol t

        my/leader "SPC")

  :config
  ;; Move up one line when doing `q:` and `q/`.
  (advice-add 'evil-command-window-ex :after #'evil-previous-line)
  (advice-add 'evil-command-window-search-forward :after #'evil-previous-line)

  ;; Disable hl-line-mode in evil visual state.
  (add-hook 'evil-visual-state-entry-hook
            (lambda () (hl-line-mode -1)))
  (add-hook 'evil-visual-state-exit-hook
            (lambda () (hl-line-mode)))

  (evil-mode 1)

  :general
  ;; TODO: Make "ESC" quit minibuffer completion stuff?

  ('insert
   "C-SPC" 'my/insert-two-spaces)

  ;; TODO: Motion state might be useless/bad to use, see noctuid/evil-guide on
  ;; github.
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
   "M"  'evil-jump-item)

  ('motion
   :prefix my/leader
   "SPC" 'execute-extended-command
   "x"   'execute-extended-command
   ;; TODO: Figure out why some keybindings, such as "SPC SPC" and "-" do not
   ;; work in Buffer Menu mode.
   "l"   'buffer-menu)

  ('normal
   "gs" 'evil-write))

(use-package evil-collection
  :after evil
  :demand t
  :config
  (evil-collection-init))

(use-package evil-commentary
  :after evil
  :demand t
  ;; I don't use the minor mode since it sets a super key binding which I don't
  ;; like.
  :general
  ('normal
   "gc" 'evil-commentary
   "gy" 'evil-commentary-yank))

(use-package evil-surround
  :after evil
  :demand t
  :config
  (global-evil-surround-mode 1))
