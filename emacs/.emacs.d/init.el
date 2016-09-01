;; Packages
;; ________
;;
(defvar fi/recipes
  '((auto-complete
     :fetcher file
     :files ("*.el")
     :path "~/ui/vendor/emacs/auto-complete")

    (bind-key
     :fetcher file
     :files ("bind-key.el")
     :path "~/ui/vendor/emacs/use-package")

    (dash
     :fetcher file
     :files ("dash.el")
     :path "~/ui/vendor/emacs/dash.el")

    (diminish
     :fetcher file
     :path "~/ui/vendor/emacs/diminish.el")

    (evil
     :fetcher file
     :path "~/ui/vendor/emacs/evil")

    (evil-matchit
     :fetcher file
     :path "~/ui/vendor/emacs/evil-matchit")

    (evil-nerd-commenter
     :fetcher file
     :path "~/ui/vendor/emacs/evil-nerd-commenter")

    (evil-quick-scope
     :fetcher file
     :path "~/gh/evil-quick-scope")

    (evil-surround
     :fetcher file
     :path "~/ui/vendor/emacs/evil-surround")

    (fill-column-indicator
     :fetcher file
     :files ("fill-column-indicator.el")
     :path "~/ui/vendor/emacs/Fill-Column-Indicator")

    (fuzzy
     :fetcher file
     :files ("fuzzy.el")
     :path "~/ui/vendor/emacs/fuzzy-el")

    (goto-chg
     :fetcher file
     :path "~/ui/vendor/emacs/goto-chg")

    (haskell-mode
     :fetcher file
     :path "~/ui/vendor/emacs/haskell-mode")

    (nyan-mode
     :fetcher file
     :files ("nyan-mode.el" "img" "mus")
     :path "~/ui/vendor/emacs/nyan-mode")

    (org-bullets
     :fetcher file
     :path "~/ui/vendor/emacs/org-bullets")

    (ov
     :fetcher file
     :path "~/ui/vendor/emacs/ov.el")

    (package-build
     :fetcher file
     :files ("package-build.el")
     :path "~/ui/vendor/emacs/package-build")

    (popup
     :fetcher file
     :files ("popup.el")
     :path "~/ui/vendor/emacs/popup-el")

    (powerline
     :fetcher file
     :path "~/ui/vendor/emacs/powerline")

    (quelpa
     :fetcher file
     :files ("quelpa.el")
     :path "~/ui/vendor/emacs/quelpa")

    (quelpa-use-package
     :fetcher file
     :path "~/ui/vendor/emacs/quelpa-use-package")

    (rainbow-delimiters
     :fetcher file
     :path "~/ui/vendor/emacs/rainbow-delimiters")

    (rust-mode
     :fetcher file
     :files ("rust-mode.el")
     :path "~/ui/vendor/emacs/rust-mode")

    (s
     :fetcher file
     :files ("s.el")
     :path "~/ui/vendor/emacs/s.el")

    (spaceline
     :fetcher file
     :path "~/ui/vendor/emacs/spaceline")

    (undo-tree
     :fetcher file
     :path "~/ui/vendor/emacs/undo-tree")

    (use-package
     :fetcher file
     :files ("use-package.el")
     :path "~/ui/vendor/emacs/use-package")

    (yasnippet
     :fetcher file
     :files ("yasnippet.el")
     :path "~/ui/vendor/emacs/yasnippet")))

(setq package-archives nil
      quelpa-melpa-recipe-stores (list fi/recipes)
      quelpa-update-melpa-p nil
      quelpa-upgrade-p t)

;; Prevent quelpa from cloning melpa.
(let* ((quelpa-melpa-dir (expand-file-name "~/.emacs.d/quelpa/melpa"))
      (quelpa-melpa-git-location (expand-file-name ".git" quelpa-melpa-dir)))
  (unless (file-exists-p quelpa-melpa-git-location)
    (make-directory quelpa-melpa-dir t)
    ;; Create empty file instead of empty directory because it seems 'cleaner'.
    (write-region "" nil quelpa-melpa-git-location)))

(require 'package)
(package-initialize)

;; Bootstrap Quelpa
;; ================
(unless (require 'package-build nil t)
  (load-file
   (expand-file-name "~/ui/vendor/emacs/package-build/package-build.el")))

(unless (require 'quelpa nil t)
  (load-file
   (expand-file-name "~/ui/vendor/emacs/quelpa/quelpa.el")))

(quelpa 'package-build)
(quelpa 'quelpa)
(quelpa 'use-package)
(quelpa 'quelpa-use-package)

(require 'quelpa-use-package)

;; Libraries
;; =========
(use-package auto-complete :defer t :quelpa)
(use-package bind-key      :defer t :quelpa)
(use-package dash          :defer t :quelpa)
(use-package diminish      :defer t :quelpa)
(use-package fuzzy         :defer t :quelpa)
(use-package goto-chg      :defer t :quelpa)
(use-package org-bullets   :defer t :quelpa)
(use-package ov            :defer t :quelpa)
(use-package popup         :defer t :quelpa)
(use-package powerline     :defer t :quelpa)
(use-package s             :defer t :quelpa)
(use-package undo-tree     :defer t :quelpa)

(use-package spaceline :defer t :quelpa)

;; Built-in Minor Modes
;; ====================
(use-package avoid
  :demand t
  :init
  ;; Moves mouse to the bottom right corner of the emacs window.
  (setq mouse-avoidance-banish-position
        '((frame-or-window . frame)
          (side . right)
          (side-pos . 0)
          (top-or-bottom . bottom)
          (top-or-bottom-pos . 0)))
  :config
  (mouse-avoidance-mode 'banish))

(use-package cus-edit
  :demand t
  :init
  (setq custom-file (expand-file-name "~/ui/emacs/.emacs.d/.custom.el"))
  :config
  (load custom-file))

(use-package files
  :demand t
  :init
  (setq auto-save-default          nil
        backup-directory-alist     `(("." . "~/.backup"))
        backup-by-copying          t
        create-lockfiles           nil
        delete-old-versions        t
        kept-new-versions          6
        kept-old-versions          2
        require-final-newline      t
        safe-local-variable-values '((fi/org-export-to-pdf-on-save . t))
        version-control            t))

(use-package font-lock
  :demand t
  :init
  ;; Fixes the following errors when using emacs daemon (being logged in
  ;; *Messages* buffer):
  ;;
  ;;     Invalid face reference: font-lock-comment-delimiter-face
  ;;     Invalid face reference: font-lock-comment-face [5 times]
  ;;
  (when (daemonp)
    (global-font-lock-mode 0)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (global-font-lock-mode 1))))))

(use-package frame
  :demand t
  :init
  ;; Actually part of window.el but window.el does not provide 'window..
  (setq recenter-positions '(0.2))
  :config
  (blink-cursor-mode 0))

(use-package fringe
  :demand t
  :config
  (fringe-mode '(0 . nil)))

(use-package hl-line
  :demand t
  :config
  (global-hl-line-mode t))

(use-package paren
  :demand t
  :init
  (setq show-paren-delay 0)
  :config
  (show-paren-mode t))

(use-package simple
  :demand t
  :init
  ;; (setq fill-indent-according-to-mode t)
  ;; From `buffer.c'.
  (setq-default auto-fill-function 'do-auto-fill)
  (setq-default tab-width 2)
  ;; From `indent.c'.
  (setq-default indent-tabs-mode nil)
  ;; Enables `gj` and `gk` and co to move up and down visually wrapped lines.
  (setq line-move-visual nil)
  ;; These get disabled for whatever reason.
  (setq-default visual-line-fringe-indicators t)
  :config
  ;; Visually wrap long lines at right window edge.
  (global-visual-line-mode t))

(use-package vc-hooks
  :demand t
  :init
  (setq vc-follow-symlinks t))

(use-package whitespace
  :demand t
  :init
  (setq whitespace-style '(face empty tabs trailing))
  :config
  (global-whitespace-mode t))

;; Evil
;; ====
(use-package evil
  :demand t :quelpa
  :init
  (setq evil-want-C-u-scroll t
        evil-cross-lines t
        evil-shift-width 2
        evil-split-window-below t
        evil-vsplit-window-right t)

  ;; Change default shift-width for some specific modes.
  (add-hook 'python-mode-hook
            (lambda () (setq evil-shift-width python-indent-offset)))

  :preface
  (defun fi/previous-line-advice (&optional CURRENT-COMMAND)
    (evil-previous-line))

  (defun fi/minibuffer-keyboard-quit ()
    "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it. Then it
takes a second \\[keyboard-quit] to abort the minibuffer."
    (interactive)
    (if (and delete-selection-mode transient-mark-mode mark-active)
        (setq deactivate-mark  t)
      (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
      (abort-recursive-edit)))

  (defun fi/exit-evil-command-window ()
    "Exit evil command window."
    (interactive)
    ;; For whatever reason, moving to the previous window and back again means
    ;; after quitting this command window, it will properly go back to the
    ;; previous window. Otherwise it seems to go to some other unintuitive
    ;; window.
    (other-window -1)
    (other-window 1)
    (kill-this-buffer)
    (evil-window-delete))

  (defun fi/evil-yank-to-end-of-line ()
    "Yank from cursor position to end of line."
    (interactive)
    (evil-yank (point) (point-at-eol)))

  (defun fi/insert-two-spaces ()
    (interactive)
    (insert "  "))

  (defun fi/insert-one-space ()
    (interactive)
    (insert " "))

  (defun fi/evil-edit-dot-emacs ()
    "Edit emacs init file."
    (interactive)
    (evil-edit "~/ui/emacs/.emacs.d/init.el"))

  (defun fi/delete-whitespace-around-point ()
    (interactive)
    (just-one-space 0))

  :config
  (evil-declare-ignore-repeat 'recenter-top-bottom)

  ;; Disable hl-line-mode in evil visual state.
  (add-hook 'evil-visual-state-entry-hook
            (lambda () (setq-local global-hl-line-mode nil)))
  (add-hook 'evil-visual-state-exit-hook
            (lambda () (setq-local global-hl-line-mode t)))

  ;; Move up one line when doing q: and q/.
  (advice-add 'evil-command-window-ex :after #'fi/previous-line-advice)
  (advice-add
   'evil-command-window-search-forward :after #'fi/previous-line-advice)

  ;; Escape quits emacs things as a vim user would expect.
  (define-key evil-motion-state-map [escape] 'keyboard-quit)
  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'fi/minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'fi/minibuffer-keyboard-quit)
  (define-key
    minibuffer-local-completion-map [escape] 'fi/minibuffer-keyboard-quit)
  (define-key
    minibuffer-local-must-match-map [escape] 'fi/minibuffer-keyboard-quit)
  (define-key
    minibuffer-local-isearch-map [escape] 'fi/minibuffer-keyboard-quit)
  (global-set-key [escape] 'evil-exit-emacs-state)
  (evil-define-key
    'normal evil-command-window-mode-map [escape] 'fi/exit-evil-command-window)

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

  (define-key
    evil-motion-state-map (vconcat "z" [return]) 'evil-scroll-line-to-top)
  (define-key
    evil-normal-state-map (vconcat "z" [return]) 'evil-scroll-line-to-top)
  (define-key
    evil-visual-state-map (vconcat "z" [return]) 'evil-scroll-line-to-top)

  (define-key evil-motion-state-map "+" 'text-scale-increase)
  (define-key evil-normal-state-map "+" 'text-scale-increase)
  (define-key evil-motion-state-map "-" 'text-scale-decrease)
  (define-key evil-normal-state-map "-" 'text-scale-decrease)
  (define-key evil-motion-state-map "'" 'evil-ex)
  (define-key evil-normal-state-map "'" 'evil-ex)
  (define-key evil-visual-state-map "'" 'evil-ex)
  (define-key evil-normal-state-map "gs" 'evil-write)
  (define-key evil-motion-state-map "Y"  'fi/evil-yank-to-end-of-line)
  (define-key evil-normal-state-map "Y"  'fi/evil-yank-to-end-of-line)
  (define-key evil-normal-state-map (kbd "C-SPC") 'fi/insert-two-spaces)
  (define-key evil-insert-state-map (kbd "C-SPC") 'fi/insert-two-spaces)
  (define-key evil-normal-state-map (kbd "M-SPC") 'fi/insert-one-space)
  (define-key evil-insert-state-map (kbd "M-SPC") 'fi/insert-one-space)

  ;; Leader layer.
  ;; Required for the following motion state maps that start with space.
  (define-key evil-motion-state-map " " nil)

  (define-key evil-motion-state-map (kbd "SPC SPC") 'recenter-top-bottom)
  (define-key evil-normal-state-map (kbd "SPC SPC") 'recenter-top-bottom)
  (define-key evil-motion-state-map (kbd "SPC ei")  'fi/evil-edit-dot-emacs)
  (define-key evil-normal-state-map (kbd "SPC ei")  'fi/evil-edit-dot-emacs)
  (define-key evil-normal-state-map (kbd "SPC ds")  'just-one-space)
  (define-key evil-normal-state-map
    (kbd "SPC dw") 'fi/delete-whitespace-around-point)
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
  (define-key evil-normal-state-map "gcc" 'evilnc-comment-or-uncomment-lines)
  (define-key evil-visual-state-map "gc" 'comment-or-uncomment-region)
  (evil-mode t))

(use-package evil-matchit
  :quelpa :config (global-evil-matchit-mode t))

(use-package evil-nerd-commenter
  :quelpa :commands evilnc-comment-or-uncomment-lines)

(use-package evil-quick-scope
  :disabled t :quelpa
  :config
  (defun turn-on-evil-quick-scope-mode ()
    "Unconditionally turn on evil-quick-scope-mode."
    (evil-quick-scope-mode t))

  (define-globalized-minor-mode fi/global-evil-quick-scope-mode
    evil-quick-scope-mode turn-on-evil-quick-scope-mode)

  (fi/global-evil-quick-scope-mode t))

(use-package evil-surround
  :quelpa :config (global-evil-surround-mode t))

;; Other Third Party Minor Modes
;; =============================
(use-package auto-complete-config
  :demand t :quelpa
  :init
  (setq ac-auto-show-menu 0.4
        ac-auto-start 3
        ac-candidate-menu-min 0
        ac-delay 0.2
        ac-disable-inline t
        ac-show-menu-immediately-on-auto-complete t
        ac-use-menu-map t
        ac-use-quick-help nil)

  :preface
  ;; Disable fci-mode during auto-complete popups to stop buggy display of
  ;; popups.
  (defun sanityinc/fci-enabled-p ()
    (and (boundp 'fci-mode) fci-mode))

  (defvar sanityinc/fci-mode-suppressed nil)

  (defadvice popup-create (before suppress-fci-mode activate)
    "Suspend fci-mode while popups are visible"
    (let ((fci-enabled (sanityinc/fci-enabled-p)))
      (when fci-enabled
        (set (make-local-variable 'sanityinc/fci-mode-suppressed) fci-enabled)
        (turn-off-fci-mode))))

  (defadvice popup-delete (after restore-fci-mode activate)
    "Restore fci-mode when all popups have closed"
    (when (and sanityinc/fci-mode-suppressed
               (null popup-instances))
      (setq sanityinc/fci-mode-suppressed nil)
      (turn-on-fci-mode)))

  :config
  (add-to-list
   'ac-dictionary-directories "~/ui/vendor/emacs/auto-complete/dict")
  (add-to-list 'ac-modes 'org-mode)
  (ac-config-default)

  (define-key ac-completing-map "\r" nil)
  (define-key ac-menu-map "\r" nil)
  (define-key ac-menu-map (kbd "S-<iso-lefttab>") 'ac-previous)
  (evil-define-key 'insert ac-menu-map (kbd "C-p") 'ac-previous)
  (evil-define-key 'insert ac-menu-map (kbd "C-n") 'ac-next)
  (evil-define-key 'insert ac-completing-map "\t" 'ac-complete))

(use-package fill-column-indicator
  :demand t :quelpa
  :init (setq-default fill-column 80)
  :config
  (define-globalized-minor-mode fi/global-fci-mode fci-mode turn-on-fci-mode)
  (fi/global-fci-mode t))

(use-package nyan-mode
  :demand t :quelpa
  :config (nyan-mode t))

(use-package rainbow-delimiters
  :defer t :quelpa
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package spaceline-config
  :demand t :quelpa
  :init
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  :config
  (spaceline-spacemacs-theme)
  (setq spaceline-buffer-size-p nil
        spaceline-minor-modes-p nil
        spaceline-nyan-cat-p t))

(use-package yasnippet
  :disabled t :quelpa
  :init
  (setq yas-snippet-dirs '("~/ui/vendor/emacs/yasnippet-snippets"))
  :config
  (yas-global-mode t)

  (evil-define-key 'insert yas-minor-mode-map (kbd "C-s") 'yas-expand)
  (evil-define-key 'insert yas-minor-mode-map (kbd "C-n") 'yas-next-field)
  (evil-define-key 'insert yas-minor-mode-map (kbd "C-p") 'yas-prev-field)

  ;; Disable default mappings.
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil))

;; Built-in Major Modes
;; ====================
(use-package erc
  :commands erc
  :init
  (setq erc-header-line-format nil)
  :config
  (add-hook 'erc-mode-hook (lambda () (setq-local auto-fill-function nil))))

(use-package ibuffer
  :commands ibuffer
  :config
  (evil-set-initial-state 'ibuffer-mode 'motion)

  ;; Do not visually wrap long lines for ibuffer because chances are there is a
  ;; long filepath open, but we don't want it to be wrapped because that messes
  ;; with the display of the buffers, which is more important.
  (add-hook 'ibuffer-mode-hook (lambda () (visual-line-mode nil)))

  ;; Use human readable size column instead of original one.
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (cond
     ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
     ((> (buffer-size) 100000)  (format "%7.0fk" (/ (buffer-size) 1000.0)))
     ((> (buffer-size) 1000)    (format "%7.1fk" (/ (buffer-size) 1000.0)))
     (t                         (format "%8d"       (buffer-size)))))

  (setq ibuffer-formats
        '((mark modified read-only " " (name 18 18 :left :elide) " "
                (size-h 9 -1 :right) "  " (mode 16 16 :left :elide) " "
                filename-and-process)))

  (evil-define-key 'motion ibuffer-mode-map
    (kbd "h") 'evil-backward-char
    (kbd "l") 'evil-forward-char
    (kbd "0") 'digit-argument
    (kbd "1") 'digit-argument
    (kbd "2") 'digit-argument
    (kbd "3") 'digit-argument
    (kbd "4") 'digit-argument
    (kbd "5") 'digit-argument
    (kbd "6") 'digit-argument
    (kbd "7") 'digit-argument
    (kbd "8") 'digit-argument
    (kbd "9") 'digit-argument

    (kbd "m") 'ibuffer-mark-forward
    (kbd "SPC t") 'ibuffer-toggle-marks
    (kbd "u") 'ibuffer-unmark-forward
    (kbd "=") 'ibuffer-diff-with-file
    (kbd "J") 'ibuffer-jump-to-buffer
    ;; (kbd "M-s a C-s") 'ibuffer-do-isearch
    ;; (kbd "M-s a M-C-s") 'ibuffer-do-isearch-regexp
    ;; (kbd "M-s a C-o") 'ibuffer-do-occur
    (kbd "DEL") 'ibuffer-unmark-backward
    (kbd "* *") 'ibuffer-unmark-all
    (kbd "* M") 'ibuffer-mark-by-mode
    (kbd "* m") 'ibuffer-mark-modified-buffers
    (kbd "* u") 'ibuffer-mark-unsaved-buffers
    (kbd "* s") 'ibuffer-mark-special-buffers
    (kbd "* r") 'ibuffer-mark-read-only-buffers
    (kbd "* /") 'ibuffer-mark-dired-buffers
    (kbd "* e") 'ibuffer-mark-dissociated-buffers
    (kbd "* h") 'ibuffer-mark-help-buffers
    (kbd "* z") 'ibuffer-mark-compressed-file-buffers
    (kbd ".") 'ibuffer-mark-old-buffers

    (kbd "d") 'ibuffer-mark-for-delete
    (kbd "x") 'ibuffer-do-kill-on-deletion-marks

    ;; Immediate operations.
    (kbd "C-f") 'ibuffer-forward-next-marked
    (kbd "C-s") 'ibuffer-backwards-next-marked
    (kbd "g") 'ibuffer-update
    "`" 'ibuffer-switch-format
    "-" 'ibuffer-add-to-tmp-hide
    "+" 'ibuffer-add-to-tmp-show
    "D" 'ibuffer-bury-buffer
    (kbd "s t") 'ibuffer-toggle-sorting-mode
    (kbd "s i") 'ibuffer-invert-sorting
    (kbd "s a") 'ibuffer-do-sort-by-alphabetic
    (kbd "s v") 'ibuffer-do-sort-by-recency
    (kbd "s s") 'ibuffer-do-sort-by-size
    (kbd "s f") 'ibuffer-do-sort-by-filename/process
    (kbd "s m") 'ibuffer-do-sort-by-major-mode

    (kbd "SPC f m") 'ibuffer-filter-by-used-mode
    (kbd "SPC f M") 'ibuffer-filter-by-derived-mode
    (kbd "SPC f n") 'ibuffer-filter-by-name
    (kbd "SPC f c") 'ibuffer-filter-by-content
    (kbd "SPC f e") 'ibuffer-filter-by-predicate
    (kbd "SPC f f") 'ibuffer-filter-by-filename
    (kbd "SPC f >") 'ibuffer-filter-by-size-gt
    (kbd "SPC f <") 'ibuffer-filter-by-size-lt
    (kbd "SPC f r") 'ibuffer-switch-to-saved-filters
    (kbd "SPC f a") 'ibuffer-add-saved-filters
    (kbd "SPC f x") 'ibuffer-delete-saved-filters
    (kbd "SPC f d") 'ibuffer-decompose-filter
    (kbd "SPC f s") 'ibuffer-save-filters
    (kbd "SPC f p") 'ibuffer-pop-filter
    (kbd "SPC f !") 'ibuffer-negate-filter
    (kbd "SPC f t") 'ibuffer-exchange-filters
    (kbd "SPC f TAB") 'ibuffer-exchange-filters
    (kbd "SPC f o") 'ibuffer-or-filter
    (kbd "SPC f g") 'ibuffer-filters-to-filter-group
    (kbd "SPC f P") 'ibuffer-pop-filter-group
    (kbd "SPC f D") 'ibuffer-decompose-filter-group
    (kbd "SPC f /") 'ibuffer-filter-disable

    (kbd "C-n") 'ibuffer-forward-filter-group
    (kbd "C-p") 'ibuffer-backward-filter-group
    (kbd "SPC j") 'ibuffer-jump-to-filter-group
    (kbd "C-y") 'ibuffer-yank
    (kbd "SPC f S") 'ibuffer-save-filter-groups
    (kbd "SPC f R") 'ibuffer-switch-to-saved-filter-groups
    (kbd "SPC f X") 'ibuffer-delete-saved-filter-groups
    (kbd "SPC f \\") 'ibuffer-clear-filter-groups

    [escape] 'ibuffer-quit
    (kbd "q") 'ibuffer-quit
    "SPC ." 'describe-mode

    (kbd "SPC r n") 'ibuffer-mark-by-name-regexp
    (kbd "SPC r m") 'ibuffer-mark-by-mode-regexp
    (kbd "SPC r f") 'ibuffer-mark-by-file-name-regexp

    (kbd "C-t") 'ibuffer-visit-tags-table

    (kbd "|") 'ibuffer-do-shell-command-pipe
    (kbd "!") 'ibuffer-do-shell-command-file
    (kbd "~") 'ibuffer-do-toggle-modified

    ;; Marked operations.
    (kbd "A") 'ibuffer-do-view
    (kbd "D") 'ibuffer-do-delete
    ;; (kbd "E") 'ibuffer-do-eval
    ;; (kbd "F") 'ibuffer-do-shell-command-file
    (kbd "I") 'ibuffer-do-query-replace-regexp
    (kbd "N") 'ibuffer-do-shell-command-pipe-replace
    (kbd "O") 'ibuffer-do-occur
    (kbd "P") 'ibuffer-do-print
    (kbd "Q") 'ibuffer-do-query-replace
    (kbd "R") 'ibuffer-do-rename-uniquely
    (kbd "S") 'ibuffer-do-save
    ;; (kbd "T") 'ibuffer-do-toggle-read-only
    (kbd "U") 'ibuffer-do-replace-regexp
    ;; (kbd "V") 'ibuffer-do-revert
    ;; (kbd "W") 'ibuffer-do-view-and-eval
    (kbd "X") 'ibuffer-do-shell-command-pipe

    ;; 'ibuffer-do-kill-lines is the function deleted here.
    (kbd "SPC c") 'ibuffer-copy-filename-as-kill

    (kbd "RET") 'ibuffer-visit-buffer
    ;; (kbd "C-x C-f") 'ibuffer-find-file
    ;; (kbd "C-o") 'ibuffer-visit-buffer-other-window-noselect
    ;; (kbd "M-o") 'ibuffer-visit-buffer-1-window
    ;; (kbd "C-x v") 'ibuffer-do-view-horizontally
    ;; (kbd "C-c C-a") 'ibuffer-auto-mode
    ;; (kbd "C-x 4 RET") 'ibuffer-visit-buffer-other-window
    ;; (kbd "C-x 5 RET") 'ibuffer-visit-buffer-other-frame)
    (kbd "o") 'ibuffer-visit-buffer-other-window))

(use-package org
  :defer t
  :init
  ;; NOTE: This :init form is special in that "/emacs/ox.el" evaluates it for
  ;; asynchronous export purposes. Therefore it is wrapped in a `progn' and it
  ;; *probably* should not use variables defined from elsewhere, because they
  ;; would be undefined in ox.el.
  (progn
    (unless (boundp 'org-latex-classes)
      (setq org-latex-classes nil))
    (add-to-list
     'org-latex-classes
     '("article"
       "\\documentclass[a4paper,12pt]{article}
[NO-DEFAULT-PACKAGES]
[NO-PACKAGES]
\\usepackage{fontspec}
\\usepackage{fixltx2e}
\\usepackage{graphicx}
\\usepackage{longtable}
\\usepackage{float}
\\usepackage{wrapfig}
\\usepackage{rotating}
\\usepackage[normalem]{ulem}
\\usepackage{amsmath}
\\usepackage{marvosym}
\\usepackage{wasysym}
\\usepackage{amssymb}

\\usepackage{grffile}
\\usepackage[dvipsnames,svgnames]{xcolor}
\\usepackage{lastpage}
\\usepackage{fancyhdr}
\\usepackage{parskip}
\\usepackage{tocloft}
\\usepackage{sectsty}
\\usepackage{titling}
\\usepackage{titlesec}
\\usepackage{enumitem}
% \\usepackage[object=vectorian]{pgfornament}
\\usepackage{tikz}
\\usepackage{array}
\\usepackage{tabularx}
\\usepackage{calc}
\\usepackage{colortbl}

\\usepackage{hyperref}

[EXTRA]

\\tolerance=1000

\\defaultfontfeatures{Ligatures=TeX}

% % For when pgfornament becomes available... :(
%
% \\newcommand{\\sectionlinetwo}[2]{
%   \\nointerlineskip \\vspace{.5\\baselineskip}\\hspace{\\fill}
%   {\\resizebox{0.5\\linewidth}{1.2ex}{\\pgfornament[color = #1]{#2}}}
%     \\hspace{\\fill}\\par\\nointerlineskip \\vspace{.5\\baselineskip}}

% \\newcommand{\\sectionlinetwo}[2]{%
%   \\nointerlineskip \\vspace{.5\\baselineskip}\\hspace{\\fill}
%   {\\color{#1}
%     \\resizebox{0.5\\linewidth}{2ex}
%     {{%
%     {\\begin{tikzpicture}
%     \\node  (C) at (0,0) {};
%     \\node (D) at (9,0) {};
%     \\path (C) to [ornament=#2] (D);
%     \\end{tikzpicture}}}}}%
%     \\hspace{\\fill}
%     \\par\\nointerlineskip \\vspace{.5\\baselineskip}
%   }

\\setmainfont{GaramondNo8}
\\setmonofont{Input}
\\newfontfamily\\titlefont{IM FELL DW Pica}
\\newfontfamily\\headingnumberfont{EB Garamond}
\\renewcommand{\\maketitlehooka}{\\Huge\\titlefont}
\\renewcommand{\\maketitlehookb}{\\large\\itshape\\color{DimGray}}
\\renewcommand{\\maketitlehookc}{\\small\\upshape\\color{gray}}

\\renewcommand{\\cfttoctitlefont}{\\Large}
\\renewcommand{\\cftchapfont}{\\large\\bfseries}
\\renewcommand{\\cftsecfont}{\\bfseries}
\\renewcommand{\\cftsubsecfont}{\\headingnumberfont}
\\renewcommand{\\cftsubsubsecfont}{\\cftsubsecfont}
\\renewcommand{\\cftparafont}{\\cftsubsecfont}
\\renewcommand{\\cftsubparafont}{\\cftsubsecfont}
\\renewcommand{\\cftsubsecpagefont}{\\footnotesize}
\\renewcommand{\\cftsubsubsecpagefont}{\\cftsubsecpagefont}
\\renewcommand{\\cftparapagefont}{\\cftsubsecpagefont}
\\renewcommand{\\cftsubparapagefont}{\\cftsubsecpagefont}
\\renewcommand\\cftaftertoctitle{\\par\\noindent{\\color{lightgray}
\\hrulefill}\\vspace{-0.5em}}

\\makeatletter
\\renewcommand\\subsection{\\@startsection{subsection}{2}{\\z@}
  {-3.25ex\\@plus -1ex \\@minus -.2ex}
  {1.5ex \\@plus .2ex}
  {\\large\\bfseries
  \\ifnum\\value{subsection}>1
  \\else \\addtocontents{toc}{\\protect\\addvspace{0.4em}}\\fi}}
\\makeatother

\\makeatletter
\\renewcommand\\paragraph{\\@startsection{paragraph}{4}{\\z@}
  {-2.5ex\\@plus -1ex \\@minus -.25ex}
  {1.25ex \\@plus .25ex}
  {\\normalfont\\normalsize\\bfseries}}
\\makeatother
\\setcounter{secnumdepth}{4}
\\setcounter{tocdepth}{4}

\\makeatletter
\\def\\@seccntformat#1{\\headingnumberfont\\color{gray}
\\llap{\\csname the#1\\endcsname\\quad}}
\\makeatother

\\fancypagestyle{plain}{
  \\renewcommand{\\headrulewidth}{0pt}
  \\fancyhead{}
  \\fancyfoot[C]{\\footnotesize
\\color{DimGray}\\thepage\\ of \\pageref*{LastPage}}
}
\\pagestyle{fancy}
\\renewcommand{\\headrulewidth}{0pt}
\\fancyhead{}
\\fancyfoot[C]{\\footnotesize
\\color{DimGray}\\thepage\\ of \\pageref*{LastPage}}

\\setlength{\\parskip}{3mm plus1mm minus1mm}

\\newcommand{\\sbt}
{\\begin{picture}(-1,1)(-1,-3)\\circle*{3}\\end{picture}\\hspace{0.4em}}
\\setlistdepth{5}
\\setlist[itemize,1]{label=\\textbullet}
\\setlist[itemize,2]{label=\\sbt}
\\setlist[itemize,3]{label=\\textperiodcentered}
\\setlist[itemize,4]{label=\\normalfont\\bfseries\\textendash}
\\setlist[itemize,5]{label=$\\star$}
\\renewlist{itemize}{itemize}{9}

\\newlength{\\conditionwd}
\\newenvironment{conditions}[1][Where:]
  {#1\\tabularx{\\textwidth-\\widthof{#1}}[t]{
     >{$}l<{$} @{${}={}$} X@{}}}
  {\\endtabularx\\\\[\\belowdisplayskip]}

\\arrayrulecolor{lightgray}

\\hypersetup{
  colorlinks = true,
  citecolor  = {YellowOrange!85!black},
  linkcolor  = {darkgray!85!black},
  urlcolor   = {MidnightBlue!85!black}
}"
       ("\\section{%s}" . "\\section*{%s}")
       ("\\subsection{%s}" . "\\subsection*{%s}")
       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
       ("\\paragraph{%s}" . "\\paragraph*{%s}")
       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
    (setq org-adapt-indentation t
          org-bullets-bullet-list '("▣" "◉" "✱" "➤")
          org-catch-invisible-edits 'smart
          org-ellipsis "↴" ; …
          org-entities-ascii-explanatory t
          org-export-async-init-file (expand-file-name "~/ui/emacs/ox.el")
          org-export-headline-levels 4
          org-export-with-smart-quotes t
          org-hide-emphasis-markers t
          org-hide-leading-stars t
          org-latex-pdf-process
          '("sleep 1s;
while [[ $? -eq 0 ]]; do sleep 2s; ps cax | grep -q lualatex; done"
            "lualatex -interaction nonstopmode -output-directory %o %f"
            "lualatex -interaction nonstopmode -output-directory %o %f"
            "lualatex -interaction nonstopmode -output-directory %o %f"
            "mv %o%b.pdf %o.%b.pdf"
            "mv %o%b.tex %o.%b.tex"
            "rm -f %o%b.aux"
            "rm -f %o%b.log"
            "rm -f %o%b.out"
            "rm -f %o%b.toc"
            "rm -f texput.log"
            "rm -f *~")
          org-latex-toc-command "\\tableofcontents
\\vspace{-1em}\\noindent{\\color{lightgray}\\hrulefill}\\\\*\\\\*"
          org-list-allow-alphabetical t
          org-list-use-circular-motion t
          org-pretty-entities t
          org-src-fontify-natively t
          org-src-preserve-indentation t
          org-startup-folded 'nofold
          org-startup-indented t
          org-startup-with-inline-images t))

  :preface
  (defvar-local fi/org-export-to-pdf-on-save nil)
  (defvar-local fi/org-list-item-fill-last-line-number 0)

  (defun fi/org-at-item-p ()
    ;; NOTE: Should probably wrap any calls to this with `save-excursion'.
    (or (org-at-item-p)
        (if (eq nil (condition-case nil
                        (org-beginning-of-item)
                      (error nil)))
            nil
          t)))

  (defun fi/org-set-list-item-fill-prefix ()
    "Sets fill-prefix accordingly so that auto-fill properly
hanging indents long list lines.

Will not work when typing in a new list item from scratch without
using `org-meta-return' though."
    (when (eq 'org-mode major-mode)
      (let ((ln (line-number-at-pos)))
        (when (not (eq fi/org-list-item-fill-last-line-number ln))
          (save-excursion
            (if (fi/org-at-item-p)
                (let ((l (thing-at-point 'line t)))
                  (string-match org-list-full-item-re l)
                  (setq-local
                   fill-prefix
                   (make-string (length (match-string-no-properties 0 l)) ?\s)))
              (setq-local fill-prefix nil))
            (setq-local fi/org-list-item-fill-last-line-number ln))))))

  (defun fi/org-evil-meta-return-above ()
    (interactive)
    (evil-insert-line 0)
    (org-meta-return))

  (defun fi/org-evil-meta-return-below ()
    (interactive)
    (evil-append-line 0)
    (org-meta-return))

  (defun fi/org-evil-insert-heading-respect-content ()
    (interactive)
    (evil-end-of-line)
    (org-insert-heading-respect-content)
    (evil-append 0))

  (defun fi/outline-focus ()
    "'Focus' current level/subtree."
    (interactive)
    (hide-other)
    (save-excursion
      (while (progn
               (show-entry)
               (show-branches)
               (condition-case nil
                   (outline-up-heading 1)
                 (error nil))))))

  (defun fi/outline-next-heading-focus ()
    (interactive)
    (outline-next-heading)
    (fi/outline-focus))

  (defun fi/outline-previous-heading-focus ()
    (interactive)
    (outline-previous-heading)
    (fi/outline-focus))

  (defun fi/org-ret ()
    (interactive)
    (if (or (org-at-heading-p)
            (save-excursion (fi/org-at-item-p)))
        (org-meta-return)
      (evil-ret)))

  :config
  (add-hook
   'org-mode-hook
   (lambda ()
     (progn
       (turn-off-fci-mode)
       (setq-local line-spacing '0.2)
       (setq-local paragraph-start "\f\\|[ 	]*$")
       (setq-local paragraph-separate "[ 	\f]*$")
       (face-remap-add-relative 'default :height 1.2)
       (face-remap-add-relative 'org-document-title :height 1.2)
       (face-remap-add-relative 'org-level-1 :height 1.1)
       (face-remap-add-relative 'org-level-2 :height 1.05)
       (org-bullets-mode 1))))

  (add-hook 'post-command-hook 'fi/org-set-list-item-fill-prefix)

  (add-hook
   'after-save-hook
   (lambda ()
     (when (and (eq 'org-mode major-mode)
                fi/org-export-to-pdf-on-save)
       (org-latex-export-to-pdf t))))

  ;; Prettier unordered list item bullets.
  (font-lock-add-keywords
   'org-mode
   '(("^ *\\([-]\\) "
      (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  (font-lock-add-keywords
   'org-mode
   '(("^ *\\([+]\\) "
      (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "◦"))))))

  (font-lock-add-keywords
   'org-mode
   '(("^ +\\([*]\\) "
      (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "∙"))))))

  (evil-define-key 'insert org-mode-map
    (kbd "M-h") 'org-metaleft
    (kbd "M-j") 'org-metadown
    (kbd "M-k") 'org-metaup
    (kbd "M-l") 'org-metaright
    (kbd "M-H") 'org-shiftmetaleft
    (kbd "M-L") 'org-shiftmetaright
    (kbd "<return>") 'fi/org-ret
    (kbd "<S-return>") 'evil-ret)

  (evil-define-key 'normal org-mode-map
    (kbd "K") 'org-shiftright
    (kbd "M-h") 'org-metaleft
    (kbd "M-j") 'org-metadown
    (kbd "M-k") 'org-metaup
    (kbd "M-l") 'org-metaright
    (kbd "M-H") 'org-shiftmetaleft
    (kbd "M-L") 'org-shiftmetaright
    (kbd "M-i") 'outline-next-heading
    (kbd "M-o") 'outline-previous-heading
    (kbd "M-I") 'fi/outline-next-heading-focus
    (kbd "M-O") 'fi/outline-previous-heading-focus
    (kbd "<return>") 'fi/org-evil-meta-return-below
    (kbd "<C-return>") 'fi/org-evil-insert-heading-respect-content
    (kbd "<S-return>") 'fi/org-evil-meta-return-above
    (kbd "SPC cl") 'org-content
    (kbd "SPC co") 'fi/outline-focus
    (kbd "SPC cx") 'show-all
    (kbd "SPC te") 'org-toggle-pretty-entities
    (kbd "SPC ti") 'org-toggle-inline-images
    (kbd "SPC tl") 'org-toggle-link-display))

;; Third Party Major Modes
;; =======================
(use-package haskell-mode
  :defer t :quelpa
  :init
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indent))

(use-package rust-mode
  :defer t :quelpa
  :mode ("\\.rs\\'" . rust-mode))

;; Theme
;; _____
;;
(load-file "~/.emacs.d/lisp/theme.el")

;; Faces
;; _____
;;
(set-face-attribute 'mode-line nil :box nil)
(set-face-attribute 'mode-line-inactive nil :box nil)

;; Disable underlines.
(mapc (lambda (face)
        (set-face-attribute face nil :underline nil))
      (face-list))

;; Font
;; ____
;;
(add-to-list 'default-frame-alist '(font . "Input-9"))
