;; Other Fonts
;; ===========

;; Main font is set in `early-init.el`.
(set-fontset-font "fontset-default" 'unicode "Noto Color Emoji")
(set-face-font 'variable-pitch "Noto Sans-11")

;; Theme
;; =====
(when (display-graphic-p)
  (add-to-list 'custom-theme-load-path "~/dl/repos/moe-theme.el")
  (load-theme 'moe-light 'no-confirm))

;; Vanilla Options
;; ===============
(setq-default auto-fill-function 'do-auto-fill
              fill-column 80
              ;; Do not insert tabs when indenting.
              indent-tabs-mode nil
              ;; Make `gj`/`gk` work across visually wrapped lines.
              line-move-visual nil
              ;; Vertical cursor padding.
              scroll-margin 5
              scroll-step 1
              tab-width 2)

;; Prevent Emacs from creating "~/.emacs.d/auto-save-list" directory on
;; startup.
(setq auto-save-default nil
      auto-save-list-file-prefix nil
      create-lockfiles nil
      custom-file (expand-file-name "~/dots/emacs/custom.el")
      ;; TODO: Change this to make backup files at ~/archive/backup.
      make-backup-files nil
      require-final-newline t
      show-paren-delay 0
      vc-follow-symlinks t
      ;; Show fringe indicators for visually wrapped lines.
      visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow)
      whitespace-line-column fill-column
      whitespace-style '(face empty lines-tail tabs trailing))

(load-file custom-file)
(blink-cursor-mode -1)
(show-paren-mode t)

;; Essential Packages
;; ==================
;;
;; These are essential packages that affect the rest of the init configuration,
;; and therefore need to be loaded early.

(setq straight-use-package-by-default t
      use-package-always-defer t)

;; https://github.com/raxod502/straight.el#getting-started
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
                         user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         (concat "https://raw.githubusercontent.com/raxod502/straight.el/"
                 "develop/install.el")
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(use-package general :demand t)
(load-file (expand-file-name "evil.el" user-emacs-directory))

;; Mode Line
;; =========
(use-package telephone-line
  :demand t
  :init
  (setq telephone-line-primary-left-separator 'telephone-line-flat
        telephone-line-primary-right-separator 'telephone-line-flat
        telephone-line-secondary-left-separator 'telephone-line-flat
        telephone-line-secondary-right-separator 'telephone-line-flat)
  :config
  (telephone-line-mode 1))

;; Buffers
;; =======

;; Lisp Languages
;; ==============
(load-file
 (expand-file-name "fix-calculate-lisp-indent.el" user-emacs-directory))

(use-package lispy
  :ghook 'emacs-lisp-mode-hook)

(use-package lispyville
  :after (evil lispy)
  :ghook 'lispy-mode-hook
  :init
  (setq lispyville-motions-put-into-special t)

  :config
  (lispyville-set-key-theme
   '(operators c-w prettify (atom-motions t) slurp/barf-cp escape))

  :general
  ('normal lispyville-mode-map
           "gc" #'lispyville-comment-or-uncomment
           "gy" #'lispyville-comment-and-clone-dwim
           "M-p" #'lispyville-wrap-round
           "M-[" #'lispyville-wrap-brackets
           "M-b" #'lispyville-wrap-braces)

  ('motion lispyville-mode-map
           "[" #'lispyville-previous-opening
           "]" #'lispyville-next-closing
           "{" #'lispyville-next-opening
           "}" #'lispyville-previous-closing
           "(" #'lispyville-backward-up-list
           ")" #'lispyville-up-list))

;; Not necessarily only for lisp languages but main use case is for lisp
;; languages where this really helps.
(use-package rainbow-delimiters)

;; Text and Prog Mode Hooks
;; ========================
;;
;; I find this a little cleaner and more 'declarative' than having multiple
;; `add-hook's around in various places.
;;
;; The idea is taken from: https://emacs.stackexchange.com/a/5384

(defun my/text-mode-hook ()
  (hl-line-mode)
  (visual-line-mode)
  (whitespace-mode 1))

(defun my/prog-mode-hook ()
  (my/text-mode-hook)
  (rainbow-delimiters-mode 1))

(add-hook 'text-mode-hook 'my/text-mode-hook)
(add-hook 'prog-mode-hook 'my/prog-mode-hook)
