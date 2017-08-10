;; Startup
;; =======
;;
;; Code that doesn't fit in any use-package form (except for code under Theme
;; and Font sections)... mainly only code that configures the Emacs startup
;; sequence.

;; Prevent Emacs from creating "~/.emacs.d/auto-save-list" directory on
;; startup.
(setq auto-save-list-file-prefix nil)

;; Bootstrap Primary Elisp Package Manager - straight.el
;; =====================================================
;;
;; Since the majority of Emacs packages I use are pure elisp, i.e. do not
;; require any other external dependencies (apart from potentially other elisp
;; packages, which may not be pure elisp), I consider it acceptable (for now)
;; to use a package manager separate from Nix to manage them.
;;
;; The main reason I want to do this is because it is more straightforward to
;; update packages compared to using Nix, where you would have to generate a
;; completely new set of Nix expressions for each Emacs package, on every
;; update.
;;
;; "Non-pure" Emacs packages would of course still be managed with Nix though.

;; Taken from: https://github.com/raxod502/straight.el#getting-started
(let ((bootstrap-file (concat user-emacs-directory "straight/bootstrap.el"))
      (bootstrap-version 2))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         (concat "https://raw.githubusercontent.com/raxod502/straight.el/"
                 "develop/install.el")
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Feature Organization Helper - use-package
;; =========================================
;;
;; use-package is a macro that helps you keep a modular configuration for the
;; features that you use and customize, which also results in minimal Emacs
;; startup time.
;;
;; It is the next most important package to install as the goal is to have
;; everything in the init file be encapsulated in use-package forms.
(straight-use-package 'use-package)
(setq use-package-always-ensure t)

;; Libraries
;; =========
;;
;; Declare any libraries that may be used in the init file itself so that they
;; are available to use.

;; Keybinding Related Libraries
;; ----------------------------
;;
;; Because Emacs relies so heavily on keyboard shortcuts for it's usage,
;; keybinding configuration is likely going to be in a lot of places.
;; Therefore, any additional third party packages that provide functionality
;; for configuring keybindings should be installed next in the init file.

;; :demand t allows :general keyword to be used in use-package forms.
(use-package general :demand t)
(use-package hydra   :defer  t)

;; Evil Mode
;; =========
;;
;; Evil mode is a special keybinding related package that brings Vim-like
;; modality to Emacs as a global minor mode. As it is global and also heavily
;; involved with configuring keybindings, it should be installed and configured
;; next.
(use-package evil
  :demand t
  :init
  (setq evil-cross-lines         t
        evil-shift-width         2
        evil-split-window-below  t
        evil-vsplit-window-right t
        evil-want-C-u-scroll     t)
  :config
  (evil-mode t))

;; Remaining Minor Modes
;; =====================
;;
;; Minor modes are usually activated over a variety of buffer types, and are
;; commonly even global. Hence it makes sense to configure them next.

;; Built-in
;; --------
(use-package cus-edit
  :ensure nil
  :demand t
  :init
  (setq custom-file (expand-file-name "~/dots/emacs/custom.el"))
  :config
  (load custom-file))

;; Thirdy Party
;; ------------

;; Major Modes
;; ===========

;; Built-in
;; --------

;; Thirdy Party
;; ------------
(use-package circe)

;; Font
;; ====
(add-to-list 'default-frame-alist '(font . "Input-9"))
