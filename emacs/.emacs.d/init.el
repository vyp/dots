;; Startup
;; =======
;;
;; Code that configures the Emacs startup sequence.

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

;; Init Organization - use-package
;; ===============================
;;
;; The goal is to have as much init file code as possible encapsulated in
;; use-package forms. Therefore it's the next most important package to
;; install.
(straight-use-package 'use-package)
(setq use-package-always-ensure t)

;; Exposed Code
;; ============
;;
;; Rest of all the code that doesn't fit in any use-package forms (except for
;; the code under the Theme and Font sections)... mainly because they're
;; defined in C source code and therefore are not from any provided features to
;; appropriately wrap under.

;;; buffer.c
(setq-default fill-column 79
              tab-width   2)

;;; indent.c
;; Do not insert tabs when indenting.
(setq-default indent-tabs-mode nil)

;;; xdisp.c
;; Vertical cursor padding.
(setq scroll-margin 5
      scroll-step   1)

;; Third Party Libraries
;; =====================

;; Keybinding Related
;; ------------------
;; Because Emacs relies so heavily on keyboard shortcuts for it's usage,
;; keybinding configuration is likely going to be in a lot of places.
;; Therefore, any additional features that provide functionality for
;; configuring keybindings should be 'demanded' for the init file itself, I
;; think.
(use-package general :demand t)
(use-package hydra   :demand t)

;; Minor Modes
;; ===========
;;
;; Minor modes are usually activated over a variety of buffer types, and are
;; commonly even global. Hence it makes sense to configure them next.

;; Built-in
;; --------
(use-package cus-edit
  :demand t
  :ensure nil
  :init
  (setq custom-file (expand-file-name "~/dots/emacs/custom.el"))
  :config
  (load custom-file))

(use-package files
  :ensure nil
  :init
  (setq auto-save-default     nil
        ;; Pretty much only use one instance of Emacs at a time anyway (or use
        ;; Emacs server), so lockfiles to prevent editing collisions are almost
        ;; always unnecessary.
        create-lockfiles      nil
        make-backup-files     nil
        require-final-newline t))

(use-package simple
  :demand t
  :ensure nil
  :init
  (setq-default auto-fill-function 'do-auto-fill)
  ;; Make `gj`/`gk` work across visually wrapped lines.
  (setq-default line-move-visual nil)
  ;; Show fringe indicators for visually wrapped lines.
  (setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
  :config
  ;; Visually wrap long lines.
  (global-visual-line-mode t))

(use-package vc-hooks
  :ensure nil
  :init
  (setq vc-follow-symlinks t))

(use-package whitespace
  :demand t
  :ensure nil
  :init
  (setq whitespace-style '(face empty tabs trailing))
  :config
  (global-whitespace-mode t))

;; Thirdy Party
;; ------------
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

;; Major Modes
;; ===========

;; Built-in
;; --------

;; Thirdy Party
;; ------------
(use-package circe
  :init
  (setq lui-scroll-behavior nil))

;; Font
;; ====
(add-to-list 'default-frame-alist '(font . "Input-9"))
