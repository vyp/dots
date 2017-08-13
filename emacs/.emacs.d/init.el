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

;; Exposed Code
;; ============
;;
;; Rest of all the code that doesn't fit in any use-package forms (except for
;; the code under the Hooks, Theme and Font sections).

;; Options from C Code
;; -------------------
;; Code for configuring options provided by C source code which cannot be
;; placed under any appropriate use-package form (because C code doesn't
;; provide features like elisp does).

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

;; Personal
;; --------
(defun my/x-urgency-hint (frame arg &optional source)
  "Set the x-urgency hint for the frame to arg:

- If arg is nil, unset the urgency.
- If arg is any other value, set the urgency."
  (let* ((wm-hints (append
                    (x-window-property "WM_HINTS" frame "WM_HINTS"
                                       source nil t)
                    nil))
         (flags (car wm-hints)))
    (setcar wm-hints
            (if arg
                (logior flags #x00000100)
              (logand flags #x1ffffeff)))
    (x-change-window-property "WM_HINTS" wm-hints frame "WM_HINTS" 32 t)))

(defun my/x-urgent (&optional arg)
  "Mark the current emacs frame as requiring urgent attention.

With a prefix argument which does not equal a boolean value of nil, remove the
urgency flag."
  (interactive "P")
  (let (frame (car (car (cdr (current-frame-configuration)))))
    (my/x-urgency-hint frame (not arg))))

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

(use-package frame
  :demand t
  :ensure nil
  :config
  (blink-cursor-mode -1))

(use-package menu-bar
  :demand t
  :ensure nil
  :config
  (menu-bar-mode -1))

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

(use-package scroll-bar
  :demand t
  :ensure nil
  :config
  (toggle-scroll-bar -1))

(use-package tool-bar
  :demand t
  :ensure nil
  :config
  (tool-bar-mode -1))

(use-package vc-hooks
  :ensure nil
  :init
  (setq vc-follow-symlinks t))

(use-package whitespace
  :ensure nil
  :init
  (setq whitespace-style '(face empty tabs trailing)))

;; Third Party
;; -----------
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

;; Hooks
;; =====
;;
;; Now that all minor modes have hopefully been loaded or autoloaded, we can
;; setup their hooks (as in, if they're not already globally enabled).
;;
;; I find this a little cleaner and more 'declarative' than having multiple
;; `add-hook's around in various use-package forms.
;;
;; The idea is taken from: https://emacs.stackexchange.com/a/5384

(defun my/text-mode-hook ()
  (whitespace-mode 1))

(defun my/prog-mode-hook ()
  (whitespace-mode 1))

(add-hook 'text-mode-hook 'my/text-mode-hook)
(add-hook 'prog-mode-hook 'my/prog-mode-hook)

;; Major Modes
;; ===========

;; Built-in
;; --------

;; Third Party
;; -----------
(use-package circe
  :defer t
  :init
  (setq circe-channel-killed-confirmation nil
        circe-server-killed-confirmation  nil
        circe-default-quit-message "Toodaloo padawans! 👣"
        ;; Align messages.
        circe-format-action        "{nick:-16s} ❯ {body}"
        circe-format-notice        "{nick:-16s} ╋ {body}"
        circe-format-say           "{nick:-16s} ┃ {body}"
        circe-format-self-say      "       ━━━       ┃ {body}"
        circe-format-self-action   "       ━━━       ❯ {body}"
        ;; Show diff when topic is changed (esp. helpful when topic is long).
        circe-format-server-topic
        "*** Topic change by {nick} ({userhost}): {topic-diff}"
        circe-reduce-lurker-spam   t
        lui-fill-column            fill-column
        lui-fill-type              "                 ┃ "
        lui-logging-directory      (expand-file-name "~/archive/irc")
        lui-scroll-behavior        nil
        lui-time-stamp-position    nil)

  :preface
  (defun irc ()
    "Connect to IRC."
    (interactive)
    (circe "irc.freenode.net" :port '(6667 . 6697)))

  :config
  ;; Going to have to change these into an alists for multiple servers because
  ;; at the moment they just work for one server (Freenode).
  (defvar my/circe-nick "xd1le")
  (defvar my/circe-channels '("#emacs" "#emacs-circe" "##linux"))

  (defun circe-command-ID (passwd)
    (circe-command-NICK my/circe-nick)
    (circe-command-MSG (concat "nickserv identify " passwd)))

  (defun circe-command-CHANS (&optional ignored)
    (mapc 'circe-command-JOIN my/circe-channels))

  ;; Colorize nicks.
  (require 'circe-color-nicks)
  (enable-circe-color-nicks)

  ;; Ask for using a paste service when pasting 3 or more lines.
  (require 'lui-autopaste)
  (add-hook 'circe-channel-mode-hook 'enable-lui-autopaste)

  (load "lui-logging" nil t)
  (enable-lui-logging-globally)

  ;; Last reading position.
  (enable-lui-track-bar)
  (add-hook 'focus-out-hook 'lui-track-bar-move)

  ;; Gray out bots.
  (defvar my/circe-bot-list '("NixOS_GitHub"))
  (defun my/circe-message-option-bot (nick &rest ignored)
    (when (member nick my/circe-bot-list)
      '((text-properties . (face circe-fool-face)))))
  (add-hook 'circe-message-option-functions 'my/circe-message-option-bot)

  ;; Don't show names list upon joining a channel.
  ;; Taken from:
  ;; https://github.com/jorgenschaefer/circe/issues/298#issuecomment-262912703
  (circe-set-display-handler "353" 'circe-display-ignore)
  (circe-set-display-handler "366" 'circe-display-ignore)

  ;; But create custom command to list nicks.
  (defvar my/circe-names-width 79)

  (defun my/string-ci-< (a b)
    (string< (downcase a) (downcase b)))

  (defun my/nicks-to-lines (nicks line-length)
    (with-temp-buffer
      (setq fill-column line-length)
      (insert (mapconcat 'identity nicks " "))
      (goto-char (point-min))
      (fill-paragraph)
      (split-string (buffer-string) "\n")))

  (defun my/circe-display-nicks ()
    (interactive)
    (when (eq major-mode 'circe-channel-mode)
      (let* ((nicks (sort (circe-channel-nicks) 'my/string-ci-<))
             (nick-lines (my/nicks-to-lines nicks my/circe-names-width)))
        (with-output-to-temp-buffer "*circe-nicks*"
          (mapc 'princ nick-lines)
          (switch-to-buffer "*circe-nicks*")))))

  ;; Custom prompt.
  (defun my/circe-prompt ()
    (lui-set-prompt
     (concat (propertize "━━━" 'face 'circe-prompt-face) " ")))
  (add-hook 'circe-chat-mode-hook 'my/circe-prompt))

;; Theme
;; =====

;; Font
;; ====
(add-to-list 'default-frame-alist '(font . "Input-9"))
