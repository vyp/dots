;; Startup
;; =======
;;
;; Code that configures the Emacs startup sequence.

;; Prevent Emacs from creating "~/.emacs.d/auto-save-list" directory on
;; startup.
(setq auto-save-list-file-prefix nil)

;; Bootstrap Emacs Package Manager - straight.el
;; =============================================

(setq straight-recipe-overrides
      '((nil . ((straight
                 :type git :host github
                 :repo "raxod502/straight.el"
                 :branch "develop"
                 :files ("straight.el"))))))

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
(use-package general :demand t
  :recipe (:host github :repo "noctuid/general.el" :branch "buttercup"))
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
(setq-default fill-column 80
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

(use-package cus-edit
  :demand t
  :ensure nil
  :init
  (setq custom-file (expand-file-name "~/dots/emacs/custom.el"))
  :config
  (load custom-file))

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

(use-package evil-surround
  :demand t
  :config
  (global-evil-surround-mode 1))

(use-package files
  :defer t
  :ensure nil
  :init
  (setq auto-save-default     nil
        ;; Pretty much only use one instance of Emacs at a time anyway (or use
        ;; Emacs server), so lockfiles to prevent editing collisions are almost
        ;; always unnecessary.
        create-lockfiles      nil
        ;; TODO: Change this to make backup files at ~/archive/backup.
        make-backup-files     nil
        require-final-newline t))

(use-package frame
  :demand t
  :ensure nil
  :config
  (blink-cursor-mode -1))

(use-package hl-line
  :demand t
  :ensure nil
  :config
  (global-hl-line-mode t))

(use-package menu-bar
  :demand t
  :ensure nil
  :config
  (menu-bar-mode -1))

(use-package paren
  :demand t
  :ensure nil
  :init
  (setq show-paren-delay 0)
  :config
  (show-paren-mode t))

(use-package prog-mode
  :demand t
  :ensure nil
  :init
  (setq-default prettify-symbols-alist
                ;; Double-ended hyphen arrows.
                '(("<->" . #xe100)
                  ("<-->" . #xe101)
                  ("<--->" . #xe102)
                  ("<---->" . #xe103)
                  ("<----->" . #xe104)

                  ;; Double-ended equals arrows.
                  ("<=>" . #xe105)
                  ("<==>" . #xe106)
                  ("<===>" . #xe107)
                  ("<====>" . #xe108)
                  ("<=====>" . #xe109)

                  ;; Double-ended asterisk operators.
                  ("<**>" . #xe10a)
                  ("<***>" . #xe10b)
                  ("<****>" . #xe10c)
                  ("<*****>" . #xe10d)

                  ;; HTML comments.
                  ("<!--" . #xe10e)
                  ("<!---" . #xe10f)

                  ;; Three character operators with discards.
                  ("<$" . #xe110)
                  ("<$>" . #xe111)
                  ("$>" . #xe112)
                  ("<." . #xe113)
                  ("<.>" . #xe114)
                  (".>" . #xe115)
                  ("<*" . #xe116)
                  ("<*>" . #xe117)
                  ("*>" . #xe118)
                  ("<\\" . #xe119)
                  ("<\\>" . #xe11a)
                  ("\\>" . #xe11b)
                  ("</" . #xe11c)
                  ("</>" . #xe11d)
                  ("/>" . #xe11e)
                  ("<\"" . #xe11f)
                  ("<\">" . #xe120)
                  ("\">" . #xe121)
                  ("<'" . #xe122)
                  ("<'>" . #xe123)
                  ("'>" . #xe124)
                  ("<^" . #xe125)
                  ("<^>" . #xe126)
                  ("^>" . #xe127)
                  ("<&" . #xe128)
                  ("<&>" . #xe129)
                  ("&>" . #xe12a)
                  ("<%" . #xe12b)
                  ("<%>" . #xe12c)
                  ("%>" . #xe12d)
                  ("<@" . #xe12e)
                  ("<@>" . #xe12f)
                  ("@>" . #xe130)
                  ("<#" . #xe131)
                  ("<#>" . #xe132)
                  ("#>" . #xe133)
                  ("<+" . #xe134)
                  ("<+>" . #xe135)
                  ("+>" . #xe136)
                  ("<-" . #xe137)
                  ("<->" . #xe138)
                  ("->" . #xe139)
                  ("<!" . #xe13a)
                  ("<!>" . #xe13b)
                  ("!>" . #xe13c)
                  ("<?" . #xe13d)
                  ("<?>" . #xe13e)
                  ("?>" . #xe13f)
                  ("<|" . #xe140)
                  ("<|>" . #xe141)
                  ("|>" . #xe142)
                  ("<:" . #xe143)
                  ("<:>" . #xe144)
                  (":>" . #xe145)

                  ;; Colons.
                  ("::" . #xe146)
                  (":::" . #xe147)
                  ("::::" . #xe148)

                  ;; Arrow-like operators.
                  ("->" . #xe149)
                  ("->-" . #xe14a)
                  ("->--" . #xe14b)
                  ("->>" . #xe14c)
                  ("->>-" . #xe14d)
                  ("->>--" . #xe14e)
                  ("->>>" . #xe14f)
                  ("->>>-" . #xe150)
                  ("->>>--" . #xe151)
                  ("-->" . #xe152)
                  ("-->-" . #xe153)
                  ("-->--" . #xe154)
                  ("-->>" . #xe155)
                  ("-->>-" . #xe156)
                  ("-->>--" . #xe157)
                  ("-->>>" . #xe158)
                  ("-->>>-" . #xe159)
                  ("-->>>--" . #xe15a)
                  (">-" . #xe15b)
                  (">--" . #xe15c)
                  (">>-" . #xe15d)
                  (">>--" . #xe15e)
                  (">>>-" . #xe15f)
                  (">>>--" . #xe160)
                  ("=>" . #xe161)
                  ("=>=" . #xe162)
                  ("=>==" . #xe163)
                  ("=>>" . #xe164)
                  ("=>>=" . #xe165)
                  ("=>>==" . #xe166)
                  ("=>>>" . #xe167)
                  ("=>>>=" . #xe168)
                  ("=>>>==" . #xe169)
                  ("==>" . #xe16a)
                  ("==>=" . #xe16b)
                  ("==>==" . #xe16c)
                  ("==>>" . #xe16d)
                  ("==>>=" . #xe16e)
                  ("==>>==" . #xe16f)
                  ("==>>>" . #xe170)
                  ("==>>>=" . #xe171)
                  ("==>>>==" . #xe172)
                  (">=" . #xe173)
                  (">==" . #xe174)
                  (">>=" . #xe175)
                  (">>==" . #xe176)
                  (">>>=" . #xe177)
                  (">>>==" . #xe178)
                  ("<-" . #xe179)
                  ("-<-" . #xe17a)
                  ("--<-" . #xe17b)
                  ("<<-" . #xe17c)
                  ("-<<-" . #xe17d)
                  ("--<<-" . #xe17e)
                  ("<<<-" . #xe17f)
                  ("-<<<-" . #xe180)
                  ("--<<<-" . #xe181)
                  ("<--" . #xe182)
                  ("-<--" . #xe183)
                  ("--<--" . #xe184)
                  ("<<--" . #xe185)
                  ("-<<--" . #xe186)
                  ("--<<--" . #xe187)
                  ("<<<--" . #xe188)
                  ("-<<<--" . #xe189)
                  ("--<<<--" . #xe18a)
                  ("-<" . #xe18b)
                  ("--<" . #xe18c)
                  ("-<<" . #xe18d)
                  ("--<<" . #xe18e)
                  ("-<<<" . #xe18f)
                  ("--<<<" . #xe190)
                  ("<=" . #xe191)
                  ("=<=" . #xe192)
                  ("==<=" . #xe193)
                  ("<<=" . #xe194)
                  ("=<<=" . #xe195)
                  ("==<<=" . #xe196)
                  ("<<<=" . #xe197)
                  ("=<<<=" . #xe198)
                  ("==<<<=" . #xe199)
                  ("<==" . #xe19a)
                  ("=<==" . #xe19b)
                  ("==<==" . #xe19c)
                  ("<<==" . #xe19d)
                  ("=<<==" . #xe19e)
                  ("==<<==" . #xe19f)
                  ("<<<==" . #xe1a0)
                  ("=<<<==" . #xe1a1)
                  ("==<<<==" . #xe1a2)
                  ("=<" . #xe1a3)
                  ("==<" . #xe1a4)
                  ("=<<" . #xe1a5)
                  ("==<<" . #xe1a6)
                  ("=<<<" . #xe1a7)
                  ("==<<<" . #xe1a8)

                  ;; Monadic operators.
                  (">=>" . #xe1a9)
                  (">->" . #xe1aa)
                  (">-->" . #xe1ab)
                  (">==>" . #xe1ac)
                  ("<=<" . #xe1ad)
                  ("<-<" . #xe1ae)
                  ("<--<" . #xe1af)
                  ("<==<" . #xe1b0)

                  ;; Composition operators.
                  (">>" . #xe1b1)
                  (">>>" . #xe1b2)
                  ("<<" . #xe1b3)
                  ("<<<" . #xe1b4)

                  ;; Lens operators.
                  (":+" . #xe1b5)
                  (":-" . #xe1b6)
                  (":=" . #xe1b7)
                  ("+:" . #xe1b8)
                  ("-:" . #xe1b9)
                  ("=:" . #xe1ba)
                  ("=^" . #xe1bb)
                  ("=+" . #xe1bc)
                  ("=-" . #xe1bd)
                  ("=*" . #xe1be)
                  ("=/" . #xe1bf)
                  ("=%" . #xe1c0)
                  ("^=" . #xe1c1)
                  ("+=" . #xe1c2)
                  ("-=" . #xe1c3)
                  ("*=" . #xe1c4)
                  ("/=" . #xe1c5)
                  ("%=" . #xe1c6)

                  ;; Logical operators.
                  ("/\\" . #xe1c7)
                  ("\\/" . #xe1c8)

                  ;; Monoid operators.
                  ("<>" . #xe1c9)
                  ("<+" . #xe1ca)
                  ("<+>" . #xe1cb)
                  ("+>" . #xe1cc)))
  :config
  (global-prettify-symbols-mode))

(use-package rainbow-delimiters
  :defer t)

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
  (global-visual-line-mode t)
  ;; Do not visually wrap long lines for the buffer menu. (Doesn't work.)
  (add-hook 'Buffer-menu-mode-hook (lambda () (visual-line-mode nil))))

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
  :defer t
  :ensure nil
  :init
  (setq vc-follow-symlinks t))

(use-package whitespace
  :defer t
  :ensure nil
  :init
  (setq whitespace-line-column fill-column
        whitespace-style       '(face empty lines-tail tabs trailing)))

;; Basic Major Mode Hooks
;; ======================
;;
;; Now that all minor modes have hopefully been loaded or autoloaded, we can
;; setup their hooks (as in, if they're not already globally enabled).
;;
;; I find this a little cleaner and more 'declarative' than having multiple
;; `add-hook's around in various use-package forms.
;;
;; The idea is taken from: https://emacs.stackexchange.com/a/5384

(defun my/elisp-prettify-symbols ()
  (setq prettify-symbols-alist
        (append '(("lambda" . 955)
                  ("defun"  . 10765))
                prettify-symbols-alist)))

(defun my/scheme-prettify-symbols ()
  (setq prettify-symbols-alist
        (append '(("lambda" . 955)
                  ("define" . 10765))
                prettify-symbols-alist)))

(defun my/text-mode-hook ()
  (whitespace-mode 1))

(defun my/prog-mode-hook ()
  (rainbow-delimiters-mode 1)
  (whitespace-mode 1))

(add-hook 'emacs-lisp-mode-hook 'my/elisp-prettify-symbols)
(add-hook 'scheme-mode-hook 'my/scheme-prettify-symbols)
(add-hook 'text-mode-hook 'my/text-mode-hook)
(add-hook 'prog-mode-hook 'my/prog-mode-hook)

;; Major Modes
;; ===========

(use-package circe
  :defer t
  :preface
  (defun irc ()
    "Connect to IRC."
    (interactive)
    (circe "irc.freenode.net" :port '(6667 . 6697)))

  (defun my/circe-format-truncated-nick (type args)
    (let* ((nick (plist-get args :nick))
           (body (plist-get args :body))
           (maxlen (if (eq type 'action) 11 12))
           (lui-nick (concat "{nick:" (number-to-string maxlen) "s}")))
      (when (> (length nick) maxlen)
        (setq nick (substring nick 0 maxlen)))
      (lui-format
       (pcase type
         ('say (concat lui-nick " {body}"))
         ('action (concat "*" lui-nick " {body}*"))
         ('notice (concat lui-nick " ‼ {body} ‼")))
       :nick nick :body body)))

  (defun my/circe-format-action (&rest args)
    (my/circe-format-truncated-nick 'action args))

  (defun my/circe-format-notice (&rest args)
    (my/circe-format-truncated-nick 'notice args))

  (defun my/circe-format-say (&rest args)
    (my/circe-format-truncated-nick 'say args))

  :init
  (setq circe-channel-killed-confirmation nil
        circe-server-killed-confirmation  nil
        circe-default-quit-message "Toodaloo padawans! 👣"
        ;; Align messages.
        circe-format-action        'my/circe-format-action
        circe-format-notice        'my/circe-format-notice
        circe-format-say           'my/circe-format-say
        circe-format-self-say      "         ━━━ {body}"
        circe-format-self-action   "         ━━━ *{body}*"
        ;; Show diff when topic is changed (esp. helpful when topic is long).
        circe-format-server-topic
        "*** Topic change by {nick} ({userhost}): {topic-diff}"
        circe-reduce-lurker-spam   t
        lui-fill-column            fill-column
        lui-fill-type              "             "
        lui-logging-directory      (expand-file-name "~/archive/irc")
        lui-scroll-behavior        nil
        lui-time-stamp-position    nil
        tracking-ignored-buffers
        (mapcar (lambda (channel) (list channel 'circe-highlight-nick-face))
                '("#emacs$" "#haskell$" "irc.freenode.net" "nickserv"
                  "#vim$")))

  :config
  ;; Going to have to change these into an alists for multiple servers because
  ;; at the moment they just work for one server (Freenode).
  ;;
  ;; TODO: Figure out how to set urgency hint when nick is mentioned.
  (defvar my/circe-nick "xd1le")
  ;; TODO: Separate this big list into a smaller core part, and an extended
  ;; interest part.
  (defvar my/circe-channels
    '("#ai" "#blink" "#chromium" "#chromium-extensions" "#chromium-support"
      "#conservancy" "#elm" "#emacs" "#emacs-circe" "#firefox" "#fsf" "#fsfe"
      "#github" "#gnu" "#gnulinuxlovers" "#guile" "#guix" "#haskell"
      "#herbstluftwm" "#idris" "#imv" "#julia" "#krebs" "#latex" "#nixos"
      "#nixos-dev" "#org-mode" "#peers" "#qutebrowser" "##rust" "#scheme"
      "#space" "#syncthing" "##vegan" "#vim" "#webkit" "#zsh"))

  (defun circe-command-ID (passwd)
    (circe-command-NICK my/circe-nick)
    (circe-command-MSG (concat "nickserv identify " passwd))
    (setq-default circe-server-inhibit-auto-reconnect-p t))

  (defun circe-command-CHANS (&optional ignored)
    (mapc 'circe-command-JOIN my/circe-channels))

  ;; Colorize nicks.
  (require 'circe-color-nicks)
  (enable-circe-color-nicks)

  ;; Ask for using a paste service when pasting 3 or more lines.
  (require 'lui-autopaste)
  (add-hook 'circe-channel-mode-hook 'enable-lui-autopaste)

  ;; Logging.
  (load "lui-logging" nil t)

  (defun my/lui-enable-tracked-logging ()
    (unless (tracking-ignored-p (current-buffer) '(circe-highlight-nick-face))
      (enable-lui-logging)))

  (add-hook 'lui-mode-hook 'my/lui-enable-tracked-logging)

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
  (add-hook 'circe-chat-mode-hook 'my/circe-prompt)

  ;; Auto-track ignored channels on talk.
  (defadvice circe-command-SAY (after my/circe-unignore-target)
    (let ((ignored (tracking-ignored-p
                    (current-buffer) '(circe-highlight-nick-face))))
      (when ignored
        (setq tracking-ignored-buffers
              (remove ignored tracking-ignored-buffers))
        (my/lui-enable-tracked-logging))))
  (ad-activate 'circe-command-SAY))

;; Theme
;; =====
(add-to-list 'custom-theme-load-path "~/dots/emacs/vendor/moe-theme.el")
(load-theme 'moe-light 'no-confirm)

;; Font
;; ====
(add-to-list 'default-frame-alist '(font . "Iosevka-12"))
(set-face-font 'variable-pitch "Noto Sans-11")
