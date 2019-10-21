;; Essential Packages
;; ==================
;;
;; These are essential packages that affect the rest of the init configuration,
;; and therefore need to be loaded first.
(setq straight-enable-use-package-integration t
      straight-use-package-by-default t
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

(use-package general
  :demand t
  :config
  (general-auto-unbind-keys)
  (general-override-mode)

  (eval-and-compile
    (defalias 'gsetq #'general-setq)
    (defalias 'gsetq-local #'general-setq-local)
    (defalias 'gsetq-default #'general-setq-default))

  (general-create-definer general-my/leader
    :states '(normal visual)
    :keymaps 'override
    :prefix "SPC"))

;; Generic Editor Behaviour
;; ========================
(gsetq-default auto-fill-function 'do-auto-fill
               fill-column 80
               ;; Do not insert tabs when indenting.
               indent-tabs-mode nil
               ;; Make gj/gk work across visually wrapped lines.
               line-move-visual nil
               ;; Vertical cursor padding.
               scroll-margin 5
               scroll-step 1
               tab-width 2)

(gsetq auto-save-list-file-prefix nil
       create-lockfiles nil
       custom-file (expand-file-name "~/dots/emacs/custom.el")
       ;; TODO: Change this to make backup files at ~/archive/backup.
       make-backup-files nil
       mouse-avoidance-banish-position '((frame-or-window . frame)
                                         (side . right)
                                         (side-pos . 0)
                                         (top-or-bottom . bottom)
                                         (top-or-bottom-pos . 0))
       require-final-newline t
       sentence-end-double-space nil
       show-paren-delay 0
       vc-follow-symlinks t
       ;; Show fringe indicators for visually wrapped lines.
       visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow)
       whitespace-line-column fill-column
       whitespace-style '(face empty lines-tail tabs trailing))

(blink-cursor-mode -1)
(column-number-mode)
(global-subword-mode)
(load-file custom-file)
(mouse-avoidance-mode 'banish)
(show-paren-mode)
(tooltip-mode -1)

(defun my/text-mode-functions ()
  (visual-line-mode)
  (whitespace-mode))

(general-add-hook 'text-mode-hook #'my/text-mode-functions)
(general-add-hook 'prog-mode-hook #'my/text-mode-functions)

(use-package aggressive-indent :ghook 'prog-mode-hook)

(use-package hl-line
  :after evil
  :straight nil
  :ghook
  'text-mode-hook
  'prog-mode-hook
  :config
  ;; Fix `describe-face' defaulting to hl-line face.
  ;; https://emacs.stackexchange.com/a/45719/8693
  (defun my/face-at-point ()
    (let ((face (get-text-property (point) 'face)))
      (or (and (face-list-p face)
               (car face))
          (and (symbolp face)
               face))))

  (defun my/describe-face (&rest ignore)
    (interactive (list (read-face-name "Describe face"
                                       (or (my/face-at-point) 'default)
                                       t)))
    ;; This only needs to change the `interactive` spec, so:
    nil)

  (general-add-advice 'describe-face :before #'my/describe-face)

  ;; Disable hl-line-mode in evil visual state.
  (general-add-hook 'evil-visual-state-entry-hook
                    (lambda ()
                      (when (or (derived-mode-p 'text-mode)
                                (derived-mode-p 'prog-mode))
                        (hl-line-mode -1))))
  (general-add-hook 'evil-visual-state-exit-hook
                    (lambda ()
                      (when (or (derived-mode-p 'text-mode)
                                (derived-mode-p 'prog-mode))
                        (hl-line-mode)))))

;; Keybinding Related
;; ==================
(use-package defrepeater :demand t)

(use-package evil
  :init
  (gsetq evil-cross-lines t
         evil-overriding-maps nil
         evil-shift-width 2
         evil-split-window-below t
         evil-vsplit-window-right t
         evil-want-C-d-scroll t
         evil-want-C-u-scroll t
         evil-want-integration t
         evil-want-keybinding nil
         evil-want-Y-yank-to-eol t)

  (evil-mode)

  :config
  ;; (defun my/insert-two-spaces ()
  ;;   (interactive)
  ;;   (insert "  "))

  (defun my/edit-init-file ()
    (interactive)
    (find-file (expand-file-name "init.el" user-emacs-directory)))

  ;; Move up one line when doing q: and q/.
  (general-add-advice 'evil-command-window-ex :after #'evil-previous-line)
  (general-add-advice 'evil-command-window-search-forward
                      :after #'evil-previous-line)

  ;; Set normal state as default state.
  ;; In :config as it references evil mode variables.
  (gsetq evil-normal-state-modes (append evil-emacs-state-modes
                                         evil-normal-state-modes)
         evil-emacs-state-modes nil
         evil-motion-state-modes nil)

  ;; Make the window resizing commands repeatable.
  ;;
  ;; The disadvantage of defrepeater compared to a hydra [1] is that it only
  ;; works one command at a time, i.e. wanting to use another command with the
  ;; same prefix sequence requires starting the sequence again.
  ;;
  ;; But the advantage here is the rest of evil-window-map commands exit
  ;; automatically, which is better for one off commands.
  ;;
  ;; [1]: https://github.com/abo-abo/hydra
  (defrepeater #'evil-window-increase-height)
  (defrepeater #'evil-window-decrease-height)
  (defrepeater #'evil-window-increase-width)
  (defrepeater #'evil-window-decrease-width)

  (general-def
    [remap evil-window-increase-height] #'evil-window-increase-height-repeat
    [remap evil-window-decrease-height] #'evil-window-decrease-height-repeat
    [remap evil-window-increase-width] #'evil-window-increase-width-repeat
    [remap evil-window-decrease-width] #'evil-window-decrease-width-repeat)

  ;; (general-def 'insert
  ;;  "C-SPC" 'my/insert-two-spaces)

  (general-def '(normal visual) 'override
    ;; TODO: Figure out how to make this work.
    ;; "z RET" 'evil-scroll-line-to-top
    "\\" #'evil-switch-to-windows-last-buffer
    "'" #'evil-ex
    "g'" #'execute-extended-command
    "+" #'text-scale-increase
    "-" #'text-scale-decrease
    "H" #'evil-first-non-blank
    "L" #'evil-end-of-line
    "M" #'evil-jump-item
    ;; "l" as in "list" buffers.
    "gl" #'ibuffer
    "C-n" #'next-buffer
    "C-p" #'previous-buffer
    "C-h" #'evil-window-left
    "C-j" #'evil-window-down
    "C-k" #'evil-window-up
    "C-l" #'evil-window-right)

  (general-def evil-window-map
    "d" #'evil-window-delete)

  (general-def 'insert 'override
    "C-h" #'help-command
    "C-l" #'forward-char)

  (general-def 'normal 'override
    "Q" #'kill-this-buffer)

  (general-def 'normal
    "gs" #'evil-write)

  (general-my/leader
    "SPC" #'help-command
    "d" #'dired
    "ee" #'eval-expression
    "ei" #'my/edit-init-file
    "q" "@q"
    "sf" #'straight-freeze-versions
    "si" #'straight-use-package
    "su" #'straight-pull-all
    "t" #'load-theme
    "w" #'evil-window-map
    "x" #'evil-command-window-ex)

  ;; Escape everywhere.
  (general-def 'emacs "<escape>" #'evil-normal-state)
  (general-def '(minibuffer-local-map
                 minibuffer-local-ns-map
                 minibuffer-local-completion-map
                 minibuffer-local-must-match-map
                 minibuffer-local-isearch-map)
    "<escape>" #'keyboard-escape-quit))

(use-package evil-collection
  :after evil
  :init
  (evil-collection-init))

(use-package evil-commentary
  :after evil
  :demand t
  :config
  ;; Not using minor mode since it sets a super key binding which I don't like.
  ;; But it means these become global keybindings.
  (general-def 'normal
    "gc" #'evil-commentary
    "gy" #'evil-commentary-yank))

(use-package evil-surround
  :after evil
  :init
  (global-evil-surround-mode))

(use-package which-key
  :init
  (which-key-mode))

;; Buffer and File Related
;; =======================
(use-package ibuffer
  :straight nil
  :gfhook #'ibuffer-auto-mode
  :init
  (gsetq ibuffer-expert t
         kill-buffer-query-functions
         (remq 'process-kill-buffer-query-function
               kill-buffer-query-functions))
  :config
  (general-def 'normal ibuffer-mode-map
    "x" #'ibuffer-do-delete
    "K" #'ibuffer-do-kill-on-deletion-marks))

(use-package dired
  :straight nil
  :gfhook #'auto-revert-mode
  :init
  (put 'dired-find-alternate-file 'disabled nil))

;; Lisp Languages
;; ==============
(use-package lispyville
  :after evil
  :ghook #'emacs-lisp-mode-hook
  :config
  (lispyville-set-key-theme
   '(operators c-w prettify (atom-motions t) slurp/barf-cp escape))

  (general-def 'insert lispyville-mode-map
    "(" #'lispy-parens
    "[" #'lispy-brackets
    "{" #'lispy-braces
    "\"" #'lispy-quotes
    ";" #'lispy-comment)

  (general-def 'normal lispyville-mode-map
    ":" #'eval-last-sexp
    "gc" #'lispyville-comment-or-uncomment
    "gy" #'lispyville-comment-and-clone-dwim
    "C-b" #'lispyville-wrap-round
    "C-]" #'lispyville-wrap-brackets
    "C-}" #'lispyville-wrap-braces
    "[" #'lispyville-previous-opening
    "]" #'lispyville-next-closing
    "{" #'lispyville-previous-closing
    "}" #'lispyville-next-opening
    "(" #'lispyville-backward-up-list
    ")" #'lispyville-up-list)

  ;; Still prefer having the paragraph motions for visual selection.
  (general-def 'visual lispyville-mode-map
    ":" #'eval-region
    "{" #'evil-backward-paragraph
    "}" #'evil-forward-paragraph))

;; Not necessarily only for lisp languages but main use case is for lisp
;; languages where this really helps.
(use-package rainbow-delimiters
  :ghook
  'prog-mode-hook
  :config
  (defun my/rainbow-delimiters-default-faces (&rest _)
    ;; Default rainbow delimiters colours do not differentiate themselves
    ;; enough.
    (unless custom-enabled-themes
      (set-face-foreground 'rainbow-delimiters-depth-1-face "dark orange")
      (set-face-foreground 'rainbow-delimiters-depth-2-face "deep pink")
      (set-face-foreground 'rainbow-delimiters-depth-3-face "chartreuse")
      (set-face-foreground 'rainbow-delimiters-depth-4-face "deep sky blue")
      (set-face-foreground 'rainbow-delimiters-depth-5-face "goldenrod")
      (set-face-foreground 'rainbow-delimiters-depth-6-face "orchid")
      (set-face-foreground 'rainbow-delimiters-depth-7-face "spring green")
      (set-face-foreground 'rainbow-delimiters-depth-8-face "sienna")
      (set-face-foreground 'rainbow-delimiters-depth-9-face "red")))

  (defun my/rainbow-delimiters-faces (&rest _)
    ;; Always bold delimiters regardless of theme, as they are easier to see and
    ;; identify for me.
    (set-face-bold 'rainbow-delimiters-depth-1-face t)
    (set-face-bold 'rainbow-delimiters-depth-2-face t)
    (set-face-bold 'rainbow-delimiters-depth-3-face t)
    (set-face-bold 'rainbow-delimiters-depth-4-face t)
    (set-face-bold 'rainbow-delimiters-depth-5-face t)
    (set-face-bold 'rainbow-delimiters-depth-6-face t)
    (set-face-bold 'rainbow-delimiters-depth-7-face t)
    (set-face-bold 'rainbow-delimiters-depth-8-face t)
    (set-face-bold 'rainbow-delimiters-depth-9-face t))

  (general-add-advice 'disable-theme
                      :after #'my/rainbow-delimiters-default-faces)
  (general-add-advice 'load-theme :after #'my/rainbow-delimiters-faces)
  (my/rainbow-delimiters-default-faces)
  (my/rainbow-delimiters-faces))

;; Fonts
;; =====
(defun my/set-fonts ()
  (set-face-font 'variable-pitch "Noto Sans-11")
  (set-fontset-font t 'unicode "Noto Color Emoji"))

;; Main font is set in `early-init.el'.
(if (daemonp)
    (general-add-hook 'after-make-frame-functions
                      (lambda (frame)
                        (with-selected-frame frame
                          (when (display-graphic-p frame)
                            (my/set-fonts)))))
  (when (display-graphic-p)
    (my/set-fonts)))

;; Theme
;; =====

;; Make a variable that holds the current theme, for convenience.
;; 'user is the name for the default theme.
(defvar my/current-theme 'user)

(defun my/update-current-theme (&optional arg &rest _)
  (gsetq my/current-theme arg))

(defun my/reset-current-theme (&rest _)
  (gsetq my/current-theme 'user))

(general-add-advice 'disable-theme :after #'my/reset-current-theme)
(general-add-advice 'load-theme :after #'my/update-current-theme)

(use-package gotham-theme)

(use-package material-theme
  :init
  (if (daemonp)
      (general-add-hook 'after-make-frame-functions
                        (lambda (frame)
                          (with-selected-frame frame
                            (when (display-graphic-p frame)
                              (load-theme 'material-light 'no-confirm)))))
    (when (display-graphic-p)
      (load-theme 'material-light 'no-confirm))))

;; https://emacs.stackexchange.com/questions/3112/how-to-reset-color-theme
(defun my/undo-themes (&rest _)
  (mapc #'disable-theme custom-enabled-themes))

(general-add-advice 'load-theme :before #'my/undo-themes)

(defun my/better-default-theme (&rest _)
  (unless custom-enabled-themes
    ;; Make selection stand out more for default theme.
    (set-face-attribute 'region nil :background "#666" :foreground "#fff")))

(general-add-advice 'disable-theme :after #'my/better-default-theme)
(my/better-default-theme)

;; Mode Line
;; =========

;; Themes setting a box for the mode line messes with emoji alignment when using
;; emojis in mode line.
(defun my/boxless-mode-line (&rest _)
  (set-face-attribute 'mode-line nil :box nil)
  (set-face-attribute 'mode-line-inactive nil :box nil))

(general-add-advice 'load-theme :after #'my/boxless-mode-line)
(my/boxless-mode-line)

;; Helper for mode line text.
(defun my/str-fill (str pos max char)
  (let ((strlen (length str)))
    (cond
     ((= strlen max) str)
     ((> strlen max) (substring str 0 max))
     (t (let ((fillstr
               (mapconcat 'identity
                          (make-list (- max strlen) (string char))
                          "")))
          (if (eq pos 'left)
              (concat fillstr str)
            (concat str fillstr)))))))

(gsetq-default
 mode-line-format
 (list
  ;; Full memory error message.
  " %e"

  ;; Evil state.
  ;;
  ;; - Operator state is uniquely identified via cursor shape change.
  ;;
  ;; - Insert/emacs state identifiers are for differentiating between the two
  ;;   since they both have the same I-beam style cursor shape.
  '(:eval (cond
           ((string= evil-mode-line-tag " <I> ") "✏️ ")
           ((string= evil-mode-line-tag " <E> ") "✒️ ")
           ((string= evil-mode-line-tag " <M> ") "👣 ")
           ((string= evil-mode-line-tag " <V> ") "👀 ")
           (t "     ")))

  ;; Whether frame is an emacsclient instance or not.
  (if (daemonp) "🖥️ " "")

  ;; Major mode.
  '(:eval (let ((mode (downcase mode-name)))
            (cond
             ((string= mode "emacs-lisp") "elisp")
             ((string= mode "shell-script") "shell")
             (t mode))))

  ;; Space separator.
  " "

  ;; Read only indicator or modified indicator if not read only.
  '(:eval (if buffer-read-only "🧐 " (if (buffer-modified-p) "💾 " "     ")))

  ;; Buffer.
  mode-line-buffer-identification

  ;; Insert spaces to right align rest.
  ;; Works because max length of right aligned text is known beforehand.
  '(:eval
    (propertize
     " "
     'display
     '((space :align-to (- (+ right right-fringe right-margin)
                           ;; Max length of rhs.
                           31)))))

  ;; Version control info, usually git branch.
  '(:eval
    (my/str-fill
     (if (or (string-prefix-p " Git:" vc-mode)
             (string-prefix-p " Git-" vc-mode))
         (substring vc-mode 5)
       vc-mode)
     'right
     12
     ?\s))

  ;; Line number.
  "%06l:"

  ;; Column number.
  '(:eval (my/str-fill (number-to-string (+ (current-column) 1)) 'right 4 ?\s))

  ;; Buffer position.
  " %06p "))
