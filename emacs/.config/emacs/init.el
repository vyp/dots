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

(use-package aggressive-indent)

(use-package apheleia
  :straight (:host github :repo "raxod502/apheleia")
  :init
  (gsetq apheleia-formatters
         '((prettier
            . ("npx"
               "prettierx"
               "--generator-star-spacing"
               "--space-before-function-paren"
               "--single-quote"
               "--jsx-single-quote"
               "--no-semi"
               "--yield-star-spacing"
               "--no-align-ternary-lines"
               "--trailing-comma" "none"
               file)))
         apheleia-mode-alist '((js-mode . prettier)))
  (apheleia-global-mode +1))

(use-package hl-line
  :after (evil ivy)
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
  (general-add-advice 'counsel-describe-face :before #'my/describe-face)

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

;; Transitive library dependency that ivy uses to sort completion candidates.
(use-package flx :demand t)

(use-package ivy
  :init
  (gsetq ivy-count-format ""
         ;; Don't insert ^ at beginning of some ivy-related commands and let flx
         ;; sort the entries.
         ivy-initial-inputs-alist nil
         ivy-wrap t)
  (ivy-mode)

  :config
  ;; Copy pasted everything except last line of function from ivy.el.
  (defun my/ivy-partial-or-next-line ()
    "Complete the minibuffer text as much as possible.
If the text hasn't changed as a result, forward to `ivy-next-line'."
    (interactive)
    (cond
     ((and completion-cycle-threshold
           (< (length ivy--all-candidates) completion-cycle-threshold))
      (let ((ivy-wrap t))
        (ivy-next-line)))
     ((and (eq (ivy-state-collection ivy-last) #'read-file-name-internal)
           (or (and (equal ivy--directory "/")
                    (string-match-p "\\`[^/]+:.*\\'" ivy-text))
               (= (string-to-char ivy-text) ?/)))
      (let ((default-directory ivy--directory)
            dir)
        (minibuffer-complete)
        (setq ivy-text (ivy--input))
        (when (setq dir (ivy-expand-file-if-directory ivy-text))
          (ivy--cd dir))))
     (t
      (or (ivy-partial)
          (when (or (eq this-command last-command)
                    (eq ivy--length 1))
            (ivy-next-line))))))

  (general-def ivy-minibuffer-map
    "TAB" #'my/ivy-partial-or-next-line
    ;; <backtab> is what emacs translates S-TAB to.
    "<backtab>" #'ivy-previous-line
    "C-j" #'ivy-next-line
    "C-k" #'ivy-previous-line
    "C-d" #'ivy-scroll-down-command
    "C-u" #'ivy-scroll-up-command)

  (general-def ivy-reverse-i-search-map
    "C-k" nil)

  (general-def ivy-switch-buffer-map
    "C-k" #'ivy-previous-line))

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
    "\\" #'evil-switch-to-windows-last-buffer
    "'" #'evil-ex
    "g'" #'counsel-M-x
    "gp" #'counsel-git-grep
    "+" #'text-scale-increase
    "-" #'text-scale-decrease
    "H" #'evil-first-non-blank
    "L" #'evil-end-of-line
    "M" #'evil-jump-item
    ;; "l" as in "list" buffers.
    "gl" #'ivy-switch-buffer
    "C-n" #'next-buffer
    "C-p" #'previous-buffer
    "C-f" #'counsel-git
    "C-h" #'evil-window-left
    "C-j" #'evil-window-down
    "C-k" #'evil-window-up
    "C-l" #'evil-window-right
    "C-/" #'help-command)

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
    "as" #'counsel-apropos
    ;; bl for 'buffer list'.
    "bl" #'ibuffer
    "df" #'counsel-describe-face
    "di" #'dired
    "dw" #'evil-window-delete
    "ev" #'eval-expression
    "ei" #'my/edit-init-file
    "ff" #'counsel-find-file
    "fj" #'counsel-file-jump
    ;; Locate doesn't seem to work.
    ;; "fl" #'counsel-locate
    "gp" #'counsel-grep
    "q" "@q"
    "sf" #'straight-freeze-versions
    "si" #'straight-use-package
    "su" #'straight-pull-all
    "t" #'counsel-load-theme
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

;; Autocompletion
;; ==============
(use-package company
  :ghook
  'text-mode-hook
  'prog-mode-hook)

;; Lisp Languages
;; ==============
(use-package racket-mode)

(use-package lispyville
  :after evil
  :ghook
  #'emacs-lisp-mode-hook
  #'racket-mode-hook
  :gfhook
  #'aggressive-indent-mode
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

;; Other Languages
;; ===============
(use-package haskell-mode)

(use-package js-mode
  :straight nil
  :init
  (gsetq js-indent-level 2))

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
      (set-face-foreground 'rainbow-delimiters-depth-9-face "red"))

    (pcase my/current-theme
      ('sexy-monochrome
       (progn
         (set-face-foreground 'rainbow-delimiters-depth-1-face "#93a8c6")
         (set-face-foreground 'rainbow-delimiters-depth-2-face "#616161")
         (set-face-foreground 'rainbow-delimiters-depth-3-face "#93a8c6")
         (set-face-foreground 'rainbow-delimiters-depth-4-face "#616161")
         (set-face-foreground 'rainbow-delimiters-depth-5-face "#93a8c6")
         (set-face-foreground 'rainbow-delimiters-depth-6-face "#616161")
         (set-face-foreground 'rainbow-delimiters-depth-7-face "#93a8c6")
         (set-face-foreground 'rainbow-delimiters-depth-8-face "#616161")
         (set-face-foreground 'rainbow-delimiters-depth-9-face "#93a8c6")))
      ('user nil)))

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

(defun my/custom-theme-overrides (&rest _)
  (pcase my/current-theme
    ((or 'punpun-dark 'punpun-light)
     (progn
       (set-face-attribute 'font-lock-function-name-face nil
                           :slant 'normal
                           :weight 'bold)
       (set-face-attribute 'font-lock-constant-face nil
                           :slant 'normal
                           :weight 'bold)
       (set-face-attribute 'font-lock-variable-name-face nil
                           :slant 'normal
                           :weight 'bold)
       (if (eq my/current-theme 'punpun-dark)
           (progn
             (set-face-foreground 'font-lock-comment-face "#3a3a3a")
             (set-face-attribute 'show-paren-match nil
                                 :foreground "#080808"
                                 :background "#949494"))
         (progn
           (set-face-foreground 'font-lock-comment-face "#b2b2b2")
           (set-face-attribute 'show-paren-match nil
                               :foreground "#eeeeee"
                               :background "#585858")))))
    ('sexy-monochrome (set-face-foreground 'mode-line-inactive "#616161"))
    ('user nil)))

(general-add-advice 'load-theme :after #'my/custom-theme-overrides)

(use-package almost-mono-themes)
(use-package flucui-themes)
(use-package gotham-theme)
(use-package hybrid-reverse-theme)
(use-package metalheart-theme)
(use-package notink-theme)

(use-package punpun-theme
  :init
  (if (daemonp)
      (general-add-hook 'after-make-frame-functions
                        (lambda (frame)
                          (with-selected-frame frame
                            (when (display-graphic-p frame)
                              (load-theme 'punpun-dark 'no-confirm)))))
    (when (display-graphic-p)
      (load-theme 'punpun-dark 'no-confirm))))

(use-package sexy-monochrome-theme)
(use-package sketch-themes)
(use-package timu-spacegrey-theme)

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

  ;; Whether frame is an emacsclient instance or not.
  (if (daemonp) " @ » " "")

  ;; Read only indicator or modified indicator if not read only.
  '(:eval (if buffer-read-only
              "[RO] » "
            (if (buffer-modified-p)
                "* » "
              "    ")))

  ;; Major mode.
  '(:eval (propertize
           (let ((mode (downcase (if (listp mode-name)
                                     (car mode-name)
                                   mode-name))))
             (cond
              ((string= mode "emacs-lisp") "elisp")
              ((string= mode "shell-script") "shell")
              ((string= mode "javascript") "js")
              (t mode)))
           'face 'italic))

  ;; Separator.
  " » "

  ;; Buffer.
  '(:eval (propertize "%b" 'face 'bold))

  ;; Insert spaces to right align rest.
  ;; Works because max length of right aligned text is known beforehand.
  '(:eval
    (propertize
     " "
     'display
     `((space :align-to (- (+ right right-fringe right-margin)
                           ;; Max length of rhs.
                           ,(if (null vc-mode) 21 36))))))

  ;; Version control info, usually git branch.
  '(:eval
    (if (null vc-mode)
        ""
      (concat
       (my/str-fill
        (if (or (string-prefix-p " Git:" vc-mode)
                (string-prefix-p " Git-" vc-mode))
            (substring vc-mode 5)
          vc-mode)
        'left
        12
        ?\s)
       " « ")))

  ;; Line number.
  "%06l:"

  ;; Column number.
  '(:eval (my/str-fill (number-to-string (+ (current-column) 1)) 'right 3 ?\s))

  ;; Separator.
  " « "

  ;; Buffer position.
  "%06p "))
