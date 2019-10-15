;; Fonts
;; =====
;; Main font is set in `early-init.el`.
(set-fontset-font "fontset-default" 'unicode "Noto Color Emoji")
(set-face-font 'variable-pitch "Noto Sans-11")

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
(use-package diminish :demand t)

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

;; Custom Function Definitions
;; ===========================
;;
;; Now that general.el is configured, we can declare our own utility functions.
(defun my/edit-init-file ()
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

;; https://emacs.stackexchange.com/questions/3112/how-to-reset-color-theme
(defun my/undo-themes (&rest _)
  (mapc #'disable-theme custom-enabled-themes))

(general-add-advice 'load-theme :before #'my/undo-themes)

;; Vanilla Options
;; ===============
(gsetq-default auto-fill-function 'do-auto-fill
               fill-column 80
               ;; Do not insert tabs when indenting.
               indent-tabs-mode nil
               ;; Make `gj`/`gk` work across visually wrapped lines.
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
(global-subword-mode)
(load-file custom-file)
(mouse-avoidance-mode 'banish)
(show-paren-mode)

;; Keybinding Related
;; ==================
(use-package defrepeater :demand t)

(use-package evil
  ;; :preface
  ;; (defun my/insert-two-spaces ()
  ;;   (interactive)
  ;;   (insert "  "))

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
  ;; Move up one line when doing `q:` and `q/`.
  (general-add-advice 'evil-command-window-ex :after #'evil-previous-line)
  (general-add-advice 'evil-command-window-search-forward
                      :after #'evil-previous-line)

  ;; Disable hl-line-mode in evil visual state.
  (general-add-hook 'evil-visual-state-entry-hook (lambda () (hl-line-mode -1)))
  (general-add-hook 'evil-visual-state-exit-hook #'hl-line-mode)

  ;; Set normal state as default state.
  ;; In :config as it references evil mode variables.
  (gsetq evil-normal-state-modes (append evil-emacs-state-modes
                                         evil-normal-state-modes)
         evil-emacs-state-modes nil
         evil-motion-state-modes nil)

  ;; Make the window resizing commands repeatable.
  ;;
  ;; The disadvantage of defrepeater compared to a hydra is that it only works
  ;; one command at a time, i.e. wanting to use another command with the same
  ;; prefix sequence requires starting the sequence again.
  ;;
  ;; But the advantage here is the rest of evil-window-map commands exit
  ;; automatically, which is better for one off commands.
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
    ;; Mapping under non-prefix keys like q has to be done like this using
    ;; `general-key-dispatch', see:
    ;; https://github.com/noctuid/general.el#mapping-under-non-prefix-keys
    "q" (general-key-dispatch 'evil-record-macro
          "'" #'evil-command-window-ex)
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
    "ee" #'eval-expression
    "ei" #'my/edit-init-file
    "q" "@q"
    "sf" #'straight-freeze-versions
    "su" #'straight-pull-all
    "w" #'evil-window-map)

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

;; Mode Line
;; =========
(use-package telephone-line
  :init
  (gsetq telephone-line-primary-left-separator 'telephone-line-flat
         telephone-line-primary-right-separator 'telephone-line-flat
         telephone-line-secondary-left-separator 'telephone-line-flat
         telephone-line-secondary-right-separator 'telephone-line-flat)
  (telephone-line-mode))

;; Buffers
;; =======
(gsetq ibuffer-expert t
       kill-buffer-query-functions
       (remq 'process-kill-buffer-query-function
             kill-buffer-query-functions))

(general-add-hook 'ibuffer-mode-hook #'ibuffer-auto-mode)
(general-with-eval-after-load 'ibuffer
  (general-def 'normal ibuffer-mode-map
    "x" #'ibuffer-do-delete
    "K" #'ibuffer-do-kill-on-deletion-marks))

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
    "\"" #'lispy-quotes)

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
    "{" #'evil-backward-paragraph
    "}" #'evil-forward-paragraph))

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
  (whitespace-mode))

(defun my/prog-mode-hook ()
  (my/text-mode-hook)
  (rainbow-delimiters-mode))

(add-hook 'text-mode-hook 'my/text-mode-hook)
(add-hook 'prog-mode-hook 'my/prog-mode-hook)

;; Theme
;; =====
(use-package moe-theme
  :init
  (when (display-graphic-p)
    (load-theme 'moe-light 'no-confirm)))
