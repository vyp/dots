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
(use-package general :demand t)
(general-auto-unbind-keys)

(eval-and-compile
  (defalias 'gsetq #'general-setq)
  (defalias 'gsetq-local #'general-setq-local)
  (defalias 'gsetq-default #'general-setq-default))

(general-create-definer general-my/leader
  :states '(normal visual)
  :keymaps 'override
  :prefix "SPC")

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

;; Prevent Emacs from creating "~/.emacs.d/auto-save-list" directory on
;; startup.
(gsetq auto-save-default nil
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

(blink-cursor-mode -1)
(global-subword-mode)
(load-file custom-file)
(show-paren-mode t)

;; Evil
;; ----
(use-package evil
  :demand t
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
         evil-want-C-u-scroll t
         evil-want-integration t
         evil-want-keybinding nil
         evil-want-Y-yank-to-eol t)

  (evil-mode)

  :config
  ;; Move up one line when doing `q:` and `q/`.
  (general-advice-add 'evil-command-window-ex :after #'evil-previous-line)
  (general-advice-add 'evil-command-window-search-forward
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

  ;; (general-def 'insert
  ;;  "C-SPC" 'my/insert-two-spaces)
  )

(general-def '(normal visual)
  ;; TODO: Figure out how to make this work.
  ;; "z RET" 'evil-scroll-line-to-top
  "\\" #'evil-switch-to-windows-last-buffer
  "'" #'evil-ex
  ;; Mapping under non-prefix keys like q has to be done like this using
  ;; `general-key-dispatch', see:
  ;; https://github.com/noctuid/general.el#mapping-under-non-prefix-keys
  "q" (general-key-dispatch 'evil-record-macro
        "'" #'evil-command-window-ex)
  "+" #'text-scale-increase
  "-" #'text-scale-decrease
  "H" #'evil-first-non-blank
  "L" #'evil-end-of-line
  "M" #'evil-jump-item)

(general-def 'normal
  "Q" "@q"
  "gs" #'evil-write)

(general-my/leader
  "SPC" #'execute-extended-command
  "x" #'execute-extended-command
  "l" #'ibuffer)

;; Escape everywhere.
(general-def 'emacs "<escape>" #'evil-normal-state)
(general-def '(minibuffer-local-map
               minibuffer-local-ns-map
               minibuffer-local-completion-map
               minibuffer-local-must-match-map
               minibuffer-local-isearch-map)
  "<escape>" #'keyboard-escape-quit)

(use-package evil-collection
  :after evil
  :demand t
  :config
  (evil-collection-init))

(use-package evil-commentary
  :after evil
  :demand t
  :config
  ;; I don't use the minor mode since it sets a super key binding which I don't
  ;; like.
  (general-def 'normal
    "gc" #'evil-commentary
    "gy" #'evil-commentary-yank))

(use-package evil-surround
  :after evil
  :demand t
  :config
  (global-evil-surround-mode 1))

;; Mode Line
;; =========
(use-package telephone-line
  :demand t
  :init
  (gsetq telephone-line-primary-left-separator 'telephone-line-flat
         telephone-line-primary-right-separator 'telephone-line-flat
         telephone-line-secondary-left-separator 'telephone-line-flat
         telephone-line-secondary-right-separator 'telephone-line-flat)
  :config
  (telephone-line-mode))

;; Buffers
;; =======
(general-add-hook 'ibuffer-mode-hook #'ibuffer-auto-mode)

;; Lisp Languages
;; ==============
(use-package lispy
  :ghook #'emacs-lisp-mode-hook)

(use-package lispyville
  :after (evil lispy)
  :ghook #'lispy-mode-hook
  :init
  (gsetq lispyville-motions-put-into-special t)

  :config
  (lispyville-set-key-theme
   '(operators c-w prettify (atom-motions t) slurp/barf-cp escape))

  (general-def 'normal lispyville-mode-map
    "gc" #'lispyville-comment-or-uncomment
    "gy" #'lispyville-comment-and-clone-dwim
    "M-p" #'lispyville-wrap-round
    "M-[" #'lispyville-wrap-brackets
    "M-b" #'lispyville-wrap-braces
    "[" #'lispyville-previous-opening
    "]" #'lispyville-next-closing
    "{" #'lispyville-previous-closing
    "}" #'lispyville-next-opening
    "(" #'lispyville-backward-up-list
    ")" #'lispyville-up-list)

  ;; I still prefer having the paragraph motions for visual selection.
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
  (whitespace-mode 1))

(defun my/prog-mode-hook ()
  (my/text-mode-hook)
  (rainbow-delimiters-mode 1))

(add-hook 'text-mode-hook 'my/text-mode-hook)
(add-hook 'prog-mode-hook 'my/prog-mode-hook)
