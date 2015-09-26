;;; Basic.
(setq auto-save-default       nil
      create-lockfiles        nil
      backup-directory-alist  `(("." . "~/.backup"))
      backup-by-copying       t
      delete-old-versions     t
      kept-new-versions       6
      kept-old-versions       2
      version-control         t
      require-final-newline   t
      recenter-positions      '(0.25))

(setq-default auto-fill-function 'do-auto-fill)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

;; TODO: Find out if this has an impact on load times at all.
(add-to-list 'load-path "~/ui/emacs/.emacs.d/custom-keybindings")

;;; Packages.
(setq package-archives       nil
      quelpa-update-melpa-p  nil
      quelpa-upgrade-p       t)

(require 'package)
(package-initialize)

;; Bootstrap quelpa.
;; TODO: Possibly change these from doing `package-install-file` to just
;; `load-file`?
(unless (require 'package-build nil t)
  (package-install-file
   (expand-file-name "~/ui/vendor/emacs/package-build/package-build.el")))

(unless (require 'quelpa nil t)
  (package-install-file
   (expand-file-name "~/gh/forks/quelpa/quelpa.el")))

;; TODO: Find out if the presence of both `(require 'quelpa)` and `(require
;; 'quelpa-use-package)` here is detrimental to startup times.
(require 'quelpa)
(add-to-list
 'quelpa-melpa-recipe-dirs (expand-file-name "~/ui/emacs/.emacs.d/recipes"))

(quelpa 'quelpa)
(quelpa 'quelpa-use-package)

(require 'quelpa-use-package)

;; Configure libraries.
(use-package ov :defer t :quelpa)

;; Built-in minor modes.
(use-package avoid
  :demand t
  :config
  ;; Banish the vile mouse to the bottom right corner of the emacs window.
  (mouse-avoidance-mode 'banish)
  (custom-set-variables
   '(mouse-avoidance-banish-position
     '((frame-or-window . frame)
       (side . right)
       (side-pos . 0)
       (top-or-bottom . bottom)
       (top-or-bottom-pos . 0)))))

(use-package fringe
  :demand t :config (fringe-mode '(0 . nil)))

(use-package hl-line
  :demand t :config (global-hl-line-mode t))

(use-package paren
  :demand t
  :init (setq show-paren-delay 0)
  :config (show-paren-mode t))

(use-package simple
  :demand t
  :init
  ;; Enables `gj` and `gk` and co to move up and down visually wrapped lines.
  (setq line-move-visual nil)

  ;; These get disabled for whatever reason.
  (setq-default visual-line-fringe-indicators t)

  :config
  ;; Visually wrap long lines at right window edge.
  (global-visual-line-mode t))

(use-package whitespace
  :demand t
  :init (setq whitespace-style '(face empty tabs trailing))
  :config (global-whitespace-mode t))

;; Evil mode and evil mode related packages.
(use-package evil
  :demand t :quelpa
  :init
  (setq evil-want-C-u-scroll t
        evil-cross-lines t
        evil-shift-width 2
        evil-split-window-below t
        evil-vsplit-window-right t)

  ;; Change default shift-width for some specific modes.
  (add-hook 'python-mode-hook (lambda () (setq evil-shift-width python-indent)))

  :config
  (evil-declare-ignore-repeat 'recenter-top-bottom)

  ;; Disable hl-line-mode in evil visual state.
  (add-hook 'evil-visual-state-entry-hook
            (lambda () (setq-local global-hl-line-mode nil)))
  (add-hook 'evil-visual-state-exit-hook
            (lambda () (setq-local global-hl-line-mode t)))

  ;; Move up one line when doing q: and q/.
  (defun my-previous-line-advice (&optional CURRENT-COMMAND)
    (evil-previous-line))

  (advice-add 'evil-command-window-ex :after #'my-previous-line-advice)
  (advice-add 'evil-command-window-search-forward :after #'my-previous-line-advice)

  (require 'evil-custom-keybindings)
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

  (define-globalized-minor-mode my-global-evil-quick-scope-mode
    evil-quick-scope-mode turn-on-evil-quick-scope-mode)

  (my-global-evil-quick-scope-mode t))

(use-package evil-surround
  :quelpa :config (global-evil-surround-mode t))

;; Other third party minor mode packages.
(use-package company
  :quelpa
  :init
  (setq company-idle-delay 0)
  (add-hook 'after-init-hook 'global-company-mode)
  :config (require 'company-custom-keybindings))

(use-package fill-column-indicator
  :demand t :quelpa
  :init (setq-default fill-column 80)
  :config
  (define-globalized-minor-mode my-global-fci-mode fci-mode turn-on-fci-mode)
  (my-global-fci-mode t))

(use-package yasnippet
  :quelpa
  :init (setq yas-snippet-dirs '("~/ui/vendor/emacs/yasnippet-snippets"))
  :config
  (yas-global-mode t)
  (require 'yasnippet-custom-keybindings))

;; Built-in major modes.
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

  (setq ibuffer-formats '((mark modified read-only " "
                                (name 18 18 :left :elide) " "
                                (size-h 9 -1 :right) "  "
                                (mode 16 16 :left :elide) " "
                                filename-and-process)))

  (require 'ibuffer-custom-keybindings))

;; Third party major modes.
(use-package pdf-tools
  :disabled t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config (pdf-tools-install))

;; (use-package magit
;;  :disabled t
;;  :quelpa
;;  :commands magit-status
;;
;;  :config
;;  ;; TODO: Change the appropriate modes to start at motion state.
;;  (evil-set-initial-state 'magit-mode 'normal)
;;  (evil-set-initial-state 'magit-blame-mode 'normal)
;;  (evil-set-initial-state 'magit-diff-mode 'normal)
;;  (evil-set-initial-state 'magit-log-mode 'normal)
;;  (evil-set-initial-state 'magit-log-select-mode 'normal)
;;  (evil-set-initial-state 'magit-refs-mode 'normal)
;;  (evil-set-initial-state 'magit-status-mode 'normal)
;;  (evil-set-initial-state 'git-commit-mode 'normal)
;;  (evil-set-initial-state 'git-rebase-mode 'normal)
;;  (evil-set-initial-state 'with-editor-mode 'normal)
;;
;;  (evil-set-initial-state 'magit-file-section 'normal)
;;  (evil-set-initial-state 'magit-hunk-section 'normal)
;;  (evil-set-initial-state 'magit-unstaged-section 'normal)
;;  (evil-set-initial-state 'magit-staged-section 'normal)
;;  (evil-set-initial-state 'magit-commit-section 'normal)
;;  (evil-set-initial-state 'magit-module-commit-section 'normal)
;;  (evil-set-initial-state 'magit-stashes-section 'normal)
;;  (evil-set-initial-state 'magit-stash-section 'normal)
;;  (evil-set-initial-state 'magit-untracked-section 'normal)
;;  (evil-set-initial-state 'magit-branch-section 'normal)
;;  (evil-set-initial-state 'magit-remote-section 'normal)
;;  (evil-set-initial-state 'magit-tag-section 'normal)
;;
;;  (require 'magit-custom-keybindings))

;;; Fonts.
;; Disable italic and underlines.
(set-face-attribute 'mode-line nil :box nil)
(set-face-attribute 'mode-line-inactive nil :box nil)
(set-face-italic 'italic nil)

(mapc (lambda (face)
        (set-face-attribute face nil :weight 'normal :underline nil))
      (face-list))

;;; Theme.
(let ((theme-path "~/gh/themes/gruvbox-emacs"))
  (dolist (list '(load-path custom-theme-load-path))
    (add-to-list list theme-path)))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (load-theme 'gruvbox-light t))))
  (load-theme 'gruvbox-light t))
