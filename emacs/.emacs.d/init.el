(dolist (path
         '("~/ui/emacs/.emacs.d/my-custom-keybindings"
           "~/.emacs.d/lisp"
           "~/ui/vendor/emacs/nyan-mode"))
  (add-to-list 'load-path path))

;; Packages
;; ________
;;
(setq package-archives nil
      quelpa-update-melpa-p nil
      quelpa-upgrade-p t)

(require 'package)
(package-initialize)

;; Bootstrap Quelpa
;; ================
(unless (require 'package-build nil t)
  (load-file
   (expand-file-name "~/ui/vendor/emacs/package-build/package-build.el")))

(unless (require 'quelpa nil t)
  (load-file
   (expand-file-name "~/ui/vendor/emacs/quelpa/quelpa.el")))

(load-file (expand-file-name "~/ui/emacs/.emacs.d/recipe-list.el"))
(add-to-list 'quelpa-melpa-recipe-stores recipe-list)

(quelpa 'package-build)
(quelpa 'quelpa)
(quelpa 'use-package)
(quelpa 'quelpa-use-package)

(require 'quelpa-use-package)

;; Libraries
;; =========
(use-package dash        :defer t :quelpa)
(use-package fuzzy       :defer t :quelpa)
(use-package org-bullets :defer t :quelpa)
(use-package ov          :defer t :quelpa)
(use-package popup       :defer t :quelpa)
(use-package powerline   :defer t :quelpa)
(use-package spaceline   :defer t :quelpa)
(use-package s           :defer t :quelpa)

;; Built-in Minor Modes
;; ====================
(use-package avoid
  :demand t
  :config
  ;; Moves mouse to the bottom right corner of the emacs window.
  (mouse-avoidance-mode 'banish)
  (custom-set-variables
   '(mouse-avoidance-banish-position
     '((frame-or-window . frame)
       (side . right)
       (side-pos . 0)
       (top-or-bottom . bottom)
       (top-or-bottom-pos . 0)))))

(use-package files
  :demand t
  :init
  (setq auto-save-default      nil
        backup-directory-alist `(("." . "~/.backup"))
        backup-by-copying      t
        create-lockfiles       nil
        delete-old-versions    t
        kept-new-versions      6
        kept-old-versions      2
        require-final-newline  t
        version-control        t))

(use-package frame
  :demand t
  :init
  (setq blink-cursor-mode nil)
  ;; Actually part of window.el but window.el does not provide 'window..
  (setq recenter-positions '(0.25)))

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
  ;; (setq fill-indent-according-to-mode t)
  ;; From `buffer.c'.
  (setq-default auto-fill-function 'do-auto-fill)
  (setq-default tab-width 2)
  ;; From `indent.c'.
  (setq-default indent-tabs-mode nil)
  ;; Enables `gj` and `gk` and co to move up and down visually wrapped lines.
  (setq line-move-visual nil)
  ;; These get disabled for whatever reason.
  (setq-default visual-line-fringe-indicators t)
  :config
  ;; Visually wrap long lines at right window edge.
  (global-visual-line-mode t))

(use-package vc-hooks
  :demand t
  :init (setq vc-follow-symlinks t))

(use-package whitespace
  :demand t
  :init (setq whitespace-style '(face empty tabs trailing))
  :config (global-whitespace-mode t))

;; Evil
;; ====
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

  (require 'my-custom-evil-keybindings)
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

;; Other Third Party Minor Modes
;; =============================
(use-package auto-complete-config
  :demand t :quelpa
  :init
  (setq ac-auto-show-menu 0.4
        ac-auto-start 3
        ac-candidate-menu-min 0
        ac-delay 0.2
        ac-disable-inline t
        ac-show-menu-immediately-on-auto-complete t
        ac-use-menu-map t
        ac-use-quick-help nil)
  :config
  (add-to-list
   'ac-dictionary-directories "~/ui/vendor/emacs/auto-complete/dict")
  (add-to-list 'ac-modes 'org-mode)
  (ac-config-default)
  (define-key ac-completing-map "\r" nil)
  (define-key ac-menu-map "\r" nil)
  (define-key ac-menu-map (kbd "S-<iso-lefttab>") 'ac-previous)
  (evil-define-key 'insert ac-menu-map (kbd "C-p") 'ac-previous)
  (evil-define-key 'insert ac-menu-map (kbd "C-n") 'ac-next)
  (evil-define-key 'insert ac-completing-map "\t" 'ac-complete))

(use-package fill-column-indicator
  :demand t :quelpa
  :init (setq-default fill-column 80)
  :config
  (define-globalized-minor-mode my-global-fci-mode fci-mode turn-on-fci-mode)
  (my-global-fci-mode t))

(use-package nyan-mode
  :demand t ; :quelpa
  :config (nyan-mode t))

(use-package spaceline-config
  :demand t :quelpa
  :init
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  :config
  (spaceline-spacemacs-theme)
  (setq spaceline-buffer-size-p nil
        spaceline-minor-modes-p nil
        spaceline-nyan-cat-p t))

(use-package yasnippet
  :disabled t :quelpa
  :init (setq yas-snippet-dirs '("~/ui/vendor/emacs/yasnippet-snippets"))
  :config
  (yas-global-mode t)
  (require 'my-custom-yasnippet-keybindings))

;; Built-in Major Modes
;; ====================
(use-package erc
  :commands erc
  :init (setq erc-header-line-format nil)
  :config
  (add-hook 'erc-mode-hook (lambda () (setq-local auto-fill-function nil)))
  ;; Michael Markert has some good urgency hint settings:
  ;; Source: <https://github.com/cofi/dotfiles/blob/master/emacs.d/config/cofi-erc.el>
  ;; (defun cofi/erc-find-erc-frame ()
  ;;   (cl-find-if (p (string= (frame-parameter x 'name) "ERC")) (frame-list)))

  ;; (defun cofi/erc-frame-urgency (&rest ignore)
  ;;   (x-urgency-hint (cofi/erc-find-erc-frame) t)
  ;;   nil)

  ;; (add-hook 'erc-text-matched-hook #'cofi/erc-frame-urgency)

  ;; (defun cofi/erc-urgency-on-nick ()
  ;;   (let ((s (buffer-substring-no-properties (point-min) (point-max))))
  ;;     (when (string-match-p (format "\\b%s\\b" (erc-current-nick)) s)
  ;;       (cofi/erc-frame-urgency))))

  ;; (setq ercn-notify-rules
  ;;       '((current-nick . all)
  ;;         (keyword . all)
  ;;         (query-buffer . all)))

  ;; (add-hook 'ercn-notify 'cofi/erc-frame-urgency)
  ;; (add-hook 'erc-insert-post-hook #'cofi/erc-urgency-on-nick)

  ;; TODO: Logging.
  ;; TODO: Don't automatically scroll down on new message.
  ;; TODO: Comment color non-messages (people joining/quiting).
  ;; TODO: Urgency hint and highlight on nick mentions.
  ;; TODO: Place miscellaneous functions like this as a separate library file
  ;; that use-package loads as a library when needed.
  ;; (defun x-urgency-hint (frame arg &optional source)
  ;;   "Enable or disables urgency hint for the frame FRAME.
  ;; Set ARG to non-nil to enable urgency hint, nil to disable."
  ;;   (let* ((wm-hints (append (x-window-property
  ;;                             "WM_HINTS" frame "WM_HINTS" source nil t) nil))
  ;;          (flags (car wm-hints)))
  ;;     (setcar wm-hints (if arg
  ;;                          (logior flags #x100)
  ;;                        (logand flags (lognot #x100))))
  ;;     (x-change-window-property "WM_HINTS" wm-hints frame "WM_HINTS" 32 t)))
  )

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

  (setq ibuffer-formats
        '((mark modified read-only " " (name 18 18 :left :elide) " "
                (size-h 9 -1 :right) "  " (mode 16 16 :left :elide) " "
                filename-and-process)))

  (require 'my-custom-ibuffer-keybindings))

(use-package org
  :defer t
  :init
  (setq org-adapt-indentation t
        org-bullets-bullet-list '("▣" "◉" "✠" "➤")
        org-catch-invisible-edits 'smart
        org-ellipsis "…"
        org-hide-emphasis-markers t
        org-src-fontify-natively t
        org-startup-folded 'nofold
        org-startup-indented t)
  :preface
  (defvar-local my-org-list-item-fill-last-line-number 0)

  (defun my-org-set-list-item-p-fill-prefix ()
    "Sets fill-prefix accordingly so that auto-fill properly
hanging indents long list lines.

Will not work when typing in a new list item from scratch without
using `org-meta-return' though."
    (when (eq 'org-mode major-mode)
      (let ((ln (line-number-at-pos)))
        (when (not (eq my-org-list-item-fill-last-line-number ln))
          (if (org-at-item-p)
              (let ((l (thing-at-point 'line t)))
                (string-match org-list-full-item-re l)
                (setq-local
                 fill-prefix
                 (make-string (length (match-string-no-properties 0 l)) ?\s)))
            (setq-local fill-prefix nil))
          (setq-local my-org-list-item-fill-last-line-number ln)))))
  :config
  (add-hook
   'org-mode-hook
   (lambda ()
     (progn
       (turn-off-fci-mode)
       (setq-local line-spacing '0.2)
       (org-bullets-mode 1)
       (text-scale-increase 1))))
  (add-hook 'post-command-hook 'my-org-set-list-item-p-fill-prefix)
  (evil-define-key 'normal org-mode-map
    (kbd "SPC tl") 'org-toggle-link-display))

;; Third Party Major Modes
;; =======================
(use-package haskell-mode
  :defer t :quelpa
  :init
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indent))

(use-package rust-mode
  :defer t :quelpa
  :mode ("\\.rs\\'" . rust-mode))

;; (use-package pdf-tools
;;   :disabled t
;;   :mode ("\\.pdf\\'" . pdf-view-mode)
;;   :config (pdf-tools-install))

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
;;  (require 'my-custom-magit-keybindings))

;; Theme
;; _____
;;
(require 'my-currently-chosen-theme)

;; Faces
;; _____
;;
(set-face-attribute 'mode-line nil :box nil)
(set-face-attribute 'mode-line-inactive nil :box nil)

;; Disable underlines.
(mapc (lambda (face)
        (set-face-attribute face nil :underline nil))
      (face-list))

;; Font
;; ____
;;
(add-to-list 'default-frame-alist '(font . "Input-9"))
