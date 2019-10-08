;; Font
;; ====
(add-to-list 'default-frame-alist '(font . "Iosevka-12"))
(set-fontset-font "fontset-default" 'unicode "Noto Color Emoji")
(set-face-font 'variable-pitch "Noto Sans-11")

;; Theme
;; =====
(add-to-list 'custom-theme-load-path "~/dl/repos/moe-theme.el")
(load-theme 'moe-light 'no-confirm)

(setq-default auto-fill-function 'do-auto-fill
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
(setq auto-save-default nil
      auto-save-list-file-prefix nil
      create-lockfiles nil
      custom-file (expand-file-name "~/dots/emacs/custom.el")
      ;; TODO: Change this to make backup files at ~/archive/backup.
      make-backup-files nil
      require-final-newline t
      show-paren-delay 0
      straight-use-package-by-default t
      vc-follow-symlinks t
      ;; Show fringe indicators for visually wrapped lines.
      visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow)
      whitespace-line-column fill-column
      whitespace-style '(face empty lines-tail tabs trailing))

(load-file custom-file)

(blink-cursor-mode -1)
(menu-bar-mode -1)
(show-paren-mode t)
(tool-bar-mode -1)
(toggle-scroll-bar -1)

  ;; Visually wrap long lines.
(global-visual-line-mode t)
;; Do not visually wrap long lines for the buffer menu. (Doesn't work.)
(add-hook 'Buffer-menu-mode-hook (lambda () (visual-line-mode nil)))

;; I find this a little cleaner and more 'declarative' than having multiple
;; `add-hook's around in various use-package forms.
;;
;; The idea is taken from: https://emacs.stackexchange.com/a/5384

(defun my/text-mode-hook ()
  (whitespace-mode 1))

(defun my/prog-mode-hook ()
  (rainbow-delimiters-mode 1)
  (whitespace-mode 1))

(add-hook 'text-mode-hook 'my/text-mode-hook)
(add-hook 'prog-mode-hook 'my/prog-mode-hook)

;; Bootstrap Emacs Package Manager - straight.el
;; =============================================
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
(use-package general :demand t)
(load-file (expand-file-name "evil.el" user-emacs-directory))
(use-package rainbow-delimiters :defer t)
