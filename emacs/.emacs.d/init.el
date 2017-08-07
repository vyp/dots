;; Bootstrapping
;; =============
;;
;; straight.el
;; -----------
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

;; use-package
;; -----------
(straight-use-package 'use-package)
(setq use-package-always-ensure t)

;; Startup
;; =======
;;
;; Prevent Emacs from creating "~/.emacs.d/auto-save-list" directory on
;; startup.
(setq auto-save-list-file-prefix nil)

;; Built-in Minor Modes
;; ====================
(use-package cus-edit
  :ensure nil
  :demand t
  :init
  (setq custom-file (expand-file-name "~/dots/emacs/custom.el"))
  :config
  (load custom-file))

;; Third Party Major Modes
;; =======================
(use-package circe)

;; Font
;; ====
(add-to-list 'default-frame-alist '(font . "Input-9"))
