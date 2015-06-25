; (require 'cask "~/etsi/cask/cask.el")
; (cask-initialize)
; (add-to-list 'load-path (cask-load-path ((cask-initialize "~/etsi"))))
; (add-to-list 'load-path "~/etsi/.cask/24.5.1/elpa/goto-chg-20131228.1459")
; (add-to-list 'load-path "~/etsi/.cask/24.5.1/elpa/undo-tree-20140509.522")
; (add-to-list 'load-path "~/etsi/.cask/24.5.1/elpa/evil-20150619.1252")

; (require 'evil)
; (evil-mode t)

;; Plugins.
; (package-initialize)
; (defvar quelpa-ci-dir "~/etsi/quelpa")
; (unless (require 'quelpa nil t)
;   (load (concat quelpa-ci-dir "/bootstrap.el")))
;
; (quelpa 'evil)
; (evil-mode t)

(add-to-list 'load-path "~/etsi/emacs-packages/evil")
; (push "~/etsi/emacs-packages/evil" 'load-path)
(require 'evil)
(evil-mode t)

; (require 'package)
; (add-to-list 'package-archives
;              '("melpa" . "http://melpa.milkbox.net/packages/") t)
;
; ;;; from purcell/emacs.d
; (defun require-package (package &optional min-version no-refresh)
;   "Install given PACKAGE, optionally requiring MIN-VERSION.
; If NO-REFRESH is non-nil, the available package lists will not be
; re-downloaded in order to locate PACKAGE."
;   (if (package-installed-p package min-version)
;       t
;     (if (or (assoc package package-archive-contents) no-refresh)
;         (package-install package)
;       (progn
;         (package-refresh-contents)
;         (require-package package min-version t)))))
;
; (package-initialize)
;
; (require-package 'evil)
;
; (setq evil-search-module 'evil-search
;       evil-want-C-u-scroll t
;       evil-want-C-w-in-emacs-state t)
;
; (require 'evil)
; (evil-mode t)

; Newbie (basic?) settings.
(setq auto-save-default nil)
(setq create-lockfiles nil)

; Does not seem to work...
(set-face-italic-p 'italic nil)
(set-face-bold-p 'bold nil)

; Enable line numbers.
(global-linum-mode t)
