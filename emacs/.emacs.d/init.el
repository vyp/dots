;;;; Todos.
;;; Special.
;; TODO: Magit.
;; TODO: Filesystem navigation, opening files, managing buffers etc.
;; TODO: Ido.
;; TODO: Helm.
;; TODO: Terminal/eshell/zsh.
;; TODO: Pandoc mode.
;; TODO: MPD client.
;; TODO: Image viewer.
;; TODO: IRC.
;; TODO: Mail.
;; TODO: RSS.
;; TODO: Bittorrent client.

;;; Behaviour.
;; TODO: Expand region.
;; TODO: Emmet.
;; TODO: Flycheck.
;; TODO: Deft.
;; TODO: Paredit.
;; TODO: Multiple cursors.
;; TODO: Highlight trailing newlines.
;; TODO: Highlight TODOs.
;; TODO: Transposing.
;; TODO: Javascript mode 2 space indentation instead of 4 space.
;; TODO: wellle/targets.vim ?

;;; Keybindings.
;; TODO: Allow `gg` in ibuffer by removing `g` for refresh.
;; TODO: pdf-view-mode.
;; TODO: "Completion List", "Packages", "Compile-log", "Messages" buffers.

;;; Appearance.
;; TODO: Report company mode popup bug on right window edge?
;; TODO: Disable fill column indicator in magit mode buffers?
;; TODO: Remove right fringe in pdf-view-mode.
;; TODO: Statusbar colors and customisation.

;;; Theme.
;; TODO: Remove bold references from redbelly.
;; TODO: Company mode colours.
;; TODO: Fix "*Packages*" buffer colours.
;; TODO: Visual search and replace colours.
;; TODO: Export theme configuration to scheme specific file.

;;; Other.
;; TODO: Learn elisp.
;; TODO: Completely switch back to el-get instead of quelpa:
;; https://github.com/milkypostman/melpa/issues/2944

;;;; Packages.
(setq evil-want-C-u-scroll t)
(setq evil-cross-lines t)
(setq evil-shift-width 2)
(setq evil-split-window-below t)
(setq evil-vsplit-window-right t)

(add-hook 'python-mode-hook
  (function (lambda ()
    (setq evil-shift-width python-indent))))

(setq quelpa-update-melpa-p nil)
(setq package-archives nil)
(package-initialize)

;; Bootstrap quelpa (the package manager) if needed.
(if (require 'quelpa nil t)
    (quelpa
     '(quelpa
       :fetcher github
       :repo "quelpa/quelpa"))

  (with-temp-buffer
    (url-insert-file-contents "https://raw.github.com/quelpa/quelpa/master/bootstrap.el")
    (eval-buffer)))

;; So that quelpa updates package-build too when doing `quelpa-upgrade`.
(quelpa 'package-build)

(quelpa 'evil)
(quelpa 'evil-matchit)
(quelpa 'evil-nerd-commenter)
(quelpa 'evil-surround)
(quelpa 'fill-column-indicator)
(quelpa 'company)

(quelpa
 '(yasnippet
   :fetcher github
   :repo "capitaomorte/yasnippet"
   :files ("yasnippet.el")))

(quelpa 'pdf-tools)
(quelpa 'magit)

;; (quelpa
;;  '(redbelly-theme
;;    :fetcher github
;;    :repo "vyp/redbelly"))

(require 'evil)
(require 'evil-matchit)
(require 'evil-nerd-commenter)
(require 'evil-surround)
(require 'fill-column-indicator)
(require 'company)
(require 'yasnippet)
(require 'magit)

(require 'paren)
(require 'ibuffer)

(setq company-idle-delay 0)
(add-hook 'after-init-hook 'global-company-mode)

;;;; Appearance.
(set-face-attribute 'mode-line nil :box nil)
(set-face-attribute 'mode-line-inactive nil :box nil)
(set-face-italic-p 'italic nil)
(mapc
 (lambda (face)
   (set-face-attribute face nil :weight 'normal :underline nil))
 (face-list))

(global-hl-line-mode t)
(fringe-mode '(0 . nil))

;; Enables `gj` and `gk` etc. to move up and down visually wrapped lines.
(setq line-move-visual nil)

;; Visually wrap long lines at right window edge.
(global-visual-line-mode t)
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (visual-line-mode nil)))

;; These get disabled for whatever reason.
(setq-default visual-line-fringe-indicators t)

(setq-default fill-column 80)
(define-globalized-minor-mode my-global-fci-mode fci-mode turn-on-fci-mode)
(my-global-fci-mode t)

(global-whitespace-mode t)
(setq whitespace-style '(face tabs trailing))

(setq show-paren-delay 0)
(show-paren-mode t)

;;; Ibuffer.
;; Use human readable Size column instead of original one.
(define-ibuffer-column size-h
  (:name "Size" :inline t)
  (cond
   ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
   ((> (buffer-size) 100000) (format "%7.0fk" (/ (buffer-size) 1000.0)))
   ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
   (t (format "%8d" (buffer-size)))))

;; Modify the default ibuffer formats.
(setq ibuffer-formats
      '((mark modified read-only " "
              (name 18 18 :left :elide)
              " "
              (size-h 9 -1 :right)
              "  "
              (mode 16 16 :left :elide)
              " "
              filename-and-process)))

;;; PDF Tools.
(pdf-tools-install)

;; Associate .pdf files with pdf-view-mode.
(add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))

;;; Magit.
(custom-set-faces
 `(magit-section-heading
   ((t (:weight, 'normal))))
 `(magit-section-highlight
   ((t (:weight, 'normal))))
 `(magit-diff-file-heading
   ((t (:weight, 'normal)))))

;;;; Theme.
(add-to-list 'custom-theme-load-path "~/hak/redbelly")
(load-theme 'redbelly t)

;;;; Basic.
(setq auto-save-default nil)
(setq create-lockfiles nil)
(setq backup-directory-alist `(("." . "~/.backup")))
(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)
(setq require-final-newline t)

(setq-default auto-fill-function 'do-auto-fill)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

(setq recenter-positions '(0.25))
(mouse-avoidance-mode 'banish)
(custom-set-variables
 '(mouse-avoidance-banish-position
   '((frame-or-window . frame)
     (side . right)
     (side-pos . 0)
     (top-or-bottom . bottom)
     (top-or-bottom-pos . 0))))

;;; Yasnippet official snippets.
(setq yas-snippet-dirs
      '("~/etsi/yasnippet-snippets"))

(yas-global-mode t)

;;;; Hooks.
;; Disable hl-line-mode in evil visual state.
(add-hook 'evil-visual-state-entry-hook
          (lambda () (setq-local global-hl-line-mode nil)))
(add-hook 'evil-visual-state-exit-hook
          (lambda () (setq-local global-hl-line-mode t)))

;;;; Advice.
(defun my-previous-line-advice (&optional CURRENT-COMMAND)
  (evil-previous-line))

(advice-add 'evil-command-window-ex :after #'my-previous-line-advice)
(advice-add 'evil-command-window-search-forward :after #'my-previous-line-advice)

;;;; Keybindings.

;; The way this works with evil mode/vim style keybindings everywhere is that
;; the following bindings are generically defined, so that they go wherever evil
;; mode is enabled. But then they are selectively overwritten for particular
;; special [1] modes in the files in the `kb` directory.
;;
;; This is esentially a shortcut for only allowing the following basic mappings
;; to apply to text and prog modes.
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Basic-Major-Modes.html#index-special_002dmode

;;; Basic.
(defun my-minibuffer-keyboard-quit ()
  "Abort recursive edit.
  In Delete Selection mode, if the mark is active, just deactivate it;
  then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(defun my-exit-evil-command-window ()
  "Exit evil command window."
  (interactive)
  (other-window -1)
  (other-window 1)
  (kill-this-buffer)
  (evil-window-delete))

;; Escape quits emacs things as a vim user would expect.
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'my-minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'my-minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'my-minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'my-minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'my-minibuffer-keyboard-quit)
(global-set-key [escape] 'evil-exit-emacs-state)
(evil-define-key 'normal evil-command-window-mode-map [escape] 'my-exit-evil-command-window)

(define-key evil-normal-state-map "\\" 'evil-switch-to-windows-last-buffer)
(define-key evil-visual-state-map "\\" 'evil-switch-to-windows-last-buffer)

(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
(define-key evil-normal-state-map "H" 'evil-first-non-blank)
(define-key evil-operator-state-map "H" 'evil-jump-item)
(define-key evil-visual-state-map "H" 'evil-first-non-blank)
(define-key evil-normal-state-map "L" 'evil-end-of-line)
(define-key evil-operator-state-map "L" 'evil-jump-item)
(define-key evil-visual-state-map "L" 'evil-end-of-line)
(define-key evil-normal-state-map "M" 'evil-jump-item)
(define-key evil-operator-state-map "M" 'evil-jump-item)
(define-key evil-visual-state-map "M" 'evil-jump-item)

(defun my-evil-yank-to-end-of-line ()
  "Yank from cursor position to end of line."
  (interactive)
  (evil-yank (point) (point-at-eol)))

(define-key evil-normal-state-map "g;" 'evil-ex)
(define-key evil-visual-state-map "g;" 'evil-ex)
(define-key evil-normal-state-map "gs" 'evil-write)
(define-key evil-normal-state-map "Y" 'my-evil-yank-to-end-of-line)

;;; Leader layer.
(defun my-evil-edit-dot-emacs ()
  "Edit emacs init file."
  (interactive)
  (evil-edit "~/etsi/emacs/.emacs.d/init.el"))

(define-key evil-normal-state-map (kbd "SPC SPC") 'recenter-top-bottom)
(define-key evil-normal-state-map (kbd "SPC d") 'kill-this-buffer)
(define-key evil-normal-state-map (kbd "SPC ei") 'my-evil-edit-dot-emacs)
(define-key evil-normal-state-map (kbd "SPC f") 'evil-fill-and-move)
(define-key evil-visual-state-map (kbd "SPC f") 'evil-fill-and-move)
(define-key evil-normal-state-map (kbd "SPC h") 'help)
(define-key evil-normal-state-map (kbd "SPC l") 'ibuffer)
(define-key evil-normal-state-map (kbd "SPC q") 'evil-command-window-ex)
(define-key evil-visual-state-map (kbd "SPC q") 'evil-command-window-ex)
(define-key evil-normal-state-map (kbd "SPC s") 'evil-window-split)
(define-key evil-normal-state-map (kbd "SPC v") 'evil-window-vsplit)
(define-key evil-normal-state-map (kbd "SPC we") 'balance-windows)
(define-key evil-normal-state-map (kbd "SPC wo") 'delete-other-windows)
(define-key evil-normal-state-map (kbd "SPC x") 'execute-extended-command)
(define-key evil-visual-state-map (kbd "SPC x") 'execute-extended-command)

;;; Generic special.
(define-key evil-normal-state-map "gcc" 'evilnc-comment-or-uncomment-lines)
(define-key evil-visual-state-map "gc" 'comment-or-uncomment-region)
(define-key evil-normal-state-map "gm" 'magit-status)

;;; Special.
;; Company mode and Yasnippet.
(evil-define-key 'insert yas-minor-mode-map (kbd "C-s") 'yas-expand)
(evil-define-key 'insert yas-minor-mode-map (kbd "C-n") 'yas-next-field)
(evil-define-key 'insert yas-minor-mode-map (kbd "C-p") 'yas-prev-field)

(eval-after-load 'company
  '(progn
     ;; Disable default mappings.
     (define-key yas-minor-mode-map (kbd "<tab>") nil)
     (define-key yas-minor-mode-map (kbd "TAB") nil)
     (define-key company-active-map [tab] nil)
     (define-key company-active-map (kbd "TAB") nil)
     (define-key company-active-map "\C-w" nil)
     (define-key company-active-map "\C-s" nil)

     (evil-define-key 'insert company-mode-map [tab] 'company-complete-common-or-cycle)
     (evil-define-key 'insert company-mode-map (kbd "S-<iso-lefttab>") 'company-select-previous)
     (evil-define-key 'insert company-active-map (kbd "C-w") 'evil-delete-backward-word)))

;; Other.
(add-to-list 'load-path "~/etsi/emacs/.emacs.d/kb")
(require 'ibuffer-kb)
; (require 'magit-kb)

;;;; Leftover mode activation.
(evil-mode t)
(global-evil-surround-mode t)
(global-evil-matchit-mode t)
