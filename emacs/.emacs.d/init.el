;; TODO: Autocompletion.
;; TODO: Snippets.
;; TODO: Folding.
;; TODO: Statusbar colors and customisation.
;; TODO: Magit.
;; TODO: Pdf viewing.
;; TODO: Filesystem navigation, opening files, managing buffers etc.
;; TODO: Helm.
;; TODO: Terminal/eshell/zsh.
;; TODO: Pandoc mode.
;; TODO: Use use-package to lazy load packages.
;; TODO: Figure out how to gracefully stop emacs daemon on shutdown.
;; TODO: Emmet.
;; TODO: Learn elisp.
;; TODO: Expand region.
;; TODO: Multiple cursors.
;; TODO: MPD client.
;; TODO: Image viewer.
;; TODO: IRC.
;; TODO: Mail.
;; TODO: RSS.
;; TODO: Bittorrent client.
;; TODO: Highlight TODOs.
;; TODO: Figure out how to show trailing newlines.
;; TODO: Tab in insert mode should insert two spaces.

;; Packages.
;; TODO: Automatically load all subdirectories (non-recursively) under
;; 'emacs-packages'. Or otherwise use some elisp to reduce this into a list of
;; package names.
(add-to-list 'load-path "~/etsi/emacs-packages/undo-tree")
(add-to-list 'load-path "~/etsi/emacs-packages/goto-chg")
(add-to-list 'load-path "~/etsi/emacs-packages/evil")
(add-to-list 'load-path "~/etsi/emacs-packages/fill-column-indicator")
(add-to-list 'load-path "~/etsi/emacs-packages/evil-surround")
(add-to-list 'load-path "~/etsi/emacs-packages/auto-complete")
(add-to-list 'load-path "~/etsi/emacs-packages/auto-complete/.cask/24.5.1/elpa/popup-20150626.711")
(add-to-list 'custom-theme-load-path "~/etsi/emacs-packages/themes/sunburst")

(setq evil-want-C-u-scroll t)
(setq evil-cross-lines t)
(setq evil-shift-width 2)

(add-hook 'python-mode-hook
  (function (lambda ()
    (setq evil-shift-width python-indent))))

(require 'evil)
(require 'fill-column-indicator)
(require 'paren)
(require 'ibuffer)
(require 'evil-surround)
(require 'auto-complete-config)
; (require 'company)

;; Auto-Complete.
(add-to-list 'ac-dictionary-directories "~/etsi/emacs-packages/auto-complete/dict")
(ac-config-default)

;; Company mode.
; (setq company-idle-delay 0)

;; Appearance.
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

;; Ibuffer.
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

;; Theme.
(load-theme 'sunburst t)

;; TODO: Use variables for color codes.
(custom-set-faces
 ;; TODO: Remove this linum line sometime.
 `(linum ((t (:foreground, "#666"))))
 `(hl-line ((t (:background, "#222")))))

(set-face-attribute 'fringe nil :background "#222")
(set-face-attribute 'isearch nil :foreground "#111")
(set-face-attribute 'lazy-highlight nil :foreground "#111")
(set-face-attribute 'vertical-border nil :foreground "#111")
(set-face-attribute 'whitespace-tab nil :background "#420e09")
(set-face-attribute 'whitespace-trailing nil :background "#420e09")

(set-face-background 'show-paren-match "blue")
(set-face-foreground 'show-paren-match "#ddd")

(setq fci-rule-color "#222")

;; Basic.
(setq auto-save-default nil)
(setq create-lockfiles nil)
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

;; Leftover mode activation.
; (add-hook 'after-init-hook 'global-company-mode)
(evil-mode t)
(global-evil-surround-mode t)

;; Keybindings.
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

;; Paves the way for "," to be used as 'leader'.
(define-key evil-normal-state-map "\\" 'evil-repeat-find-char-reverse)
(define-key evil-visual-state-map "\\" 'evil-repeat-find-char-reverse)

(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
(define-key evil-normal-state-map "H" 'evil-first-non-blank)
(define-key evil-visual-state-map "H" 'evil-first-non-blank)
(define-key evil-normal-state-map "L" 'evil-end-of-line)
(define-key evil-visual-state-map "L" 'evil-end-of-line)
(define-key evil-normal-state-map "M" 'evil-jump-item)
(define-key evil-operator-state-map "M" 'evil-jump-item)
(define-key evil-visual-state-map "M" 'evil-jump-item)

(defun my-evil-yank-to-end-of-line ()
  "Yank from cursor position to end of line."
  (interactive)
  (evil-yank (point) (point-at-eol)))

(define-key evil-normal-state-map [tab] 'evil-ex)
(define-key evil-visual-state-map [tab] 'evil-ex)
(define-key evil-normal-state-map "gs" 'evil-write)
(define-key evil-normal-state-map "Y" 'my-evil-yank-to-end-of-line)

(defun my-evil-command-window-ex ()
  "Open evil ex command window and move one line up."
  (interactive)
  (evil-command-window-ex)
  (evil-previous-line))

(defun my-evil-command-window-search-forward ()
  "Open evil search forward command window and move one line up."
  (interactive)
  (evil-command-window-search-forward)
  (evil-previous-line))

(define-key evil-normal-state-map ",," 'evil-switch-to-windows-last-buffer)
(define-key evil-normal-state-map ",." 'my-evil-command-window-search-forward)
(define-key evil-normal-state-map ",f" 'fill-paragraph)
(define-key evil-visual-state-map ",f" 'fill-paragraph)
(define-key evil-normal-state-map ",l" 'ibuffer)
(define-key evil-normal-state-map ",q" 'my-evil-command-window-ex)
(define-key evil-visual-state-map ",q" 'my-evil-command-window-ex)
(define-key evil-normal-state-map ",we" 'balance-windows)
(define-key evil-normal-state-map ",x" 'execute-extended-command)
(define-key evil-visual-state-map ",x" 'execute-extended-command)
(define-key evil-normal-state-map ",z" 'recenter-top-bottom)

; (eval-after-load 'company
;   '(progn
;      (define-key evil-insert-state-map [tab] 'company-select-next)
;      (define-key evil-insert-state-map (kbd "TAB") 'company-select-next)))
; (define-key evil-insert-state-map (kbd "S-<iso-lefttab>") 'company-select-previous)

(eval-after-load 'ibuffer
  '(progn
     (evil-set-initial-state 'ibuffer-mode 'normal)
     (evil-define-key 'normal ibuffer-mode-map
       (kbd "0") 'digit-argument
       (kbd "1") 'digit-argument
       (kbd "2") 'digit-argument
       (kbd "3") 'digit-argument
       (kbd "4") 'digit-argument
       (kbd "5") 'digit-argument
       (kbd "6") 'digit-argument
       (kbd "7") 'digit-argument
       (kbd "8") 'digit-argument
       (kbd "9") 'digit-argument

       (kbd "m") 'ibuffer-mark-forward
       (kbd "t") 'ibuffer-toggle-marks
       (kbd "u") 'ibuffer-unmark-forward
       (kbd "=") 'ibuffer-diff-with-file
       (kbd "j") 'evil-next-line
       (kbd "J") 'ibuffer-jump-to-buffer
       (kbd "M-g") 'ibuffer-jump-to-buffer
       (kbd "M-s a C-s") 'ibuffer-do-isearch
       (kbd "M-s a M-C-s") 'ibuffer-do-isearch-regexp
       (kbd "M-s a C-o") 'ibuffer-do-occur
       (kbd "DEL") 'ibuffer-unmark-backward
       (kbd "M-DEL") 'ibuffer-unmark-all
       (kbd "* *") 'ibuffer-unmark-all
       (kbd "* M") 'ibuffer-mark-by-mode
       (kbd "* m") 'ibuffer-mark-modified-buffers
       (kbd "* u") 'ibuffer-mark-unsaved-buffers
       (kbd "* s") 'ibuffer-mark-special-buffers
       (kbd "* r") 'ibuffer-mark-read-only-buffers
       (kbd "* /") 'ibuffer-mark-dired-buffers
       (kbd "* e") 'ibuffer-mark-dissociated-buffers
       (kbd "* h") 'ibuffer-mark-help-buffers
       (kbd "* z") 'ibuffer-mark-compressed-file-buffers
       (kbd ".") 'ibuffer-mark-old-buffers

       (kbd "d") 'ibuffer-mark-for-delete
       (kbd "C-d") 'evil-scroll-down
       (kbd "k") 'evil-previous-line
       (kbd "x") 'ibuffer-do-kill-on-deletion-marks

       ;; Immediate operations.
       (kbd "n") 'ibuffer-forward-line
       (kbd "SPC") 'forward-line
       (kbd "p") 'ibuffer-backward-line
       (kbd "M-}") 'ibuffer-forward-next-marked
       (kbd "M-{") 'ibuffer-backwards-next-marked
       (kbd "l") 'ibuffer-visit-buffer
       (kbd "g") 'ibuffer-update
       "`" 'ibuffer-switch-format
       "-" 'ibuffer-add-to-tmp-hide
       "+" 'ibuffer-add-to-tmp-show
       "b" 'ibuffer-bury-buffer
       (kbd ",") 'ibuffer-toggle-sorting-mode
       (kbd "s i") 'ibuffer-invert-sorting
       (kbd "s a") 'ibuffer-do-sort-by-alphabetic
       (kbd "s v") 'ibuffer-do-sort-by-recency
       (kbd "s s") 'ibuffer-do-sort-by-size
       (kbd "s f") 'ibuffer-do-sort-by-filename/process
       (kbd "s m") 'ibuffer-do-sort-by-major-mode

       (kbd "/ m") 'ibuffer-filter-by-used-mode
       (kbd "/ M") 'ibuffer-filter-by-derived-mode
       (kbd "/ n") 'ibuffer-filter-by-name
       (kbd "/ c") 'ibuffer-filter-by-content
       (kbd "/ e") 'ibuffer-filter-by-predicate
       (kbd "/ f") 'ibuffer-filter-by-filename
       (kbd "/ >") 'ibuffer-filter-by-size-gt
       (kbd "/ <") 'ibuffer-filter-by-size-lt
       (kbd "/ r") 'ibuffer-switch-to-saved-filters
       (kbd "/ a") 'ibuffer-add-saved-filters
       (kbd "/ x") 'ibuffer-delete-saved-filters
       (kbd "/ d") 'ibuffer-decompose-filter
       (kbd "/ s") 'ibuffer-save-filters
       (kbd "/ p") 'ibuffer-pop-filter
       (kbd "/ !") 'ibuffer-negate-filter
       (kbd "/ t") 'ibuffer-exchange-filters
       (kbd "/ TAB") 'ibuffer-exchange-filters
       (kbd "/ o") 'ibuffer-or-filter
       (kbd "/ g") 'ibuffer-filters-to-filter-group
       (kbd "/ P") 'ibuffer-pop-filter-group
       (kbd "/ D") 'ibuffer-decompose-filter-group
       (kbd "/ /") 'ibuffer-filter-disable

       (kbd "M-n") 'ibuffer-forward-filter-group
       "\t" 'ibuffer-forward-filter-group
       (kbd "M-p") 'ibuffer-backward-filter-group
       [backtab] 'ibuffer-backward-filter-group
       (kbd "M-j") 'ibuffer-jump-to-filter-group
       (kbd "C-k") 'evil-window-up
       (kbd "C-y") 'ibuffer-yank
       (kbd "/ S") 'ibuffer-save-filter-groups
       (kbd "/ R") 'ibuffer-switch-to-saved-filter-groups
       (kbd "/ X") 'ibuffer-delete-saved-filter-groups
       (kbd "/ \\") 'ibuffer-clear-filter-groups

       [escape] 'ibuffer-quit
       (kbd "q") 'ibuffer-quit
       (kbd "h") 'describe-mode
       (kbd "?") 'describe-mode

       (kbd "% n") 'ibuffer-mark-by-name-regexp
       (kbd "% m") 'ibuffer-mark-by-mode-regexp
       (kbd "% f") 'ibuffer-mark-by-file-name-regexp

       (kbd "C-t") 'ibuffer-visit-tags-table

       (kbd "|") 'ibuffer-do-shell-command-pipe
       (kbd "!") 'ibuffer-do-shell-command-file
       (kbd "~") 'ibuffer-do-toggle-modified

       ;; Marked operations
       (kbd "A") 'ibuffer-do-view
       (kbd "D") 'ibuffer-do-delete
       (kbd "E") 'ibuffer-do-eval
       (kbd "F") 'ibuffer-do-shell-command-file
       (kbd "I") 'ibuffer-do-query-replace-regexp
       (kbd "H") 'evil-first-non-blank
       (kbd "N") 'ibuffer-do-shell-command-pipe-replace
       (kbd "M") 'evil-jump-item
       (kbd "O") 'ibuffer-do-occur
       (kbd "P") 'ibuffer-do-print
       (kbd "Q") 'ibuffer-do-query-replace
       (kbd "R") 'ibuffer-do-rename-uniquely
       (kbd "S") 'ibuffer-do-save
       (kbd "T") 'ibuffer-do-toggle-read-only
       (kbd "U") 'ibuffer-do-replace-regexp
       (kbd "V") 'ibuffer-do-revert
       (kbd "W") 'ibuffer-do-view-and-eval
       (kbd "X") 'ibuffer-do-shell-command-pipe

       ;; 'ibuffer-do-kill-lines is the function deleted here.
       (kbd "w") 'ibuffer-copy-filename-as-kill

       (kbd "RET") 'ibuffer-visit-buffer
       (kbd "e") 'ibuffer-visit-buffer
       (kbd "f") 'ibuffer-visit-buffer
       (kbd "C-x C-f") 'ibuffer-find-file
       (kbd "o") 'ibuffer-visit-buffer-other-window
       (kbd "C-o") 'ibuffer-visit-buffer-other-window-noselect
       (kbd "M-o") 'ibuffer-visit-buffer-1-window
       (kbd "v") 'ibuffer-do-view
       (kbd "C-x v") 'ibuffer-do-view-horizontally
       (kbd "C-c C-a") 'ibuffer-auto-mode
       (kbd "C-x 4 RET") 'ibuffer-visit-buffer-other-window
       (kbd "C-x 5 RET") 'ibuffer-visit-buffer-other-frame)))
