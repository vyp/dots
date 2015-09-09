(evil-define-key 'motion ibuffer-mode-map
  (kbd "h") 'evil-backward-char
  (kbd "l") 'evil-forward-char
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
  (kbd "SPC t") 'ibuffer-toggle-marks
  (kbd "u") 'ibuffer-unmark-forward
  (kbd "=") 'ibuffer-diff-with-file
  (kbd "J") 'ibuffer-jump-to-buffer
  ;; (kbd "M-s a C-s") 'ibuffer-do-isearch
  ;; (kbd "M-s a M-C-s") 'ibuffer-do-isearch-regexp
  ;; (kbd "M-s a C-o") 'ibuffer-do-occur
  (kbd "DEL") 'ibuffer-unmark-backward
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
  (kbd "x") 'ibuffer-do-kill-on-deletion-marks

  ;; Immediate operations.
  (kbd "C-f") 'ibuffer-forward-next-marked
  (kbd "C-s") 'ibuffer-backwards-next-marked
  (kbd "g") 'ibuffer-update
  "`" 'ibuffer-switch-format
  "-" 'ibuffer-add-to-tmp-hide
  "+" 'ibuffer-add-to-tmp-show
  "D" 'ibuffer-bury-buffer
  (kbd "s t") 'ibuffer-toggle-sorting-mode
  (kbd "s i") 'ibuffer-invert-sorting
  (kbd "s a") 'ibuffer-do-sort-by-alphabetic
  (kbd "s v") 'ibuffer-do-sort-by-recency
  (kbd "s s") 'ibuffer-do-sort-by-size
  (kbd "s f") 'ibuffer-do-sort-by-filename/process
  (kbd "s m") 'ibuffer-do-sort-by-major-mode

  (kbd "SPC f m") 'ibuffer-filter-by-used-mode
  (kbd "SPC f M") 'ibuffer-filter-by-derived-mode
  (kbd "SPC f n") 'ibuffer-filter-by-name
  (kbd "SPC f c") 'ibuffer-filter-by-content
  (kbd "SPC f e") 'ibuffer-filter-by-predicate
  (kbd "SPC f f") 'ibuffer-filter-by-filename
  (kbd "SPC f >") 'ibuffer-filter-by-size-gt
  (kbd "SPC f <") 'ibuffer-filter-by-size-lt
  (kbd "SPC f r") 'ibuffer-switch-to-saved-filters
  (kbd "SPC f a") 'ibuffer-add-saved-filters
  (kbd "SPC f x") 'ibuffer-delete-saved-filters
  (kbd "SPC f d") 'ibuffer-decompose-filter
  (kbd "SPC f s") 'ibuffer-save-filters
  (kbd "SPC f p") 'ibuffer-pop-filter
  (kbd "SPC f !") 'ibuffer-negate-filter
  (kbd "SPC f t") 'ibuffer-exchange-filters
  (kbd "SPC f TAB") 'ibuffer-exchange-filters
  (kbd "SPC f o") 'ibuffer-or-filter
  (kbd "SPC f g") 'ibuffer-filters-to-filter-group
  (kbd "SPC f P") 'ibuffer-pop-filter-group
  (kbd "SPC f D") 'ibuffer-decompose-filter-group
  (kbd "SPC f /") 'ibuffer-filter-disable

  (kbd "C-n") 'ibuffer-forward-filter-group
  (kbd "C-p") 'ibuffer-backward-filter-group
  (kbd "SPC j") 'ibuffer-jump-to-filter-group
  (kbd "C-y") 'ibuffer-yank
  (kbd "SPC f S") 'ibuffer-save-filter-groups
  (kbd "SPC f R") 'ibuffer-switch-to-saved-filter-groups
  (kbd "SPC f X") 'ibuffer-delete-saved-filter-groups
  (kbd "SPC f \\") 'ibuffer-clear-filter-groups

  [escape] 'ibuffer-quit
  (kbd "q") 'ibuffer-quit
  "SPC ." 'describe-mode

  (kbd "SPC r n") 'ibuffer-mark-by-name-regexp
  (kbd "SPC r m") 'ibuffer-mark-by-mode-regexp
  (kbd "SPC r f") 'ibuffer-mark-by-file-name-regexp

  (kbd "C-t") 'ibuffer-visit-tags-table

  (kbd "|") 'ibuffer-do-shell-command-pipe
  (kbd "!") 'ibuffer-do-shell-command-file
  (kbd "~") 'ibuffer-do-toggle-modified

  ;; Marked operations.
  (kbd "A") 'ibuffer-do-view
  (kbd "D") 'ibuffer-do-delete
  ;; (kbd "E") 'ibuffer-do-eval
  ;; (kbd "F") 'ibuffer-do-shell-command-file
  (kbd "I") 'ibuffer-do-query-replace-regexp
  (kbd "N") 'ibuffer-do-shell-command-pipe-replace
  (kbd "O") 'ibuffer-do-occur
  (kbd "P") 'ibuffer-do-print
  (kbd "Q") 'ibuffer-do-query-replace
  (kbd "R") 'ibuffer-do-rename-uniquely
  (kbd "S") 'ibuffer-do-save
  ;; (kbd "T") 'ibuffer-do-toggle-read-only
  (kbd "U") 'ibuffer-do-replace-regexp
  ;; (kbd "V") 'ibuffer-do-revert
  ;; (kbd "W") 'ibuffer-do-view-and-eval
  (kbd "X") 'ibuffer-do-shell-command-pipe

  ;; 'ibuffer-do-kill-lines is the function deleted here.
  (kbd "SPC c") 'ibuffer-copy-filename-as-kill

  (kbd "RET") 'ibuffer-visit-buffer
  ;; (kbd "C-x C-f") 'ibuffer-find-file
  (kbd "o") 'ibuffer-visit-buffer-other-window)
  ;; (kbd "C-o") 'ibuffer-visit-buffer-other-window-noselect
  ;; (kbd "M-o") 'ibuffer-visit-buffer-1-window
  ;; (kbd "C-x v") 'ibuffer-do-view-horizontally
  ;; (kbd "C-c C-a") 'ibuffer-auto-mode
  ;; (kbd "C-x 4 RET") 'ibuffer-visit-buffer-other-window
  ;; (kbd "C-x 5 RET") 'ibuffer-visit-buffer-other-frame)

(provide 'ibuffer-custom-keybindings)
