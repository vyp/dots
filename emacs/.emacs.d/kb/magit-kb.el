(eval-after-load 'magit
  '(progn
     (evil-set-initial-state 'magit-mode 'normal)
     (evil-set-initial-state 'magit-blame-mode 'normal)
     (evil-set-initial-state 'magit-diff-mode 'normal)
     (evil-set-initial-state 'magit-log-mode 'normal)
     (evil-set-initial-state 'magit-log-select-mode 'normal)
     (evil-set-initial-state 'magit-refs-mode 'normal)
     (evil-set-initial-state 'magit-status-mode 'normal)
     (evil-set-initial-state 'git-commit-mode 'normal)
     (evil-set-initial-state 'git-rebase-mode 'normal)
     (evil-set-initial-state 'with-editor-mode 'normal)

     (evil-define-key 'normal magit-mode-map
       "\s"    'magit-section-toggle
       (kbd "C-SPC") 'magit-section-cycle
       (kbd "M-SPC") 'magit-section-cycle-diffs
       (kbd "S-SPC") 'magit-section-cycle-global
       "K"    'magit-section-up
       "\C-n"    'magit-section-forward
       "\C-p"    'magit-section-backward
       "\C-f" 'magit-section-forward-sibling
       "\C-s" 'magit-section-backward-sibling
       "+"    'magit-diff-more-context
       "-"    'magit-diff-less-context
       "0"    'magit-diff-default-context
       "1"    'magit-section-show-level-1
       "2"    'magit-section-show-level-2
       "3"    'magit-section-show-level-3
       "4"    'magit-section-show-level-4
       ; "\M-1" 'magit-section-show-level-1-all
       ; "\M-2" 'magit-section-show-level-2-all
       ; "\M-3" 'magit-section-show-level-3-all
       ; "\M-4" 'magit-section-show-level-4-all
       "g" 'magit-refresh
       "G" 'magit-refresh-all
       "q" 'magit-mode-bury-buffer
       "$" 'magit-process
       "A" 'magit-cherry-pick-popup
       ",b" 'magit-branch-popup
       ; "B" 'magit-bisect-popup
       "c" 'magit-commit-popup
       "d" 'magit-diff-popup
       "D" 'magit-diff-refresh-popup
       ",." 'magit-dispatch-popup
       ; "e" 'magit-ediff-dwim
       ; "E" 'magit-ediff-popup
       ",f" 'magit-fetch-popup
       "p" 'magit-pull-popup
       "i" 'magit-gitignore
       "I" 'magit-gitignore-locally
       "o" 'magit-log-popup
       ; "L" 'magit-toggle-margin
       "m" 'magit-merge-popup
       "R" 'magit-remote-popup
       ",s" 'magit-submodule-popup
       "P" 'magit-push-popup
       "r" 'magit-rebase-popup
       ",t" 'magit-tag-popup
       ; "T" 'magit-notes-popup
       ; [M-return] 'magit-dired-jump
       "\C-," 'magit-diff-show-or-scroll-up
       "\C-." 'magit-diff-show-or-scroll-down
       "s" 'magit-stage-file
       "S" 'magit-stage-modified
       "u" 'magit-unstage-file
       ",ri" 'magit-reset-index
       ; "V" 'magit-revert-popup
       ; "w" 'magit-am-popup
       ; "W" 'magit-patch-popup
       ",rs" 'magit-reset
       ; "y" 'magit-show-refs-popup
       ; "Y" 'magit-cherry
       "z" 'magit-stash-popup
       "Z" 'magit-stash-popup
       ":" 'magit-git-command
       "!" 'magit-run-popup
       ; "\C-xa"  'magit-add-change-log-entry
       ; "\C-x4a" 'magit-add-change-log-entry-other-window
       ; "\C-w"   'magit-copy-as-kill
       ; "\M-w"   'magit-copy-buffer-thing-as-kill
       [remap evil-previous-line] 'evil-previous-visual-line
       [remap evil-next-line] 'evil-next-visual-line)

     (evil-define-key 'normal magit-blame-mode-map
       "\r" 'magit-show-commit
       (kbd "S-SPC") 'magit-diff-show-or-scroll-up
       "\s" 'magit-diff-show-or-scroll-down
       ",b"  'magit-blame-popup
       "\C-n"  'magit-blame-next-chunk
       "\C-f"  'magit-blame-next-chunk-same-commit
       "\C-p"  'magit-blame-previous-chunk
       "\C-s"  'magit-blame-previous-chunk-same-commit
       "q"  'magit-blame-quit
       ",t"  'magit-blame-toggle-headings
       ",ch" 'magit-blame-copy-hash)

     (evil-define-key 'normal magit-diff-mode-map
       "\C-c" 'magit-diff-while-committing
       "\C-s" 'magit-go-backward
       "\C-f" 'magit-go-forward
       "\s" 'scroll-down
       (kbd "S-SPC") 'scroll-up
       "J" 'magit-jump-to-diffstat-or-diff)

     (evil-define-key 'normal magit-file-section-map
       [C-return] 'magit-diff-visit-file-worktree
       "\r" 'magit-diff-visit-file
       "a"  'magit-apply
       "x"  'magit-discard
       "U"  'magit-file-untrack
       "R"  'magit-file-rename
       "s"  'magit-stage
       "u"  'magit-unstage
       ",v"  'magit-reverse)

     (evil-define-key 'normal magit-hunk-section-map
       [C-return] 'magit-diff-visit-file-worktree
       "\r" 'magit-diff-visit-file
       "a"  'magit-apply
       "C"  'magit-commit-add-log
       "x"  'magit-discard
       "s"  'magit-stage
       "u"  'magit-unstage
       ",v"  'magit-reverse)

     (evil-define-key 'normal magit-unstaged-section-map
       "\r" 'magit-diff-unstaged
       "x"  'magit-discard
       "s"  'magit-stage
       "u"  'magit-unstage)

     (evil-define-key 'normal magit-staged-section-map
       "\r" 'magit-diff-unstaged
       "x"  'magit-discard
       "s"  'magit-stage
       "u"  'magit-unstage
       ",v"  'magit-reverse)

     (evil-define-key 'normal magit-log-mode-map
       "\C-c\C-b" 'magit-go-backward
       "\C-c\C-f" 'magit-go-forward
       "+" 'magit-log-show-more-commits
       "q" 'magit-log-bury-buffer)

     (evil-define-key 'normal magit-log-select-mode-map
       ; "\C-c\C-b" 'undefined
       ; "\C-c\C-f" 'undefined
       ; "."        'magit-log-select-pick
       "e"        'magit-log-select-pick
       ; "\C-c\C-c" 'magit-log-select-pick
       "q"        'magit-log-select-quit)
       ; "\C-c\C-k" 'magit-log-select-quit)

     ; (evil-define-key 'normal magit-commit-section-map
     ;   "\r" 'magit-show-commit
     ;   "a"  'magit-cherry-apply
     ;   "v"  'magit-revert-no-commit)

     ; (evil-define-key 'normal magit-module-commit-section-map
     ;   "\r" 'magit-show-commit)

     (evil-define-key 'normal magit-stashes-section-map
       "k"  'magit-stash-clear)

     (evil-define-key 'normal magit-stash-section-map
       "\r" 'magit-stash-show
       "a"  'magit-stash-apply
       "A"  'magit-stash-pop
       "k"  'magit-stash-drop)

     (evil-define-key 'normal magit-status-mode-map
       "jz" 'magit-jump-to-stashes
       "jt" 'magit-jump-to-tracked
       "jn" 'magit-jump-to-untracked
       "ju" 'magit-jump-to-unstaged
       "js" 'magit-jump-to-staged
       "jf" 'magit-jump-to-unpulled
       "jp" 'magit-jump-to-unpushed)

     (evil-define-key 'normal magit-untracked-section-map
       "k"  'magit-discard
       "s"  'magit-stage)

     (evil-define-key 'normal magit-refs-mode-map
       "\C-y" 'magit-refs-set-show-commit-count)

     (evil-define-key 'normal magit-branch-section-map
       "\r" 'magit-visit-ref
       "k"  'magit-branch-delete
       "R"  'magit-branch-rename)

     (evil-define-key 'normal magit-remote-section-map
       "k"  'magit-remote-remove
       "R"  'magit-remote-rename)

     (evil-define-key 'normal magit-tag-section-map
       "\r" 'magit-visit-ref
       "k"  'magit-tag-delete)

     (evil-define-key 'normal git-commit-mode-map
       ; (kbd "M-p")     'git-commit-prev-message
       ; (kbd "M-n")     'git-commit-next-message
       (kbd "C-c C-d") 'magit-diff-while-committing
       (kbd "C-c C-s") 'git-commit-signoff
       (kbd "C-c C-a") 'git-commit-ack
       (kbd "C-c C-t") 'git-commit-test
       (kbd "C-c C-r") 'git-commit-review
       (kbd "C-c C-o") 'git-commit-cc
       (kbd "C-c C-p") 'git-commit-reported
       (kbd "C-c C-i") 'git-commit-suggested
       (kbd "C-c M-s") 'git-commit-save-message)

     (evil-define-key 'normal git-rebase-mode-map
       [remap undo] 'git-rebase-undo
       (kbd "RET") 'git-rebase-show-commit
       (kbd "SPC") 'magit-diff-show-or-scroll-up
       (kbd "x")   'git-rebase-exec
       (kbd "c")   'git-rebase-pick
       (kbd "r")   'git-rebase-reword
       (kbd "w")   'git-rebase-reword
       (kbd "e")   'git-rebase-edit
       (kbd "s")   'git-rebase-squash
       (kbd "f")   'git-rebase-fixup
       (kbd "y")   'git-rebase-insert
       (kbd "k")   'git-rebase-kill-line
       (kbd "C-k") 'git-rebase-kill-line
       (kbd "p")   'git-rebase-backward-line
       (kbd "n")   'forward-line
       ; (kbd "M-p")      'git-rebase-move-line-up
       ; (kbd "M-n")      'git-rebase-move-line-down
       (kbd "M-<up>")   'git-rebase-move-line-up
       (kbd "M-<down>") 'git-rebase-move-line-down
       (kbd "C-x C-t")  'git-rebase-move-line-up)

     (evil-define-key 'normal with-editor-mode-map
       "\C-c\C-c"                   'with-editor-finish
       [remap server-edit]          'with-editor-finish
       "\C-c\C-k"                   'with-editor-cancel
       [remap kill-buffer]          'with-editor-cancel
       [remap ido-kill-buffer]      'with-editor-cancel
       [remap iswitchb-kill-buffer] 'with-editor-cancel)))

(provide 'magit-kb)
