;; Disable default mappings.
(define-key company-active-map [tab] nil)
(define-key company-active-map (kbd "TAB") nil)
(define-key company-active-map "\C-w" nil)
(define-key company-active-map "\C-s" nil)

(evil-define-key 'insert company-mode-map [tab] 'company-complete-common-or-cycle)
(evil-define-key 'insert company-mode-map (kbd "S-<iso-lefttab>") 'company-select-previous)
(evil-define-key 'insert company-active-map (kbd "C-w") 'evil-delete-backward-word)

(provide 'company-custom-keybindings)
