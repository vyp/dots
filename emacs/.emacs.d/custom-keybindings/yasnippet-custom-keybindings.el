(evil-define-key 'insert yas-minor-mode-map (kbd "C-s") 'yas-expand)
(evil-define-key 'insert yas-minor-mode-map (kbd "C-n") 'yas-next-field)
(evil-define-key 'insert yas-minor-mode-map (kbd "C-p") 'yas-prev-field)

;; Disable default mappings.
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)

(provide 'yasnippet-custom-keybindings)
