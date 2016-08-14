(let ((theme-path "~/gh/gruvbox-emacs"))
  (dolist (list '(load-path custom-theme-load-path))
    (add-to-list list theme-path)))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (load-theme 'gruvbox-light t))))
  (load-theme 'gruvbox-light t))
