(let ((theme-path "~/gh/themes/redbelly"))
  (dolist (list '(load-path custom-theme-load-path))
    (add-to-list list theme-path)))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (load-theme 'redbelly t))))
  (load-theme 'redbelly t))

(provide 'my-currently-chosen-theme)
