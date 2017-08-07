(require 'ox)

(dolist (form (cdr (read
                    (with-temp-buffer
                      (insert-file-contents
                       (expand-file-name "~/dots/emacs/.emacs.d/init.el"))
                      (format "(progn %s)" (buffer-string))))))
  (when (and (eq 'use-package (car form))
             (eq 'org (cadr form)))
    (eval (plist-get form :init))))
