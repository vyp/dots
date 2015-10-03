(setq
 recipe-list
 `((bind-key
    :fetcher file
    :files ("bind-key.el")
    :path ,(expand-file-name "~/ui/vendor/emacs/use-package"))

   (company
    :fetcher file
    :path ,(expand-file-name "~/ui/vendor/emacs/company-mode"))

   (diminish
    :fetcher file
    :path ,(expand-file-name "~/ui/vendor/emacs/diminish"))

   (evil
    :fetcher file
    :path ,(expand-file-name "~/ui/vendor/emacs/evil"))

   (evil-matchit
    :fetcher file
    :path ,(expand-file-name "~/ui/vendor/emacs/evil-matchit"))

   (evil-nerd-commenter
    :fetcher file
    :path ,(expand-file-name "~/ui/vendor/emacs/evil-nerd-commenter"))

   (evil-quick-scope
    :fetcher file
    :path ,(expand-file-name "~/gh/evil-quick-scope"))

   (evil-surround
    :fetcher file
    :path ,(expand-file-name "~/ui/vendor/emacs/evil-surround"))

   (fill-column-indicator
    :fetcher file
    :files ("fill-column-indicator.el")
    :path ,(expand-file-name "~/ui/vendor/emacs/Fill-Column-Indicator"))

   (goto-chg
    :fetcher file
    :path ,(expand-file-name "~/ui/vendor/emacs/goto-chg"))

   (ov
    :fetcher file
    :path ,(expand-file-name "~/ui/vendor/emacs/ov.el"))

   (package-build
    :fetcher file
    :files ("package-build.el")
    :path ,(expand-file-name "~/ui/vendor/emacs/package-build"))

   (quelpa
    :fetcher file
    :files ("quelpa.el")
    :path ,(expand-file-name "~/gh/forks/quelpa"))

   (quelpa-use-package
    :fetcher file
    :path ,(expand-file-name "~/ui/vendor/emacs/quelpa-use-package"))

   (undo-tree
    :fetcher file
    :path ,(expand-file-name "~/ui/vendor/emacs/undo-tree"))

   (use-package
    :fetcher file
    :files ("use-package.el")
    :path ,(expand-file-name "~/ui/vendor/emacs/use-package"))

   (yasnippet
    :fetcher file
    :files ("yasnippet.el")
    :path ,(expand-file-name "~/ui/vendor/emacs/yasnippet"))))

(provide 'recipe-list)
