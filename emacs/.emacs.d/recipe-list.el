(setq
 recipe-list
 '((bind-key
    :fetcher file
    :files ("bind-key.el")
    :path "~/ui/vendor/emacs/use-package")

   (company
    :fetcher file
    :path "~/ui/vendor/emacs/company-mode")

   (diminish
    :fetcher file
    :path "~/ui/vendor/emacs/diminish")

   (evil
    :fetcher file
    :path "~/ui/vendor/emacs/evil")

   (evil-matchit
    :fetcher file
    :path "~/ui/vendor/emacs/evil-matchit")

   (evil-nerd-commenter
    :fetcher file
    :path "~/ui/vendor/emacs/evil-nerd-commenter")

   (evil-quick-scope
    :fetcher file
    :path "~/gh/evil-quick-scope")

   (evil-surround
    :fetcher file
    :path "~/ui/vendor/emacs/evil-surround")

   (fill-column-indicator
    :fetcher file
    :files ("fill-column-indicator.el")
    :path "~/ui/vendor/emacs/Fill-Column-Indicator")

   (goto-chg
    :fetcher file
    :path "~/ui/vendor/emacs/goto-chg")

   (haskell-mode
    :fetcher file
    :path "~/ui/vendor/emacs/haskell-mode")

   (ov
    :fetcher file
    :path "~/ui/vendor/emacs/ov.el")

   (package-build
    :fetcher file
    :files ("package-build.el")
    :path "~/ui/vendor/emacs/package-build")

   (quelpa
    :fetcher file
    :files ("quelpa.el")
    :path "~/ui/vendor/emacs/quelpa")

   (quelpa-use-package
    :fetcher file
    :path "~/ui/vendor/emacs/quelpa-use-package")

   (rust-mode
    :fetcher file
    :files ("rust-mode.el")
    :path "~/ui/vendor/emacs/rust-mode")

   (undo-tree
    :fetcher file
    :path "~/ui/vendor/emacs/undo-tree")

   (use-package
    :fetcher file
    :files ("use-package.el")
    :path "~/ui/vendor/emacs/use-package")

   (yasnippet
    :fetcher file
    :files ("yasnippet.el")
    :path "~/ui/vendor/emacs/yasnippet")))

(provide 'recipe-list)
