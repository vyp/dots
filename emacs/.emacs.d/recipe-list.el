(setq
 recipe-list
 '((auto-complete
    :fetcher file
    :files ("*.el")
    :path "~/ui/vendor/emacs/auto-complete")

   (bind-key
    :fetcher file
    :files ("bind-key.el")
    :path "~/ui/vendor/emacs/use-package")

   (dash
    :fetcher file
    :files ("dash.el")
    :path "~/ui/vendor/emacs/dash.el")

   (diminish
    :fetcher file
    :path "~/ui/vendor/emacs/diminish.el")

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

   (fuzzy
    :fetcher file
    :files ("fuzzy.el")
    :path "~/ui/vendor/emacs/fuzzy-el")

   (goto-chg
    :fetcher file
    :path "~/ui/vendor/emacs/goto-chg")

   (haskell-mode
    :fetcher file
    :path "~/ui/vendor/emacs/haskell-mode")

   (nyan-mode
    :fetcher file
    :files ("nyan-mode.el" "img" "mus")
    :path "~/ui/vendor/emacs/nyan-mode")

   (org-bullets
    :fetcher file
    :path "~/ui/vendor/emacs/org-bullets")

   (ov
    :fetcher file
    :path "~/ui/vendor/emacs/ov.el")

   (package-build
    :fetcher file
    :files ("package-build.el")
    :path "~/ui/vendor/emacs/package-build")

   (popup
    :fetcher file
    :files ("popup.el")
    :path "~/ui/vendor/emacs/popup-el")

   (powerline
    :fetcher file
    :path "~/ui/vendor/emacs/powerline")

   (quelpa
    :fetcher file
    :files ("quelpa.el")
    :path "~/gh/quelpa")

   (quelpa-use-package
    :fetcher file
    :path "~/ui/vendor/emacs/quelpa-use-package")

   (rust-mode
    :fetcher file
    :files ("rust-mode.el")
    :path "~/ui/vendor/emacs/rust-mode")

   (s
    :fetcher file
    :files ("s.el")
    :path "~/ui/vendor/emacs/s.el")

   (spaceline
    :fetcher file
    :path "~/ui/vendor/emacs/spaceline")

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
