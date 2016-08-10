(require 'ox)

(setq org-entities-ascii-explanatory t
      org-export-with-smart-quotes t
      org-latex-packages-alist '(("" "grffile" t))
      org-latex-pdf-process
      (append
       (make-list 3 "lualatex -interaction nonstopmode -output-directory %o %f")
       '("mv %o%b.pdf %o.%b.pdf"
         "mv %o%b.tex %o.%b.tex"
         "rm %o%b.aux"
         "rm %o%b.log"
         "rm %o%b.out"
         "rm %o%b.toc")))
