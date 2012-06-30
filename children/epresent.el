(autoload 'epresent-run "epresent")

(add-hook 'org-mode-hook
        (function
         (lambda ()
           (setq truncate-lines nil)
           (setq word-wrap t)
           (define-key org-mode-map [f3]
             'epresent-run)
           )))