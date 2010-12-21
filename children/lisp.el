;; (setq inferior-lisp-program "/usr/local/bin/sbcl") ; your Lisp system
;; (add-to-list 'load-path "~/Documents/Projects/dotemacs/children/support/slime") ; your SLIME directory
;; (add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
;; (add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
;; ;; Optionally, specify the lisp program you are using. Default is "lisp"
;; (setq inferior-lisp-program "sbcl --control-stack-size 32")
;; (require 'slime)
;; (slime-setup '(slime-fancy slime-asdf))


(setq org-agenda-files (list "~/Documents/Projects/citycouncil/citycouncil.org"))
(setq org-log-done 'time)
