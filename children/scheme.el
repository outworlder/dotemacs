;(require 'quack)

;; TODO: Customize Quack here. For instance, the menu should not be present unless we are actually editing a scheme file.

(require 'cluck)

(add-hook 'scheme-mode-hook
	  '(lambda ()
	     (maybe-require 'highlight-parentheses
			    (highlight-parentheses-mode t))))

;;; Using Geiser (Experimental support)

;(load-file "~/Documentos/Projetos/geiser/elisp/geiser.el")

(add-to-list 'load-path "~/.emacs.children/support/slime") ; your SLIME directory

(require 'slime)
(slime-setup '(slime-repl slime-autodoc))

(autoload 'chicken-slime "chicken-slime" "SWANK backend for Chicken" t)
(setq swank-chicken-path "~/.emacs.children/support/swank-chicken/swank-chicken.scm")

(add-hook 'scheme-mode-hook
          (lambda ()
            (slime-mode t)))