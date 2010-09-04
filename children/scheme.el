(require 'quack)

;; TODO: Customize Quack here. For instance, the menu should not be present unless we are actually editing a scheme file.

(add-hook 'scheme-mode-hook
	  '(lambda ()
	     (maybe-require 'highlight-parentheses
			    (highlight-parentheses-mode t))))
