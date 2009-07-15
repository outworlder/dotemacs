;;; Emacs/W3 Configuration
(setq load-path (cons "~/.emacs.children/support/w3/lisp" load-path))
(condition-case () (require 'w3-auto "w3-auto") (error nil))
