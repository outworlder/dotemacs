;; Loading Quack
;;(require 'quack)

;;-----------------------------------------------------------------------------
;; Setting  the Emacs Lisp style
;;-----------------------------------------------------------------------------

(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (modify-syntax-entry ?- "w")       ; now '-' is not considered a word-delimiter
	     (highlight-parentheses-mode t)
             ))
