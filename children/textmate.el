(require 'textmate)
(tm/initialize)

;; (define-skeleton ruby-hash-skeleton
;;   "Inserts a ruby hash with brackets. TODO: Check if inside a string" nil
;;   ?#?{ _ ?})
;; ;
;; ;; (add-hook 'ruby-mode-hook
;; ;; 	  (set-key "#" 'ruby-hash-skeleton))

;; (require 'autopair)
;; (autopair-global-mode)
;; (setq autopair-autowrap t)
;; (setq autopair-blink t)

;; MOVED to ruby-el
;; (defun ruby-insert-hash-string ()
;;   "Inserts a #{} but only if inside a string"
;;   (let ((stringp (fourth (syntax-ppss)))
;; 	(skeleton-end-newline nil))
;;     (if stringp
;; 	(skeleton-insert '(nil ?# ?{ _ ?} nil))
;;       (skeleton-insert '(nil ?# _)))))

;; (add-hook 'ruby-mode-hook
;; 	  (lambda ()
;; 	    ()))
