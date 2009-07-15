(require 'anything)
(require 'anything-config)
(require 'anything-auto-install)

;; (defvar anything-c-source-M-x
;;   (anything-c-define-dummy-source
;;    "M-x"
;;    #'anything-c-dummy-candidate
;;    '(type . command)))

(require 'anything-complete)
;; ;; Automatically collect symbols by 150 secs
(anything-lisp-complete-symbol-set-timer 150)
;; ;; replace completion commands with `anything'
(anything-read-string-mode 1)
;; ;; Bind C-o to complete shell history
(anything-complete-shell-history-setup-key "\C-o")

;; (defun anything-append (stuff)
;;   (append 


(require 'lacarte t)

(if (featurep 'lacarte)
    (progn
    (defvar anything-c-source-lacarte
      '((name . "Lacarte")
	(candidates
	 . (lambda () (delete '(nil) (lacarte-get-overall-menu-item-alist))))
	(candidate-number-limit . 9999)
	(action . call-interactively)))))


