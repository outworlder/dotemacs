(maybe-require 'anything)
(maybe-require 'anything-config)
(maybe-require 'anything-auto-install)

;; (defvar anything-c-source-M-x
;;   (anything-c-define-dummy-source
;;    "M-x"
;;    #'anything-c-dummy-candidate
;;    '(type . command)))

(maybe-require 'anything-complete)
;; ;; Automatically collect symbols by 150 secs
(anything-lisp-complete-symbol-set-timer 150)
;; ;; replace completion commands with `anything'
(anything-read-string-mode 0)
;; ;; Bind C-o to complete shell history
(anything-complete-shell-history-setup-key "\C-o")

;; (defun anything-append (stuff)
;;   (append 


(maybe-require 'lacarte)

(if (featurep 'lacarte)
    (progn
    (defvar anything-c-source-lacarte
      '((name . "Lacarte")
	(candidates
	 . (lambda () (delete '(nil) (lacarte-get-overall-menu-item-alist))))
	(candidate-number-limit . 9999)
	(action . call-interactively)))))


;; Hackish bug fix:
;; For some reason, anything (when used to find files) will not correctly change directories
;; But it will if the ido-find-file function is used. So this "fixes" it.
;; (if (and (featurep 'ido) (featurep 'anything))
;;     (set-key "C-x C-f" 'ido-find-file))