;; -----------------------------------------------------------------------------
;; Automatically adding login/logout tasks
;; -----------------------------------------------------------------------------

(setq worklog-automatic-login t)
;; (add-hook 'kill-emacs-hook
;;           (function (lambda ()
;;                       (worklog-do-task "logout" t)
;;                       (worklog-finish)
;;                       )))

;;-----------------------------------------------------------------------------
