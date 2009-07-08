(provide 'dotemacs)

(require 'cl)

(require 'todochiku nil t)

(defun display-status (status)
  (if status
    (propertize "OK" 'face "flymake-warnline")
    (propertize "ERROR" 'face "flymake-errline")))

(setq dotemacs-loaded-ok t)

(defun dotemacs-todochiku-notify ()
  (if (not (null dotemacs-loaded-ok))
    (todochiku-message "Dotemacs status" "All packages loaded successfully." (todochiku-icon 'package))
    (todochiku-message "Dotemacs status" "Error loading some packages. Check the *Dotemacs status* buffer for more info." (todochiku-icon 'alert))))

(defun dotemacs-load-children (dotemacs-children-list)
  (with-current-buffer (get-buffer-create "*Dotemacs Status*")
    (toggle-read-only -1)
    (insert "Dotemacs package load status: \n\n")
    (mapc (lambda(x)
	    (condition-case err-message
		(unwind-protect
		    (load (concat dotemacs-children-prefix x ".el"))
		  (insert (format "[%s] Finished loading file: %s\n" (display-status t) x)))
	      (error (progn
		      (insert (format "[%s] Unable to load file: %s - %s\n" (display-status nil) x err-message))
		      (setq dotemacs-loaded-ok nil))))) dotemacs-children-list)
    (toggle-read-only t))
  (if (featurep 'todochiku)
      (add-hook 'after-init-hook 'dotemacs-todochiku-notify)))