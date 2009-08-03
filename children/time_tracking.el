(defun timeclock-notify-leave ()
  (todochiku-message "Timeclock" "8:00 completed. It is time to leave." ""))

(add-hook 'timeclock-day-over-hook 'timeclock-notify-leave)

(timeclock-modeline-display nil)