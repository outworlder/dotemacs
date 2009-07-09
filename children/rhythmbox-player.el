(require 'emms-player-simple)

(defcustom emms-player-rhythmbox (emms-player 'emms-player-rhythmbox-start
					      'emms-player-rhythmbox-stop
					      'emms-player-rhythmbox-playable-p))

(defun emms-rhythmbox-dbus-call-player (method &rest args)
  (apply 'dbus-call-method :session "org.gnome.Rhythmbox" "/org/gnome/Rhythmbox/Player" "org.gnome.Rhythmbox.Player" method args))

(defun emms-rhythmbox-dbus-play-pause ()
  (emms-rhythmbox-dbus-call-player "playPause" t))

(defun emms-rhythmbox-dbus-playing-p ()
  (emms-rhythmbox-dbus-call-player "getPlaying"))

(defun emms-rhythmbox-dbus-get-elapsed ()
  (emms-rhythmbox-dbus-call-player "getElapsed"))

(defun emms-rhythmbox-dbus-set-elapsed ()
  (emms-rhythmbox-dbus-call-player "setElapsed"))

(defun emms-player-rhythmbox-play-pause ()
  (emms-rhythmbox-dbus-play-pause))

(defun emms-player-rhythmbox-start ()
  (unless (emms-rhythmbox-dbus-playing-p)
    (emms-rhythmbox-dbus-play-pause)))

(defun emms-player-rhythmbox-stop ()
  (if (emms-rhythmbox-dbus-playing-p)
    (emms-rhythmbox-dbus-play-pause)))

(defun emms-player-rhythmbox-seek-to (seconds)
  (emms-rhythmbox-dbus-set-elapsed seconds))

(defun emms-player-rhythmbox-seek (seconds)
  (let ((elapsed-time (emms-rhythmbox-dbus-get-elapsed)))
    (emms-player-rhythmbox-seek-to (+ elapsed-time seconds))))

(emms-player-set emms-player-rhythmbox 'pause 'emms-player-rhythmbox-play-pause)
(emms-player-set emms-player-rhythmbox 'resume nil) ; pause is resume
(emms-player-set emms-player-rhythmbox 'start 'emms-player-rhythmbox-start)
(emms-player-set emms-player-rhythmbox 'seek 'emms-player-rhythmbox-seek)
(emms-player-set emms-player-rhythmbox 'seek-to 'emms-player-rhythmbox-seek-to)

(provide 'emms-player-rhythmbox)