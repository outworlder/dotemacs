;; EMMS config

;; Loading emms
(require 'emms)
(require 'emms-setup)
(emms-standard)
(emms-default-players)

(require 'emms-mode-line)
;; (require 'emms-player-mpd)

;; (setq emms-player-mpd-server-name "localhost")
;; (setq emms-player-mpd-server-port "6600")
;; (add-to-list 'emms-player-list 'emms-player-mpd)
;; (emms-mode-line 1)

(require 'emms-player-simple)

(defcustom emms-player-rhythmbox (emms-player 'emms-player-rhythmbox-start
					      'emms-player-rhythmbox-stop
					      'emms-player-rhythmbox-playable-p)
  "Plays music using the Rhythmbox D-Bus interface")

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

(defun emms-player-rhythmbox-start (arg)
  (interactive)
  (unless (emms-rhythmbox-dbus-playing-p)
    (emms-rhythmbox-dbus-play-pause)))

(defun emms-player-rhythmbox-stop ()
  (interactive)
  (if (emms-rhythmbox-dbus-playing-p)
      (emms-rhythmbox-dbus-play-pause)))

(defun emms-player-rhythmbox-playable-p (track)
  t)

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


(setq emms-player-list '(emms-player-rhythmbox))

(provide 'emms-player-rhythmbox)

(require 'emms-player-rhythmbox)