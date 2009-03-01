;; Loading IDO
(require 'ido)

;; Turning IDO on
(ido-mode t)
(ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point t)
(setq ido-auto-merge-work-directories-length -1) ;What's this?
