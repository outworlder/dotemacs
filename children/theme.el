
;; Loading color themes
(require 'color-theme)
(color-theme-initialize)

(load "color-theme-vivid-chalk.el")

;(maybe-require 'color-theme-railscasts
;	       (color-theme-railscasts))

;; (color-theme-vivid-chalk)

(load "color-theme-twilight.el" t)
(color-theme-twilight)

(set-face-background 'region "#333377")
;; This is for Aquamacs
(maybe-require 'hl-line
	       (global-hl-line-mode t)
	       (set-face-background 'hl-line "#222222"))

;; Changing colors
;(color-theme-zenburn)
(set-cursor-color "red")

;; This should not fail - bar cursor is now included.
(maybe-require 'bar-cursor
	       (if (fboundp 'bar-cursor-mode)
		   (bar-cursor-mode)))

;; Changing the flymake error face
(maybe-require 'flymake
	       (set-face-background 'flymake-errline "coral4"))

;; Font Specification
;;;      -maker-family-weight-slant-widthtype-style...
;;;      ...-pixels-height-horiz-vert-spacing-width-registry-encoding
;; weight: normal/medium/bold
;; slant: r (roman)/i (italic)/ o (oblique)/ ri (reverse italic)/ ot (other)
;; widthtype: condensed/extended/semicondensed/normal
;; spacing: m (monospaced)/ p (proportional)/ c (character cell)
;; Setting the default font

;; (setq st-default-font "-apple-monaco-medium-r-normal--16-140-72-72-m-140-mac-roman")
;; (setq st-smaller-font "-apple-monaco-medium-r-normal--12-140-72-72-m-140-mac-roman")
;;(setq st-default-font "-apple-monaco-medium-r-normal--14-140-72-72-m-140-mac-roman")

;; The fonts below are only for Unix machines.
;; (setq st-default-font "-unknown-DejaVu Sans Mono-normal-normal-normal-*-14-*-*-*-*-0-iso10646-1")
 ;; (setq st-smaller-font "-unknown-DejaVu Sans-normal-normal-normal-*-12-*-*-*-*-0-iso10646-1")
(setq st-svn-status-font "-unknown-DejaVu Sans Mono-normal-normal-normal-*-*-*-*-*-*-0-iso10646-1")
;; (setq st-default-font "-unknown-Inconsolata-medium-*-*-*-16-*-*-*-*-0-iso10646-1")

;(setq st-default-font "DejaVu Sans Mono-12")
(setq st-smaller-font "Inconsolata-12")
(setq st-default-font "Inconsolata-14")
;;(setq st-default-font "Menlo-16")

;; (defun dotemacs-try-fonts (font-list &optional funct)
;;   ;; Try every font in the list until one succeeds
;;   (let ((try-font
;; 	 (lambda (working-font-list)
;; 	   (condition-case error
;; 	       (let ((font-function (or funct 'set-default-font)))
;; 		 (unless (null (working-font-list))
;; 		   ((funcall font-function (cadr working-font-list)))))
;; 	     (error (funcall try-font (cdr working-font-list)))))))))

;; (dotemacs-try-fonts '("-unknown-DejaVu Sans Mono-bold-normal-normal-*-14-*-*-*-*-0-iso10646-1"
;; 		      "-apple-monaco-medium-r-normal--16-140-72-72-m-140-mac-roman"))

(set-default-font st-default-font)
;; ;; Setting font for new frames
(add-to-list 'default-frame-alist `(font . ,st-default-font))

;; ;; Using a smaller font for the modeline
(set-face-font 'mode-line st-smaller-font)
(set-face-font 'mode-line-inactive st-smaller-font)
;;(set-face-font 'modeline-mousable "-unknown-DejaVu Sans-bold-normal-normal-*-14-*-*-*-*-0-iso10646-1")
(set-face-font 'minibuffer-prompt st-smaller-font)
(set-face-foreground 'mode-line "white")
(set-face-background 'mode-line "gray20")
(set-face-background 'mode-line-inactive "gray2")
(set-face-foreground 'mode-line-buffer-id "cyan")

(if (featurep 'ido)
    (progn
      (set-face-font 'ido-first-match st-default-font)
      (set-face-foreground 'ido-first-match "Red")))

;;(set-face-font 'svn-status-filename-face (read-face-font 'fixed-pitch)) ;;TODO
;;(set-face-font 'svn-status-filename-face "-unknown-DejaVu Sans Mono-bold-normal-normal-*-14-*-*-*-*-0-iso10646-1")
