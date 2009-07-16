;; Loading color themes
(require 'color-theme)

;;-----------------------------------------------------------------------------
;; Custom color theme (Based on Textmate's Vibrant Ink
;;-----------------------------------------------------------------------------
(defun color-theme-vivid-chalk ()
  "Based on Vivid Chalk, a vim port of Vibrant Ink."
  (interactive)
  (color-theme-install
   '(color-theme-vivid-chalk
     ((background-color . "black")
      (background-mode . dark)
      (border-color . "black")
      (cursor-color . "white")
      (foreground-color . "white")
      (list-matching-lines-face . bold)
      (view-highlight-face . highlight))
     (default ((t (nil))))
     (bold ((t (:bold t))))
     (bold-italic ((t (:italic t :bold t))))
     (fringe ((t (:background "black"))))
     (font-lock-builtin-face ((t (:foreground "#aaccff"))))
     (font-lock-comment-face ((t (:italic t :foreground "#9933cc"))))
     (font-lock-comment-delimiter-face ((t (:foreground "#9933cc"))))
     (font-lock-constant-face ((t (:foreground "#339999"))))
     (font-lock-function-name-face ((t (:foreground "#ffcc00"))))
     (font-lock-keyword-face ((t (:foreground "#ff6600"))))
     (font-lock-preprocessor-face ((t (:foreground "#aaffff"))))
     (font-lock-reference-face ((t (:foreground "LightSteelBlue"))))
     (font-lock-string-face ((t (:foreground "#66FF00"))))
     (font-lock-doc-face ((t (:foreground "LightSalmon"))))
     (font-lock-type-face ((t (:italic t :foreground "#aaaaaa"))))
     (font-lock-variable-name-face ((t (:foreground "#aaccff"))))
     (font-lock-warning-face ((t (:bold t :foreground "Pink"))))
     (paren-face-match-light ((t (:background "#222222"))))
     (highlight ((t (:background "darkolivegreen"))))
     (italic ((t (:italic t))))
     (modeline ((t (:background "#a5baf1" :foreground "black"))))
     (modeline-buffer-id ((t (:background "#a5baf1" :foreground
"black"))))
     (modeline-mousable ((t (:background "#a5baf1" :foreground
"black"))))
     (modeline-mousable-minor-mode ((t (:background
"#a5baf1" :foreground "black"))))
     (region ((t (:background "#555577"))))
     (primary-selection ((t (:background "#555577"))))
     (isearch ((t (:background "#555555"))))
     (zmacs-region ((t (:background "#555577"))))
     (secondary-selection ((t (:background "darkslateblue"))))
     (flymake-errline ((t (:background "LightSalmon" :foreground
"black"))))
     (flymake-warnline ((t (:background "LightSteelBlue" :foreground
"black"))))
     (underline ((t (:underline t))))
     (minibuffer-prompt ((t (:bold t :foreground "#ff6600")))))))

(color-theme-vivid-chalk)

;;(set-face-background 'mode-line "DarkRed")
(set-face-background 'mode-line "#222288")

(require 'highline)
(set-face-background 'highline-face "#222222")
(highline-mode-on)

;; Changing colors
;(color-theme-zenburn)
(set-cursor-color "red")

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
(setq st-default-font "-unknown-DejaVu Sans Mono-normal-normal-normal-*-14-*-*-*-*-0-iso10646-1")
(setq st-smaller-font "-unknown-DejaVu Sans-normal-normal-normal-*-12-*-*-*-*-0-iso10646-1")
(setq st-svn-status-font "-unknown-DejaVu Sans Mono-normal-normal-normal-*-*-*-*-*-*-0-iso10646-1")

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
;; Setting font for new frames
(add-to-list 'default-frame-alist `(font . ,st-default-font))

;; Using a smaller font for the modeline
(set-face-font 'mode-line st-smaller-font)
(set-face-font 'mode-line-inactive st-smaller-font)
;;(set-face-font 'modeline-mousable "-unknown-DejaVu Sans-bold-normal-normal-*-14-*-*-*-*-0-iso10646-1")
(set-face-font 'modeline-mousable st-smaller-font)
(set-face-font 'minibuffer-prompt st-smaller-font)
(set-face-foreground 'mode-line "White")

(if (featurep 'ido)
    (progn
      (set-face-font 'ido-first-match st-default-font)
      (set-face-foreground 'ido-first-match "Red")))

;;(set-face-font 'svn-status-filename-face (read-face-font 'fixed-pitch)) ;;TODO
;;(set-face-font 'svn-status-filename-face "-unknown-DejaVu Sans Mono-bold-normal-normal-*-14-*-*-*-*-0-iso10646-1")
