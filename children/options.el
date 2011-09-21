;; Overriding the default org-mode. 
(require 'org)

;; Setting cursor to a bar one.

;; Displaying line numbers globally
(require 'linum)
(global-linum-mode t)

;; Preventing the toggling off linum mode (mostily under nxhtml-mode)
(add-hook 'after-change-major-mode-hook 'linum-on) 

(if (fboundp 'set-cursor-color)
    (set-cursor-color "red"))

;; Setting the default to Unicode
(prefer-coding-system 'utf-8)

;; Silencing the annoying bell
(setq visible-bell nil)

(setq ring-bell-function (lambda () ))

;; Setting the default tab width to 8 - this should be the default anyway
(setq-default tab-width 8)

;; Spaces instead of tabs
(setq-default indent-tabs-mode t)

;; Similar to autoindent in VI
(global-set-key "\C-m" 'newline-and-indent)

;; Paren matching
(show-paren-mode t)

;; Removing the useless toolbar
(tool-bar-mode 0)
;; ;; Removing the menu bar as well
;; (menu-bar-mode 0)

(put 'scroll-left 'disabled nil)
(put 'scroll-right 'disabled nil)

;; Making Emacs scroll a buffer one line at a time, instead of half-screen
(setq scroll-step 1)
(setq scroll-conservatively 5)

;; Scrollbars to the right

(set-scroll-bar-mode 'right)

;; Turning on window-numbering-mode
(require 'window-numbering)
(window-numbering-mode 1)

;; Turning on the global-auto-revert-mode
(global-auto-revert-mode t)

(if (string-equal (system-name) "Polaris")
    (setq emacs-location :guilda)
  (setq emacs-location :detran))

(if (eq emacs-location :detran)
    (progn
      (setq ecb-source-path (quote ("~/Documentos/Projetos/Detran")))
      (setq default-directory "~/Documentos/Projetos/Detran"))
  (progn
    (setq ecb-source-path (quote ("~/Documents/Projects/Guilda")))
    (setq default-directory "~/Documents/Projects/Guilda")))

(put 'narrow-to-region 'disabled nil)

;; HTML export
(maybe-require 'htmlize)

;; For the Google Chrome extension
(maybe-require 'edit-server
	       (edit-server-start))

;; Enabling cut and copy with the clipboard
;; (setq x-select-enable-primary nil)
;; (setq x-select-enable-clipboard nil)
;; (setq select-active-regions t)
;; (global-set-key [mouse-2] 'mouse-yank-primary)

;;; Displaying flymake messages in the modeline
(maybe-require 'flymake-cursor)

(delete-selection-mode t)

(column-number-mode t)