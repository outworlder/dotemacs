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
(setq visible-bell t)

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
(global-auto-revert-mode)

(if (string-equal (system-name) "Arcturus")
    (setq emacs-location :home)
  (setq emacs-location :atlantico))

(if (eq emacs-location :atlantico)
    (progn
      (setq ecb-source-path (quote ("~/projetos/lvs/subversion/trunk")))
      (setq default-directory "~/projects")
      (require 'jira)
      (setq jira-url "http://jira.atlantico.com.br/rpc/xmlrpc"))
  (progn
    (setq ecb-source-path (quote ("~/projects")))
    (setq default-directory "~/projects")))

(put 'narrow-to-region 'disabled nil)

;; HTML export
(maybe-require 'htmlize)

;; For the Google Chrome extension
(maybe-require 'edit-server
	       (edit-server-start))

;; Enabling cut and copy with the clipboard
(setq x-select-enable-primary nil)
(setq x-select-enable-clipboard nil)
(setq select-active-regions t)
(global-set-key [mouse-2] 'mouse-yank-primary)
