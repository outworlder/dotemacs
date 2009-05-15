;; .Emacs file
;; -----------------------------------------------------------------------------
;; TODO:
;; -----------------------------------------------------------------------------
;; * Create a modular .emacs file. Add the ability to enable and disable selected features
;; Ex: Disable ruby support entirely. [PARTIAL - should provide more control and show load status]
;; * Investigate Recentf
;; -------------------- CHANGELOG --------------------
;; 2009-02-19 - .emacs broken down into distinct modules. Only testes on OS-X
;; TODO: nxml-mode is utterly broken for rails. Figure out a way to reenable it.
;; Note: Aquamacs already comes with rails.el
;; 2008-11-18 - Face customizations
;; 2008-11-12 - Added a new color theme.
;; 2008-10-27 - Added function to count the amount of TODOs in a buffer
;; 2008-07-11 - Changed the flymake error face to a better one (coral4)
;; 2008-06-11
;; Added a new function to show issues where I am a participant.
;; Fixed emms-previous error.
;; ELPA package order fix.
;; Added a dictionary.
;; 2008-06-10
;; Adding worklog.
;; Added EMMS/MPD integration.
;; 2008-06-02
;; Added a function to display the issues currently assigned to me in Jira.
;; 2008-04-19
;; Reorganizing sections.
;;
;; 2008-04-18 Stephen Pedrosa Eilert
;; Atlantico and Home .Emacs unification.
;; Adding Ruby modes.
;; Removed Ubuntu-specific dependencies.
;; Adding the scripts path.
;; Replacing many lines of "cons" with a single "list" call.

;;-----------------------------------------------------------------------------

(setq load-path (append (list "~/.emacs.d/color-theme-6.6.0/"
                              "~/.emacs.d/color-theme-6.6.0/themes/"
                              "~/.emacs.d/icicles/"
                              "~/.emacs.d/emacs-jabber-0.7.1/"
                              "~/.emacs.d/w3m-0.5.2/"
                              "~/.emacs.d/rdebug/"
                              "~/.emacs.d/dictionary-1.8.7"
                              "~/.emacs.d/emms"
                              "~/.emacs.d/rhtml"
                              "~/.emacs.d/git-emacs-1.1"
                              "~/.emacs.d/tomtt-emacs-rails"
                              "~/.emacs.d/nxml"
                              "~/.emacs.d"
                              "~/.emacs.children/support"
                              "~/.emacs.children/support/emacs-rails"
                              ) load-path ))

(if (string-equal (system-name) "Arcturus")
    (setq emacs-location :home)
  (setq emacs-location :atlantico))

;; -----------------------------------------------------------------------------
;; Loading the packaging system

;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))

;;-----------------------------------------------------------------------------

(require 'cl)

;; Loading the svn script
(require 'psvn)

;; Setting cursor to a bar one.
(if (fboundp 'bar-cursor-mode)
    (bar-cursor-mode))

(if (fboundp 'set-cursor-color)
    (set-cursor-color "red"))

;; Changing the flymake error face
(require 'flymake)
(set-face-background 'flymake-errline "coral4")

;; Enabling Jabber mode
;; NOTE: Jabber support has been disabled. Never worked properly anyway.
;;(require 'jabber)

;; Loading mode to track work being done
(require 'worklog)

;; Loading VM mail reader
(autoload 'vm "~/vm" "Start VM on your primary inbox." t)
(autoload 'vm-visit-folder "~/vm" "Start VM on an arbitrary folder." t)
(autoload 'vm-mail "~/vm" "Send a mail message using VM." t)
(autoload 'vm-submit-bug-report "~/vm" "Send a bug report about VM." t)

;; Loading the various .emacs files
(setq dotemacs-children-prefix "~/.emacs.children/")
(setq dotemacs-children-list '("elisp"
                               "ruby"
                               "options"
                               "ido"
                               "dictionary"
                               "functions"
                               "vivid_chalk"
			       ;; "git"
                               "keymaps"))
(mapc (lambda(x)
	(condition-case nil
	    (load (concat dotemacs-children-prefix x ".el"))
	  (error (message "Unable to load file: %s" x)))) dotemacs-children-list)

;;-----------------------------------------------------------------------------

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ecb-options-version "2.32")
 '(emms-mode-line-mode-line-function nil)
 '(emms-mode-line-titlebar-function (quote emms-mode-line-playlist-current))
 '(erc-nick "outworlder")
 '(erc-track-exclude-types (quote ("JOIN" "NICK" "PART" "QUIT" "333" "353")))
 '(erc-user-full-name "Stephen Pedrosa Eilert")
 '(initial-buffer-choice t)
 '(jabber-backlog-number 50)
 '(jabber-connection-type (quote ssl))
 '(jabber-network-server "talk.google.com")
 '(jabber-nickname "Stephen")
 '(jabber-port 443)
 '(jabber-resource "Home")
 '(jabber-server "gmail.com")
 '(jabber-username "spedrosa")
 '(js2-highlight-level 3)
 '(newsticker-url-list (quote (("Smspillaz" "http://smspillaz.wordpress.com/feed/" nil nil nil))))
 '(newsticker-url-list-defaults (quote (("Emacs Wiki" "http://www.emacswiki.org/cgi-bin/wiki.pl?action=rss" nil 3600) ("slashdot" "http://slashdot.org/index.rss" nil 3600))))
 '(nxhtml-auto-mode-alist (quote (("\\.x?html?\\'" . nxhtml-mumamo) ("\\.x?htmlf?\\'" . nxhtml-mumamo) ("\\.php\\'" . nxhtml-mumamo) ("\\.phtml\\'" . nxhtml-mumamo) ("\\.jsp\\'" . jsp-nxhtml-mumamo) ("\\.asp\\'" . asp-nxhtml-mumamo) ("\\.djhtml\\'" . django-nxhtml-mumamo) ("\\.rhtml\\'" . eruby-nxhtml-mumamo) ("\\.html.erb\\'" . eruby-nxhtml-mumamo) ("\\.phps\\'" . smarty-nxhtml-mumamo) ("\\.epl\\'" . embperl-nxhtml-mumamo) (".lzx\\'" . laszlo-nxml-mumamo) ("\\.js\\'" . javascript-mode) ("\\.css\\'" . css-mode))))
 '(nxhtml-skip-welcome t)
 '(quack-default-program "csi")
 '(quack-dir "~/emacs.d/.quack")
 '(quack-emacsish-keywords-to-fontify (quote ("and" "begin" "begin0" "call-with-current-continuation" "call-with-input-file" "call-with-output-file" "call/cc" "case" "case-lambda" "class" "cond" "delay" "do" "else" "exit-handler" "field" "for-each" "if" "import" "inherit" "init-field" "interface" "lambda" "let" "let*" "let*-values" "let-values" "let-syntax" "let/ec" "letrec" "letrec-syntax" "map" "mixin" "opt-lambda" "or" "override" "protect" "provide" "public" "rename" "require" "require-for-syntax" "syntax" "syntax-case" "syntax-error" "syntax-rules" "unit/sig" "unless" "when" "with-syntax" "use")))
 '(quack-fontify-style (quote emacs))
 '(quack-global-menu-p nil)
 '(quack-newline-behavior (quote indent-newline-indent))
 '(quack-pltish-keywords-to-fontify (quote ("and" "begin" "begin0" "c-declare" "c-lambda" "case" "case-lambda" "class" "class*" "class*/names" "class100" "class100*" "compound-unit/sig" "cond" "cond-expand" "define" "define-class" "define-const-structure" "define-constant" "define-embedded" "define-entry-point" "define-external" "define-foreign-record" "define-foreign-type" "define-foreign-variable" "define-generic" "define-generic-procedure" "define-inline" "define-location" "define-macro" "define-method" "define-module" "define-opt" "define-public" "define-reader-ctor" "define-record" "define-record-printer" "define-record-type" "define-signature" "define-struct" "define-structure" "define-syntax" "define-syntax-set" "define-values" "define-values/invoke-unit/sig" "define/contract" "define/override" "define/private" "define/public" "delay" "do" "else" "exit-handler" "field" "if" "import" "inherit" "inherit-field" "init" "init-field" "init-rest" "instantiate" "interface" "lambda" "let" "let*" "let*-values" "let+" "let-syntax" "let-values" "let/ec" "letrec" "letrec-values" "letrec-syntax" "match-lambda" "match-lambda*" "match-let" "match-let*" "match-letrec" "match-define" "mixin" "module" "opt-lambda" "or" "override" "override*" "namespace-variable-bind/invoke-unit/sig" "parameterize" "private" "private*" "protect" "provide" "provide-signature-elements" "provide/contract" "public" "public*" "quasiquote" "quote" "receive" "rename" "require" "require-for-syntax" "send" "send*" "set!" "set!-values" "signature->symbols" "super-instantiate" "syntax-case" "syntax-case*" "syntax-error" "syntax-rules" "unit/sig" "unless" "unquote" "unquote-splicing" "when" "with-handlers" "with-method" "with-syntax" "use" "with-output-to-file" "with-exception-handler" "with-output-from-file" "string-append")))
 '(quack-pretty-lambda-p t)
 '(quack-programs (quote ("csi" "bigloo" "csi -:c" "csi -hygienic" "gosh" "gsi" "gsi ~~/syntax-case.scm -" "guile" "kawa" "mit-scheme" "mred -z" "mzscheme" "mzscheme -M errortrace" "rs" "scheme" "scheme48" "scsh" "sisc" "stklos" "sxi")))
 '(rails-always-use-text-menus t)
 '(rails-tags-command "ctags-exuberant -e -a --Ruby-kinds=-f -o %s -R %s")
 '(rails-ws:default-server-type "mongrel")
 '(show-paren-mode t)
 '(speedbar-use-images t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(highlight-current-line-face ((t (:background "darkcyan"))))
 '(quack-pltish-comment-face ((((class color) (background light)) (:foreground "red4"))))
 '(quack-pltish-defn-face ((t (:foreground "white" :slant italic :weight bold)))))

;;-----------------------------------------------------------------------------

;; enabling the server
(server-start)


;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))


;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))
