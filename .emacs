;; .Emacs file
;; -----------------------------------------------------------------------------
;; TODO:
;; -----------------------------------------------------------------------------
;; * Create a modular .emacs file. Add the ability to enable and disable selected features
;; Ex: Disable ruby support entirely. [PARTIAL - should provide more control and show load status]
;; * Investigate Recentf

(setq load-path (append (list "~/.emacs.children/support"
                              "~/.emacs.children/support/emacs-rails"
			      "~/.emacs.children/support/magit"
			      "~/.emacs.children/support/org-mode/lisp"
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

;; Overriding the default org-mode. 
(require 'org)

;; This should not fail - bar cursor is now included.
(require 'bar-cursor)
;; Setting cursor to a bar one.
(if (fboundp 'bar-cursor-mode)
    (bar-cursor-mode))

;; Displaying line numbers globally
(require 'linum)
(global-linum-mode t)

(if (fboundp 'set-cursor-color)
    (set-cursor-color "red"))

;; Changing the flymake error face
(require 'flymake)
(set-face-background 'flymake-errline "coral4")

;; Loading mode to track work being done
(require 'worklog)

;; TODO: Move this outside .emacs
(require 'todochiku)

;; Loading the various .emacs files
(setq dotemacs-children-prefix "~/.emacs.children/")
(setq dotemacs-children-list '("elisp"
                               "ruby"
                               "options"
                               "ido"
                               "dictionary"
                               "functions"
                               "vivid_chalk"
			       "git"
                               "keymaps"
			       "twit"
			       "scheme"))
(mapc (lambda(x)
	(condition-case err-message
	    (unwind-protect
		(load (concat dotemacs-children-prefix x ".el"))
	      (message "Finished loading file: %s" x))
	  (error (message "Unable to load file: %s" err-message)))) dotemacs-children-list)

;;-----------------------------------------------------------------------------

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ecb-options-version "2.32")
 '(emms-mode-line-mode-line-function nil)
 '(emms-mode-line-titlebar-function (quote emms-mode-line-playlist-current))
 '(erc-join-buffer (quote bury))
 '(erc-nick "outworlder")
 '(erc-track-exclude-types (quote ("JOIN" "NICK" "PART" "QUIT" "333" "353")))
 '(erc-user-full-name "Stephen Pedrosa Eilert")
 '(highline-ignore-regexp "Faces\\|Colors\\|Minibuf\\|\\*tip\\*\\|\\*.*\\*")
 '(initial-buffer-choice t)
 '(js2-highlight-level 3)
 '(newsticker-url-list (quote (("Smspillaz" "http://smspillaz.wordpress.com/feed/" nil nil nil))))
 '(newsticker-url-list-defaults (quote (("Emacs Wiki" "http://www.emacswiki.org/cgi-bin/wiki.pl?action=rss" nil 3600) ("slashdot" "http://slashdot.org/index.rss" nil 3600))))
 '(nxhtml-auto-mode-alist (quote (("\\.x?html?\\'" . nxhtml-mumamo) ("\\.x?htmlf?\\'" . nxhtml-mumamo) ("\\.php\\'" . nxhtml-mumamo) ("\\.phtml\\'" . nxhtml-mumamo) ("\\.jsp\\'" . jsp-nxhtml-mumamo) ("\\.asp\\'" . asp-nxhtml-mumamo) ("\\.djhtml\\'" . django-nxhtml-mumamo) ("\\.rhtml\\'" . eruby-nxhtml-mumamo) ("\\.html.erb\\'" . eruby-nxhtml-mumamo) ("\\.phps\\'" . smarty-nxhtml-mumamo) ("\\.epl\\'" . embperl-nxhtml-mumamo) (".lzx\\'" . laszlo-nxml-mumamo) ("\\.js\\'" . javascript-mode) ("\\.css\\'" . css-mode))))
 '(nxhtml-skip-welcome t)
 '(org-enforce-todo-dependencies t)
 '(org-modules (quote (org-bbdb org-bibtex org-gnus org-info org-jsinfo org-irc org-mew org-mhe org-rmail org-vm org-wl org-w3m org-mouse)))
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
 '(speedbar-use-images t)
 '(todochiku-icons-directory "~/.emacs.children/support/todochiku-icons")
 '(twit-follow-idle-interval 5)
 '(twit-pass "necromancer")
 '(twit-show-user-images t)
 '(twit-user "spedrosa@gmail.com")
 '(twit-user-image-dir "~/.emacs.twit/images"))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(highlight-current-line-face ((t (:background "darkcyan"))))
 '(magit-branch ((t (:inherit magit-header :background "blue" :slant italic :weight bold))))
 '(magit-diff-hunk-header ((t (:inherit magit-header :background "darkgreen"))))
 '(magit-header ((t (:background "darkred"))))
 '(quack-pltish-comment-face ((((class color) (background light)) (:foreground "red4"))))
 '(quack-pltish-defn-face ((t (:foreground "white" :slant italic :weight bold))))
 '(twit-message-face ((default (:inherit nil :height 1.1 :family "sans")) (nil nil))))

;;-----------------------------------------------------------------------------

;; enabling the server
(server-start)
