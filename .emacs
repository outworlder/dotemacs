;;; .emacs --- .Emacs main file
;; .Emacs file


;;; Commentary:
;; 


;;; History:
;; 

;;; Code:


(require 'cl)

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

(setq load-path (append (list "~/.emacs.children/support"
			      "~/.emacs.children/support/color-theme-6.6.0"
                              "~/.emacs.children/support/emacs-rails"
			      "~/.emacs.children/support/magit"
			      "~/.emacs.children/support/org-mode/lisp"
			      "~/.emacs.children/support/vm-7.19"
			      "~/.emacs.children/support/bbdb-2.35/lisp"
			      "~/.emacs.children/support/emms/lisp"
			      "~/.emacs.children/support/egg"
			      "~/.emacs.children/support/auto-install"
			      "~/.emacs.children/support/cedet-1.0pre7"
			      "~/.emacs.children/support/jdee-2.4.0.1/lisp"
			      "~/.emacs.children/support/muse-latest/lisp"
			      "~/.emacs.children/support/cluck"
                              ) load-path ))


;; enabling the server
(server-start)

(setq dotemacs-children-prefix "~/.emacs.children/")

;;(require 'dotemacs)
(load "~/.emacs.children/dotemacs.el")
(dotemacs-load-children '("options"
			  "recentf"
			  "ido"
			  "elisp"
			  "ruby"
			  "rvm"
			  "dictionary"
			  "functions"
			  "git"
			  ;"egg"
			  "twit"
			  "svn"
			  "scheme"
			  "gnus"
			  "bbdb"
			  "lisp"
			  "w3"
			  "anything"
			  "company"
			  "time_tracking"
			  ;; "cedet"
			  "jdee"
			  "muse"
			  "keymaps"
			  "textmate"
			  "theme") )

(setq debug-on-error nil)

;;-----------------------------------------------------------------------------

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(auto-completion-delay 0.5)
 '(bbdb-complete-name-allow-cycling t)
 '(cluck-fontify-style nil)
 '(completion-auto-show nil)
 '(completion-auto-show-delay 1)
 '(completion-use-echo t)
 '(completion-use-menu nil)
 '(completion-use-popup-frame nil)
 '(display-time-day-and-date nil)
 '(display-time-mode t)
 '(display-time-use-mail-icon t)
 '(ecb-options-version "2.32")
 '(emms-mode-line-mode-line-function nil)
 '(emms-mode-line-titlebar-function (quote emms-mode-line-playlist-current))
 '(erc-join-buffer (quote bury))
 '(erc-nick "outworlder")
 '(erc-track-enable-keybindings t)
 '(erc-track-exclude-types (quote ("JOIN" "NICK" "PART" "QUIT" "333" "353")))
 '(erc-user-full-name "Stephen Pedrosa Eilert")
 '(highline-ignore-regexp "Faces\\|Colors\\|Minibuf\\|\\*tip\\*\\|\\*.*\\*")
 '(highline-priority 9999)
 '(ido-enable-flex-matching t)
 '(imenu-eager-completion-buffer t)
 '(imenu-max-item-length nil)
 '(initial-buffer-choice t)
 '(js2-highlight-level 3)
 '(linum-delay t)
 '(linum-format "%4d ")
 '(mode-line-format (quote ("%e" #("-" 0 1 (help-echo "mouse-1: Select (drag to resize)
mouse-2: Make current window occupy the whole frame
mouse-3: Remove current window from display")) mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification #("   " 0 3 (help-echo "mouse-1: Select (drag to resize)
mouse-2: Make current window occupy the whole frame
mouse-3: Remove current window from display")) mode-line-position (vc-mode vc-mode) #("  " 0 2 (help-echo "mouse-1: Select (drag to resize)
mouse-2: Make current window occupy the whole frame
mouse-3: Remove current window from display")) mode-line-modes (which-func-mode ("" which-func-format #("--" 0 2 (help-echo "mouse-1: Select (drag to resize)
mouse-2: Make current window occupy the whole frame
mouse-3: Remove current window from display")))) (global-mode-string (#("--" 0 2 (help-echo "mouse-1: Select (drag to resize)
mouse-2: Make current window occupy the whole frame
mouse-3: Remove current window from display")) global-mode-string)) #("  " 0 2 (help-echo "mouse-1: Select (drag to resize)
mouse-2: Make current window occupy the whole frame
mouse-3: Remove current window from display")))))
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
 '(timeclock-relative nil)
 '(todochiku-icons-directory "~/.emacs.children/support/todochiku-icons")
 '(twit-follow-idle-interval 5)
 '(twit-show-user-images t)
 '(twit-user "spedrosa@gmail.com")
 '(twit-user-image-dir "~/.emacs.twit/images"))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:height 105 :foundry "unknown" :family "DejaVu Sans Mono"))))
 '(highlight-current-line-face ((t (:background "darkcyan"))))
 '(linum ((t (:inherit (shadow default) :background "gray10" :weight bold))))
 '(magit-branch ((t (:inherit magit-header :background "blue" :slant italic :weight bold))))
 '(magit-diff-hunk-header ((t (:inherit magit-header :background "darkgreen"))))
 '(magit-header ((t (:background "darkred"))))
 '(message-header-cc ((t (:foreground "DeepSkyBlue2"))))
 '(message-header-subject ((t (:foreground "yellow" :weight bold))))
 '(message-header-to ((t (:foreground "Red3" :weight bold))))
 '(mode-line ((t (:background "dim gray" :foreground "black" :box nil :height 0.9 :family "DejaVu Sans"))))
 '(mode-line-buffer-id ((t (:foreground "black" :weight bold))))
 '(mumamo-background-chunk-submode ((t (:background "#222233"))))
 '(quack-pltish-comment-face ((((class color) (background light)) (:foreground "red4"))))
 '(quack-pltish-defn-face ((t (:foreground "white" :slant italic :weight bold))))
 '(twit-author-face ((t (:weight bold :height 0.8 :family "sans"))))
 '(twit-message-face ((default (:height 0.9 :family "sans")) (nil nil)))
 '(twit-title-face ((((background light)) (:background "dark red" :underline "DeepSkyBlue" :box (:line-width 2 :color "PowderBlue" :style 0))) (((background dark)) (:background "dark red" :underline "DeepSkyBlue" :box (:line-width 2 :color "PowderBlue" :style 0))) (t (:underline "white")))))

;;-----------------------------------------------------------------------------



(provide '.emacs)

;;; .emacs ends here

