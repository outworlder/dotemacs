;;; cluck.el --- Enhanced support for Chicken Scheming

(defconst cluck-version "1.0.3")

;; INTRODUCTION:
;;
;;     Enhanced support for Chicken Scheming.

;; INSTALLATION:
;;
;;     To install, put this file (`cluck.el') in your Emacs load path, and add
;;     the following to your `.emacs' file:
;;
;;         (require 'cluck)
;;
;;     'wget' is required for building the SRFI index.
;;     The 'chicken-doc' egg is required for the Chicken Doc functions.
;;
;;     NB: 'cluck.el' is incompatible with 'hen.el'.  You will not see the 
;;     proper fontification if you have hen-mode enabled.

;; KEY BINDINGS:
;;
;;     Cluck adds the following bindings to `scheme-mode':
;;
;;         C-c C-q m   View a manual in your web browser
;;         C-c C-q s   View a SRFI in your web browser
;;         C-c C-q d   Lookup symbol under point in chicken-doc
;;         C-c C-q x   Lookup resulting sexp in chicken-doc
;;         C-c C-q r   Run an inferior Scheme process
;;         C-c C-Q z   Switch to Scheme buffer
;;         C-c C-q l   Toggle `lambda' syntax of `define'-like forms
;;         C-c C-q t   Tidy the formatting of the buffer

;; CREDIT:
;;
;;      This is a shameless hack on Quack 0.37.  The TODO's are from Quack.

(require 'advice)
(require 'cmuscheme)
(require 'compile)
(require 'custom)
(require 'easymenu)
(require 'font-lock)
(require 'scheme)
(require 'thingatpt)

(unless (fboundp 'customize-save-variable)
  (autoload 'customize-save-variable "cus-edit"))

;; Custom Variables:

(defgroup cluck nil
  "Enhanced support for Chicken Scheming."
  :group  'scheme
  :prefix "cluck-"
  :link   '(url-link "http://chicken.wiki.br/cluck"))

(defcustom cluck-dir "~/.cluck"
  "*Directory where Cluck stores various persistent data in file format."
  :type  'string
  :group 'cluck)

(defcustom cluck-scheme-mode-keymap-prefix "\C-c\C-q"
  "*Keymap prefix string for `cluck-scheme-mode-keymap'.

One of the nice things about having C-q in the prefix is that it is unlikely to
be already be in use, due to the historical reality of software flow control
\(and the fact that it is hard to type).  If your C-q doesn't seem to be going
through, then you have several options: disable flow control (if it is safe to
do so), change the value of this variable, or see the Emacs documentation for
`enable-flow-control-on'."
  :type  'string
  :group 'cluck)

(defcustom cluck-global-menu-p t
  "*Whether to have a \"Cluck\" menu always on the menu bar."
  :type  'boolean  :group 'cluck)

(defcustom cluck-tabs-are-evil-p t
  "*Whether Cluck should avoid use of Tab characters in indentation."
  :type  'boolean
  :group 'cluck)

(defcustom cluck-browse-url-browser-function nil
  "*Optional override for `browse-url-browser-function'.

If non-nil, overrides that variable for URLs viewed by `cluck-browse-url'."
  :type '(choice (const    :tag "Do Not Override" nil)
                 (function :tag "Function")
                 (alist    :tag "Regexp/Function Association List"
                           :key-type regexp :value-type function))
  :group 'cluck)

(defcustom cluck-manuals
  '(
    (r5rs 
     "R5RS"
     "http://www.schemers.org/Documents/Standards/R5RS/HTML/"
     nil)
;;     "file:///Users/ddp/Documents/Scheme/www.schemers.org/Documents/Standards/R5RS/HTML/index.html")
    (chicken 
     "Chicken User's Manual"
     "http://chicken.wiki.br/man/4/The%20User's%20Manual"
     nil)
;;     "file:///usr/local/share/chicken/doc/html/The%20User's%20Manual.html")
    (faq        
     "Scheme Frequently Asked Questions"
     "http://community.schemewiki.org/?scheme-faq"
     nil)    
    (htdp
     "How to Design Programs"
     "http://www.htdp.org/"
     nil)
;;     "file:///Users/ddp/Documents/Scheme/www.htdp.org/2003-09-26/Book/index.html")
    (htus       
     "How to Use Scheme"
     "http://www.htus.org/"
     nil)
    (sicp
     "Structure and Interpretation of Computer Programs"
     "http://mitpress.mit.edu/sicp/full-text/book/book.html"
     nil)
;;     "file:///Users/ddp/Documents/Scheme/mitpress.mit.edu/sicp/full-text/book/book.html")
    (t-y-scheme 
     "Teach Yourself Scheme in Fixnum Days"
     "http://www.ccs.neu.edu/home/dorai/t-y-scheme/t-y-scheme.html"
     nil)
;;     "file:///Users/ddp/Documents/Scheme/www.ccs.neu.edu/home/doral/t-y-scheme/t-y-scheme.html")
    (tspl       
     "The Scheme Programming Language"
     "http://www.scheme.com/tspl4/"
     nil)
;;     "file:///Users/ddp/Documents/Scheme/www.scheme.com/tspl4/index.html")
    )
  "*List of specifications of manuals that can be viewed.

Each manual specification is a list of four elements:

    (SYMBOL TITLE URL LOCAL-URL)

where SYMBOL is a symbol that identifies the manual, TITLE is a
string, LOCATION is a URL string, and LOCAL-URL is a URL string or 'nil."
  :type  '(repeat (list (symbol :tag "Identifying Symbol")
                        (string :tag "Title String")
                        (string :tag "URL")
                        (choice :tag "Localized"
                                (string :tag "Local URL")
                                (boolean :tag "'nil"))))
  :group 'cluck)

(defcustom cluck-srfi-master-base-url "http://srfi.schemers.org/"
  ;; Note: Intentionally not letting user change this through the options menu.
  "*The base URL for the master SRFI Web pages.
The SRFI index files should be immediately beneath this."
  :type   'string
  :group  'cluck)

(defcustom cluck-fontify-style 'plt
  "*Which font-lock fontification style to use.

If symbol `plt', an approximation of PLT DrScheme 200 Check Syntax
fontification will be used.  If nil, then Cluck will use the default
emacs Scheme Mode fontification."
  :type       '(choice (const :tag "Quack PLT Style"          plt)
                       (const :tag "Emacs Scheme Default"     nil))
  :group      'cluck
  :set        'cluck-custom-set
  :initialize 'custom-initialize-default)

(defcustom cluck-pltish-fontify-definition-names-p t
  "*If non-nil, fontify names in definition forms for PLT-style fontification.

This only has effect when `cluck-fontify-style' is `plt'."
  :type       'boolean
  :group      'cluck
  :set        'cluck-custom-set
  :initialize 'custom-initialize-default)

(defcustom cluck-pltish-fontify-syntax-keywords-p t
  "*If non-nil, fontify syntax keywords in PLT-style fontification.

This only has effect when `cluck-fontify-style' is `plt'."
  :type       'boolean
  :group      'cluck
  :set        'cluck-custom-set
  :initialize 'custom-initialize-default)

(defcustom cluck-pltish-syntax-keywords-to-fontify
  '(

    "and" 
    "and-let*"
    "assert"
    "begin"
    "begin-for-syntax"
    "call-with-current-continuation"
    "call-with-values"
    "call/cc"
    "case"
    "cond"
    "cond-expand"
    "cut" 
    "cute"
    "define-compiled-syntax"
    "delay"
    "do"
    "dynamic-wind"
    "else"
    "ensure"
    "eval"
    "eval-when"
    "except"
    "fluid-let"
    "for-each"
    "force"
    "foreign-code"
    "foreign-declare"
    "foreign-lambda"
    "foreign-lambda*"
    "foreign-primitive"
    "foreign-safe-lambda"
    "foreign-safe-lambda*"
    "foreign-value"
    "if"
    "import"
    "import-for-syntax"
    "include"
    "let" 
    "let*"
    "let*-values"
    "let-optionals"
    "let-optionals*"
    "let-values" 
    "letrec" 
    "letrec*"
    "letrec-values"
    "load"
    "module"
    "nth-value"
    "only"
    "optional"
    "or"
    "parameterize"
    "prefix"
    "provide"
    "receive"
    "reexport"
    "rename"
    "require"
    "require-extension"
    "require-library"
    "select"
    "set!"
    "set!-values"
    "strip-syntax"
    "syntax"
    "time"
    "unless"
    "use"
    "values"
    "when"

    )
  "*Scheme keywords to fontify when `cluck-fontify-style' is `plt'.  Keep
sorted because this list is fed to regexp-opt."
  :type       '(repeat string)
  :group      'cluck
  :set        'cluck-custom-set
  :initialize 'custom-initialize-default)

(defcustom cluck-fontify-threesemi-p t
  "*Whether three and four semicolon comments should be fontified differently."
  :type       'boolean
  :group      'cluck
  :set        'cluck-custom-set
  :initialize 'custom-initialize-default)

(defcustom cluck-pretty-lambda-p t
  "*Whether Cluck should display \"lambda\" as the lambda character.

`cluck-fontify-style' must be `plt'.  Only supported under GNU Emacs version
21 and up.

Note: Pretty lambda requires that suitable iso8859-7 fonts be available.  Under
Debian/GNU Linux, for example, these can be downloaded and installed with the
shell command \"apt-get install 'xfonts-greek-*'\"."
  :type       'boolean
  :group      'cluck
  :set        'cluck-custom-set
  :initialize 'custom-initialize-default)

(defcustom cluck-programs
  '("csi")
  "List of Scheme interpreter programs that can be used with `run-scheme'.

These names will be accessible via completion when `run-scheme' prompts for
which program to run."
  :group      'cluck
  :type       '(repeat string)
  :set        'cluck-custom-set
  :initialize 'custom-initialize-default)

(defcustom cluck-default-program "csi"
  "Default Scheme interpreter program to use with `run-scheme'."
  :group 'cluck
  :type  'string)

(defcustom cluck-run-scheme-always-prompts-p nil
  "`run-scheme' should always prompt for which program to run.

If nil, `run-scheme' will always use `cluck-default-program' when invoked
interactively without a prefix argument; this is closest to the behavior of the
`cmuscheme' package."
  :group 'cluck
  :type  'boolean)

(defcustom cluck-run-scheme-prompt-defaults-to-last-p t
  "If non-nil, `run-scheme' prompt should default to the last program run."
  :group 'cluck
  :type  'boolean)

(defcustom cluck-remember-new-programs-p t
  "Programs are added to `cluck-programs' automatically."
  :group 'cluck
  :type  'boolean)

(defcustom cluck-switch-to-scheme-method 'other-window
  "Method to use for choosing a window and frame for the process buffer.

One of three symbols:
`other-window' will split display in a different window in the current frame,
splitting the current window if necessary.
`own-frame' will display the process buffer in its own frame.
`cmuscheme' will use the normal behavior of the `cmuscheme' package."
  :group  'cluck
  :type   '(choice (const :tag "Other Window"       other-window)
                   (const :tag "Own Frame"          own-frame)
                   (const :tag "Cmuscheme Behavior" cmuscheme)))

(defcustom cluck-warp-pointer-to-frame-p t
  "Warp mouse pointer to frame with Scheme process buffer.

When `cluck-switch-to-scheme-method' is `own-frame', `switch-to-scheme' will
warp the mouse pointer to the frame displaying the Scheme process buffer."
  :group 'cluck
  :type  'boolean)

(defcustom cluck-chicken-doc-switch-to-scheme-p t
  "Switch to the Scheme process buffer after Chicken Doc."
  :group 'cluck
  :type  'boolean)

(defcustom cluck-newline-behavior 'newline-indent
  "*Behavior of the RET key in Scheme-Mode buffers.  The value is one of three
symbols: `newline' inserts a normal newline, `newline-indent' \(the default\)
inserts a newline and leaves the point properly indented on the new line, and
`indent-newline-indent' re-indents the current line before inserting a newline 
and indenting the new one."
  :type '(choice (const 'newline)
                 (const 'newline-indent)
                 (const 'indent-newline-indent))
  :group 'cluck)

(defcustom cluck-smart-open-paren-p t
  "The `[' can be used to insert `(' characters.
Actually, this just makes the `(' and '[' keys both insert `(', unless given a
prefix argument.  This makes typing parens easier on typical keyboards for
which `(' requires a shift modifier but `[' does not.  A later version of Cluck
might add actual \"smart\" support for automatic PLT-esque insertion of `['
instead of `(' in some syntactic contexts."
  :group 'cluck
  :type  'boolean)

(defcustom cluck-options-persist-p t
  "Option menu settings and programs persist using the `custom' facility.

Note that the value of this option itself cannot be set persistently via the
option menu -- you must use the `customize' interface or set it manually in an
Emacs startup file.  This is by design, to avoid the risk of users accidentally
disabling their ability to set persistent options via the option menu."
  :group 'cluck
  :type  'boolean)

(defconst cluck-pltish-comment-face 'cluck-pltish-comment-face)
(defface  cluck-pltish-comment-face
  '((((class color) (background light)) (:foreground "cyan4"))
    (((class color) (background dark))  (:foreground "cyan1"))
    (t                                  (:slant italic)))
  "Face used for comments when `cluck-fontify-style' is `plt'."
  :group 'cluck)

(defconst cluck-pltish-selfeval-face 'cluck-pltish-selfeval-face)
(defface  cluck-pltish-selfeval-face
  '((((class color) (background light)) (:foreground "green4"))
    (((class color) (background dark))  (:foreground "green2"))
    (t                                  ()))
  "Face used for self-evaluating forms when `cluck-fontify-style' is `plt'."
  :group 'cluck)

(defconst cluck-pltish-paren-face 'cluck-pltish-paren-face)
(defface  cluck-pltish-paren-face
  '((((class color) (background light)) (:foreground "red3"))
    (((class color) (background dark))  (:foreground "red1"))
    (((class grayscale))                (:foreground "gray"))
    (t                                  ()))
  "Face used for parentheses when `cluck-fontify-style' is `plt'."
  :group 'cluck)

(defconst cluck-pltish-colon-keyword-face 'cluck-pltish-colon-keyword-face)
(defface  cluck-pltish-colon-keyword-face
  '((t (:bold t :foreground "gray50")))
  "Face used for `#:' keywords when `cluck-fontify-style' is `plt'.
Note that this isn't based on anything in PLT."
  :group 'cluck)

(defconst cluck-pltish-paren-face 'cluck-pltish-paren-face)
(defface  cluck-pltish-paren-face
  '((((class color) (background light)) (:foreground "red3"))
    (((class color) (background dark))  (:foreground "red1"))
    (((class grayscale))                (:foreground "gray"))
    (t                                  ()))
  "Face used for parentheses when `cluck-fontify-style' is `plt'."
  :group 'cluck)

(defconst cluck-banner-face 'cluck-banner-face)
(defface  cluck-banner-face
  '((t (:family "Helvetica")))
  "Face used in the inferior process buffer for the MzScheme banner.

Currently only takes effect when `cluck-fontify-style' is `plt'."
  :group 'cluck)

(defconst cluck-pltish-defn-face 'cluck-pltish-defn-face)
(defface  cluck-pltish-defn-face
  '((((class color) (background light)) (:bold t :foreground "blue3"))
    (((class color) (background dark))  (:bold t :foreground "blue1"))
    (t                                  (:bold t :underline t)))
  "Face used for names in toplevel definitions.

For PLT-style when `cluck-pltish-fontify-definition-names-p' is non-nil."
  :group 'cluck)

(defconst cluck-pltish-class-defn-face 'cluck-pltish-class-defn-face)
(defface  cluck-pltish-class-defn-face
  '((((class color) (background light))
     (:foreground "purple3" :inherit cluck-pltish-defn-face))
    (((class color) (background dark))
     (:foreground "purple1" :inherit cluck-pltish-defn-face))
    (t (:inherit cluck-pltish-defn-face)))
  "Face used for class names in toplevel definitions.

For PLT-style when `cluck-pltish-fontify-definition-names-p' is non-nil."
  :group 'cluck)

(defconst cluck-pltish-module-defn-face 'cluck-pltish-module-defn-face)
(defface  cluck-pltish-module-defn-face
  '((((class color) (background light))
     (:foreground "purple3" :inherit cluck-pltish-defn-face))
    (((class color) (background dark))
     (:foreground "purple1" :inherit cluck-pltish-defn-face))
    (t (:inherit cluck-pltish-defn-face)))
  "Face used for module names in toplevel definitions.

For PLT-style when `cluck-pltish-fontify-definition-names-p' is non-nil."
  :group 'cluck)

(defconst cluck-pltish-keyword-face 'cluck-pltish-keyword-face)
(defface  cluck-pltish-keyword-face
  '((t (:bold t)))
  "Face used for keywords in PLT Style fontification.

For PLT-style when `cluck-pltish-fontify-syntax-keywords-p' is non-nil."
  :group 'cluck)

(defconst cluck-threesemi-semi-face 'cluck-threesemi-semi-face)
(defface  cluck-threesemi-semi-face
  '((((class color) (background light))
     (:foreground "#a0ffff":background "#c0ffff"))
    (((class color) (background dark))
     (:foreground "cyan2" :background "cyan4"))
    (t (:slant italic)))
  "Face used for `;;;' semicolons when `cluck-fontify-threesemi-p' is non-nil."
  :group 'cluck)

(defconst cluck-threesemi-text-face 'cluck-threesemi-text-face)
(defface  cluck-threesemi-text-face
  '((((class color) (background light))
     (:foreground "cyan4" :background "#c0ffff"))
    (((class color) (background dark))
     (:foreground "white" :background "cyan4"))
    (t (:slant italic)))
  "Face used for `;;;' text when `cluck-fontify-threesemi-p' is non-nil."
  :group 'cluck)

(defconst cluck-threesemi-h1-face 'cluck-threesemi-h1-face)
(defface  cluck-threesemi-h1-face
  '((t (:bold t :family "Helvetica" :height 1.4 :size "20pt")))
  "Face used for H1 headings in `;;;' text."
  :group 'cluck)

(defconst cluck-threesemi-h2-face 'cluck-threesemi-h2-face)
(defface  cluck-threesemi-h2-face
  '((t (:bold t :family "Helvetica" :height 1.2 :size "16pt")))
  "Face used for H2 headings in `;;;' text."
  :group 'cluck)

(defconst cluck-threesemi-h3-face 'cluck-threesemi-h3-face)
(defface  cluck-threesemi-h3-face
  '((t (:bold t :family "Helvetica")))
  "Face used for H3 headings in `;;;' text."
  :group 'cluck)

(defconst cluck-about-title-face 'cluck-about-title-face)
(defface  cluck-about-title-face
  '((((class color) (background light))
     (:bold t :family "Helvetica" :foreground "#008000"
            :height 2.0 :size "24pt"))
    (((class color) (background dark))
     (:bold t :family "Helvetica" :foreground "#00f000"
            :height 2.0 :size "24pt"))
    (t               (:bold t :family "Helvetica"
                            :height 2.0 :size "24pt")))
  "Face used for Cluck name in About Cluck."
  :group 'cluck)

(defconst cluck-about-face 'cluck-about-face)
(defface  cluck-about-face
  '((t (:family "Helvetica")))
  "Face used for the body text in About Cluck."
  :group 'cluck)

(defconst cluck-smallprint-face 'cluck-smallprint-face)
(defface  cluck-smallprint-face
  '((t (:family "Courier" :height 0.8 :size "8pt")))
  "Face used for the \"small print\" in About Cluck."
  :group 'cluck)

;; Compatibility/Portability Misc. Kluges:

;; Note: Some compatibility gotchas found while porting Cluck that aren't
;; addressed by macros and functions:
;;
;;   * `defface' in Emacs 21 supports ":weight bold", but this is silently
;;     ignored under older Emacsen, so ":bold t" must be used instead.

(defmacro cluck-delete-horizontal-space (&rest args)
  (if (>= emacs-major-version 21)
      `(delete-horizontal-space ,@args)
    `(delete-horizontal-space)))

(defmacro cluck-menufilter-return (name form)
  (if (= emacs-major-version 20)
      ;; Note: This isn't working in Emacs 20.  Menu displays now but actions
      ;;       are not executed.  No answer to test case posted to comp.emacs
      ;;       and then to gnu.emacs.help.  In response to my subsequent bug
      ;;       report against Emacs, RMS says that, if this is indeed a bug,
      ;;       then nothing will be done, since 20 is no longer supported.  I'm
      ;;       going to let this quietly not work unless someone emails me that
      ;;       they're actually using Emacs 20.
      `(easy-menu-filter-return (easy-menu-create-menu ,name ,form))
    form))

(defmacro cluck-propertize (obj &rest props)
  (if (>= emacs-major-version 21)
      `(propertize ,obj ,@props)
    (let ((obj-var 'cluck-propertize-G-obj))
      `(let ((,obj-var ,obj))
         (add-text-properties 0 (length ,obj-var) (list ,@props) ,obj-var)
         ,obj-var))))

;; Compatibility/Portability Hash Table:

(eval-and-compile
  (defmacro cluck-make-hash-table (&rest args)
    `(,(if (>= emacs-major-version 21)
           'make-hash-table
         'cluck-fake-make-hash-table)
      ,@args)))

(defmacro cluck-puthash (key value table)
  (list (if (>= emacs-major-version 21) 'puthash 'cluck-fake-puthash)
        key value table))

(defmacro cluck-gethash (key table &optional dflt)
  (list (if (>= emacs-major-version 21) 'gethash 'cluck-fake-gethash)
        key table dflt))

(defun cluck-fake-make-hash-table (&rest args)
  ;; TODO: Parse the keyword args and make this do 'assoc or 'assq, as
  ;;       appropriate.  Currently, this package only needs 'assoc.
  (vector 'assoc '()))

(defun cluck-fake-puthash (key value table)
  (let ((pair (funcall (aref table 0) key (aref table 1))))
    (if pair
        (setcdr pair value)
      (aset table 1 (cons (cons key value) (aref table 1))))))

(defun cluck-fake-gethash (key table &optional dflt)
  (let ((pair (funcall (aref table 0) key (aref table 1))))
    (if pair (cdr pair) dflt)))

;; Messages, Errors, Warnings:

(defmacro cluck-activity (what &rest body)
  (let ((var-what (make-symbol "cluck-activity-G-what")))
    `(let ((,var-what ,what))
       (message (concat ,var-what "..."))
       (prog1 (progn ,@body)
         (message (concat ,var-what "...done"))))))

(defun cluck-internal-error (&optional format &rest args)
  (if format
      (apply 'error (concat "Cluck Internal Error: " format) args)
    (error "Cluck Internal Error.")))

(defun cluck-warning (format &rest args)
  (apply 'message (concat "Cluck Warning: " format) args))

;; Regular Expressions:

(defun cluck-re-alt (&rest regexps)
  (concat "\\(" (mapconcat 'identity regexps "\\|") "\\)"))

(defun cluck-re-optional (&rest regexps)
  (concat "\\("
          (apply 'concat regexps)
          "\\)?"))

;; Misc.:

;; (defun cluck-abbreviate-file-name (file-name)
;;   (let ((directory-abbrev-alist '()))
;;     (abbreviate-file-name file-name)))

(defun cluck-delete-file-if-can (file)
  (condition-case nil (delete-file file) (error nil)))

(defun cluck-expand-file-name (name-or-names &optional directory)
  ;; Note: This only works for systems with Unix-like filenames.
  (expand-file-name (if (listp name-or-names)
                        (mapconcat 'identity name-or-names "/")
                      name-or-names)
                    directory))

(defun cluck-kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

(defun cluck-line-at-point ()
  (save-excursion
    (buffer-substring-no-properties
     (progn (beginning-of-line) (point))
     (progn (end-of-line)       (point)))))

(defun cluck-looking-at-backward (re &optional limit)
  (save-excursion
    (save-restriction
      (let ((start-pt (point)))
        (narrow-to-region (point-min) (point))
        (and (re-search-backward re limit t)
             (= (match-end 0) start-pt)
             (match-beginning 0))))))

(defun cluck-looking-at-close-paren-backward ()
  (save-match-data
    (cluck-looking-at-backward "[])][ \t\r\n\f]*")))

(defun cluck-looking-at-open-paren-backward ()
  (save-match-data
    (cluck-looking-at-backward "[[(][ \t\r\n\f]*")))

(defun cluck-make-directory (dir)
  (setq dir (file-name-as-directory dir))
  (unless (file-directory-p dir)
    (make-directory dir t)))

(defun cluck-make-directory-for-file (file)
  (let ((dir (file-name-directory file)))
    (when dir (cluck-make-directory dir))))

(defun cluck-propertize-bold (str)
  (cluck-propertize str 'face 'bold))

(defun cluck-propertize-face (str face)
  (cluck-propertize str 'face face))

(defun cluck-propertize-italic (str)
  (cluck-propertize str 'face 'italic))

(defun cluck-sort-string-list-copy (lst)
  (sort (copy-sequence lst) 'string<))

(defun cluck-uncomment-region (beg end)
  ;; TODO: Make a cluck-toggle-commentout-region.
  (interactive "r")
  (comment-region beg end '(4)))

(defun cluck-without-side-whitespace (str)
  ;; Copied from `padr-str-trim-ws' by nvd.
  ;;
  ;; TODO: Don't make an intermediate string.  Use regexp match start position.
  (save-match-data
    (if (string-match "^[ \t\n\r]+" str)
        (setq str (substring str (match-end 0))))
    (if (string-match "[ \t\n\r]+$" str)
        (setq str (substring str 0 (match-beginning 0))))
    str))

;; Klugey Sexp Buffer Operations:

(defconst cluck-backward-sexp-re
  (concat "\\`"
          (cluck-re-alt "[^\";\\\\]"
                        "\\\\\\."
                        (concat "\""
                                (cluck-re-alt "[^\"\\\\]"
                                              "\\\\\\.")
                                "*\""))
          "*\\([\"\\\\]\\)?"))

(defun cluck-backward-sexp ()
  ;; Returns non-nil iff point was in a string literal or comment.
  (interactive)
  (when (bobp)
    (error "beginning of buffer"))
  (save-match-data
    (let* ((orig (point))
           (bol  (progn (beginning-of-line) (point))))
      (if (string-match cluck-backward-sexp-re
                        (buffer-substring-no-properties bol orig))
          (if (match-beginning 3)
              ;; We're in what appears to be a comment or unterminated string
              ;; literal (though might not be, due to multi-line string
              ;; literals and block comments), so move point to the beginning.
              (progn (goto-char (+ bol (match-beginning 3)))
                     t)
            ;; We don't appear to be in a comment or string literal, so just
            ;; let `backward-sexp' do its thing.
            (goto-char orig)
            (backward-sexp)
            nil)))))

(defun cluck-parent-sexp-search (name-regexp &optional max-depth max-breadth)
  (save-match-data
    (save-excursion
      (let ((max-depth   (or max-depth   100))
            (max-breadth (or max-breadth 100))
            (orig-point  (point))
            (found       'looking)
            (depth       0)
            (child-start nil))
        (while (and (eq found 'looking) (< depth max-depth))
          (condition-case nil
              (let ((breadth 0))
                ;; Loop until we hit max breadth or error.
                (while (< breadth max-breadth)
                  (when (and (cluck-backward-sexp) (not child-start))
                    (setq child-start (point)))
                  (setq breadth (1+ breadth)))
                ;; We hit our max breadth without erroring, so set the found
                ;; flag to indicate failure and then fall out of our loop.
                (setq found nil))
            (error                      ; scan-error
             ;; We probably hit the beginning of the enclosing sexp, and point
             ;; should be on the first sexp, which will most often be the form
             ;; name, so first check that there really is an open paren to our
             ;; left, and then check if it matches our regexp.
             (let ((paren-start (cluck-looking-at-open-paren-backward)))
               (if paren-start
                   ;; There is a paren, so check the name of the form.
                   (if (and (looking-at name-regexp)
                            (cluck-not-symbol-char-at-point-p (match-end 0)))
                       ;; Found it, so set the result to a list (lexeme, lexeme
                       ;; end point, last nested child sexp start point, parent
                       ;; paren start point) and then fall out of our loop.
                       ;; Note that we return the original point if no child
                       ;; point was found, on the assumption that point was at
                       ;; the beginning of the child sexp (unless it was within
                       ;; the found form name, in which case child sexp start
                       ;; is nil).
                       (setq found (list (match-string-no-properties 0)
                                         (match-end 0)
                                         (or child-start
                                             (if (> orig-point (match-end 0))
                                                 orig-point))
                                         paren-start))
                     ;; This form name didn't match, so try to move up in the
                     ;; paren syntax (which will usually mean moving left one
                     ;; character).
                     (condition-case nil
                         (progn (up-list -1)
                                (setq child-start (point))
                                (setq depth (1+ depth)))
                       (error           ; scan-error
                        ;; We can't go up here, so set found flag to indicate
                        ;; failure and then fall out of the loop.
                        (setq found nil))))
                 ;; There wasn't a paren, which means we hit a scan error for
                 ;; some reason other than being at the beginning of the sexp,
                 ;; so consider the search a failure
                 (setq found nil))))))
        (if (eq found 'looking)
            nil
          found)))))

;; TODO: We really need a global definition of what are Scheme symbol
;;       constituent characters (or a whole-symbol regexp)!

(defun cluck-not-symbol-char-at-point-p (pt)
  ;; This is used to check for a symbol boundary point.
  (save-match-data
    (or (= pt (point-max))
        (if (string-match "[^-a-zA-Z0-9!+<=>$%&*./:@^_~]"
                          (buffer-substring-no-properties pt (1+ pt)))
            t))))

;; String Constant Hashtable:

(eval-and-compile
  (if (< emacs-major-version 21)

      (defun cluck-strconst (str) str)

    (defvar cluck-strconst-hashtable
      (if (>= emacs-major-version 21)
          (cluck-make-hash-table :test 'equal :size 1000)))

    (defun cluck-strconst (str)
      (unless (stringp str)
        (error "Non-string object passed to cluck-strconst: %s" str))
      (or (cluck-gethash str cluck-strconst-hashtable nil)
          (cluck-puthash str str cluck-strconst-hashtable)
          str))))

;; Web URLs:

(defun cluck-quote-url-substring (str &optional quote-slash-p always-new-p)
  (save-match-data
    (let ((regexp (if quote-slash-p "[^-_.A-Za-z0-9]" "[^-_.A-Za-z0-9/]"))
          (subs   '())
          (len    (length str))
          (start  0))
      (while (and (> len start)
                  (string-match regexp str start))
        (let ((beg (match-beginning 0))
              (end (match-end       0)))
          (when (> beg start)
            (setq subs (cons (substring str start beg) subs)))
          (setq subs (cons (format "%%%X" (aref str beg)) subs))
          (setq start end)))
      (if subs
          (apply 'concat (reverse (if (> len start)
                                      (cons (substring str start len) subs)
                                    subs)))
        (if always-new-p (copy-sequence str) str)))))

(defun cluck-file-url (dir file)
  ;; TODO: This is Unix-centric and a little fragile.  Rewrite eventually.
  (concat "file:"
          (cluck-quote-url-substring dir)
          "/"
          (or (cluck-quote-url-substring file) "")))

(defun cluck-build-url (base path)
  (let ((base-slash-p (= (aref base (1- (length base))) ?\/)))
    (if path
        (mapconcat 'identity
                   (cons (if base-slash-p
                             (substring base 0 -1)
                           base)
                         path)
                   "/")
      (if base-slash-p
          base
        (concat base "/")))))

;; Web Browsing:

(defun cluck-browse-url (url)
  (require 'browse-url)
  (message "Cluck viewing URL: %s" url)
  (let ((browse-url-browser-function (or cluck-browse-url-browser-function
                                         browse-url-browser-function)))
    (browse-url url)))

(defun cluck-w3m-browse-url-other-window (url &optional new-window)
  (interactive (eval '(browse-url-interactive-arg "URL: ")))
  (unless (string= (buffer-name) "*w3m*")
    (switch-to-buffer-other-window (current-buffer)))
  ;; TODO: If `*w3m*' buffer is visible in current frame or other frame,
  ;;       switch to that, for Emacsen that don't do that by default.
  (eval '(w3m-browse-url url nil)))

;; Web Getting:

(defconst cluck-web-get-log-buffer-name "*cluck-web-get*")

(defun cluck-web-get-to-file (url out-file)
  ;; TODO: Support other getting tools, such as "lynx -source", "links
  ;;       -source", "w3m -dump_source", and the Emacs w3 package.  Most of
  ;;       these send the Web content to stdout, so, unlike for wget, it will
  ;;       be easier to insert directly to a buffer and send stderr to a temp
  ;;       file.  We should have *-to-file-* and *-insert-via-* functions for
  ;;       each external downloader program anyway.
  (cluck-make-directory-for-file out-file)
  (cluck-web-get-to-file-via-wget url out-file))

;;(defun cluck-web-get-to-temp-file (url)
;;  (let ((temp-file (cluck-make-temp-file "web-get")))
;;    (cluck-web-get-to-file url temp-file)
;;    temp-file))

(defun cluck-web-get-to-file-via-wget (url out-file)
  ;; TODO: Make this initially download to a temp file; replace any
  ;;       pre-existing out-file after successful download.  Do this for any
  ;;       external downloader programs that write to the specified output file
  ;;       before the download is complete.
  (let ((window    (selected-window))
        (saved-buf (current-buffer))
        (log-buf   (get-buffer-create cluck-web-get-log-buffer-name)))
    (unwind-protect
        (progn
          ;; Prepare the log buffer.
          (set-buffer log-buf)
          (widen)
          (buffer-disable-undo)
          (goto-char (point-min))
          (delete-region (point-min) (point-max))
          (set-window-buffer window log-buf)
          ;; Do the wget.
          (cluck-activity
           (format "Getting %S via wget" url)
           (let ((status (call-process "wget" nil t t
                                       "-O" out-file "-t" "1" "--" url)))
             (unless (= status 0)
               (cluck-delete-file-if-can out-file)
               (error "Could not get %S via wget." url))
             (kill-buffer log-buf)
             out-file)))
      ;; unwind-protect cleanup
      (set-window-buffer window saved-buf)
      (set-buffer saved-buf))))

;; HTML Kluges:

(defun cluck-strip-limited-html-tags (str)
  (save-match-data
    (let ((case-fold-search t)
          (str-len          (length str))
          (frags            '())
          (start            0))
      (while (string-match "</?[a-z]+[ \r\n]*>" str start)
        (when (> (match-beginning 0) start)
          (setq frags (cons (substring str start (match-beginning 0)) frags)))
        (setq start (match-end 0)))
      (if frags
          (progn (when (< start str-len)
                   (setq frags (cons (substring str start) frags)))
                 (apply 'concat (reverse frags)))
        str))))

;; Temp Files:

(defun cluck-temp-dir ()
  (file-name-as-directory (expand-file-name "tmp" cluck-dir)))

;; TODO: Make sure this gets executed in load phase even if byte-compiled.

(random t)

(defun cluck-make-temp-file (purpose-str)
  ;; Note: There is an obvious race condition here.  But we're trying to do
  ;;       this in portable Elisp, and if user's `cluck-dir' is writable by
  ;;       someone other than user, then user has bigger problems.
  (save-excursion
    (let* ((buf (generate-new-buffer "*cluck-make-temp-file*"))
           (dir (cluck-temp-dir))
           file)
      (set-buffer buf)
      (cluck-make-directory dir)
      (while (progn (setq file (expand-file-name (format "%d-%s-%d"
                                                         (emacs-pid)
                                                         purpose-str
                                                         (random 10000))
                                                 dir))
                    (file-exists-p file)))
      (set-visited-file-name file)
      (save-buffer 0)
      (kill-buffer buf)
      file)))

;; About:

(defconst cluck-copyright    "Copyright (c) 2009 Derrell Piper; cloned from 'quack.el' by Neil Van Dyke")
(defconst cluck-copyright-2  "Copyright (c) 2002-2009 Neil Van Dyke")
(defconst cluck-copyright-3  "Portions Copyright (c) Free Software Foundation")
(defconst cluck-author-name  "Derrell Piper")
(defconst cluck-author-email "ddp@electric-loft.org")
(defconst cluck-gratuitous-orb-quote "Cluck is the product of a huge ever growing pulsating brain that rules from\nbeyond the Ultraword, loving you.")
(defconst cluck-legal-notice 
"This is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your
option) any later version.  This is distributed in the hope that
it will be useful, but without any warranty; without even the
implied warranty of merchantability or fitness for a particular
purpose.  See the GNU General Public License for more details.
See http://www.gnu.org/licenses/ for details.")

(defun cluck-about ()
  (interactive)
  (let* ((buf-name "*About Cluck*")
         (buf      (get-buffer buf-name)))
    (when buf (kill-buffer buf))
    (setq buf (get-buffer-create buf-name))
    (switch-to-buffer buf)
    (setq buffer-read-only nil)
    (widen)
    (fundamental-mode)
    (when font-lock-mode
      ;;(cluck-warning "Font-lock mode mysteriously on in fundamental-mode.")
      (font-lock-mode -1))
    (buffer-disable-undo)
    ;;(delete-region (point-min) (point-max))
    (erase-buffer)
    (insert
     "\n"
     (cluck-propertize-face (copy-sequence "Cluck") 'cluck-about-title-face)
     "   Version "
     (cluck-propertize-bold (copy-sequence cluck-version))
     "\n"
     (cluck-propertize-italic
      (copy-sequence "Enhanced support for Chicken Scheming"))
     "\n\n"
     "Cluck was cloned from Quack 0.37 by Neil Van Dyke.  Neil deserves all of the\n"
     "credit for the overall look-and-feel, in particular the spiffy fontification.\n"
     "I just ripped out the support for multiple-schemes, replaced the syntax and\n"
     "function keyword lists with ones that reflect only R5RS and Chicken, and then\n"
     "reorganized the menus.  The result is much better suited to Chicken scheming\n"
     "and probably worthless for any other flavor of scheme.\n"
     "\n"
     "Under the hood, embedded in various regexp's throughout, there are still many\n"
     "remnants of its Quack roots which may manifest in various and sundry font lock\n"
     "type bugs.  Email bug reports to me, "
     cluck-author-name
     " <"
     cluck-author-email
     ">\n"
     "though be forewarned that the author has little interest in ongoing support.\n"
     "\n"
     "Mention that you are using "
     (cluck-propertize-bold
      (copy-sequence "GNU Emacs"))
     " "
     (cluck-propertize-bold
      (format "%d.%d" emacs-major-version emacs-minor-version))
     " on "
     (cluck-propertize-bold (copy-sequence system-configuration))
     ".\n\n"
     cluck-gratuitous-orb-quote
     "\n")
    (insert "\n"
            (cluck-propertize-face (copy-sequence cluck-copyright)
                                   'cluck-smallprint-face)
            "\n"
            (cluck-propertize-face (copy-sequence cluck-copyright-2)
                                   'cluck-smallprint-face)
            "\n"
            (cluck-propertize-face (copy-sequence cluck-copyright-3)
                                   'cluck-smallprint-face)
            "\n\n"
            (cluck-propertize-face (concat cluck-legal-notice "\n")
                                   'cluck-smallprint-face))
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)
    (local-set-key "q" 'cluck-kill-current-buffer)
    (message
     "Press `q' to quit *About Cluck*.")))

;; Indenting Newline:

(defun cluck-newline (&optional arg)
  (interactive "*P")
  (if (eq cluck-newline-behavior 'newline)
      (newline arg)
    (if (eq cluck-newline-behavior 'indent-newline-indent)
        (lisp-indent-line)
      (unless (eq cluck-newline-behavior 'newline-indent)
        (error "invalid cluck-newline-behavior value: %s"
               cluck-newline-behavior)))
    (let ((n (prefix-numeric-value arg)))
      (when (> n 0)
        (while (> n 0)
          (setq n (1- n))
          (cluck-delete-horizontal-space t)
          (newline))
        (lisp-indent-line)))))

;; Agreeing-Paren Insert:

;; TODO: Make paren-matching within comments limit seaching to within comments,
;;       not skip back and try to match code.  One workaround is to prefix
;;       parents/brackets in comments with backslash.

(defun cluck-insert-closing (prefix default-close other-open other-close)
  (insert default-close)
  (unless prefix
    (let ((open-pt (condition-case nil
                       (scan-sexps (point) -1)
                     (error (beep) nil))))
      (when open-pt
        (let ((open-char (aref (buffer-substring-no-properties
                                open-pt (1+ open-pt))
                               0)))
          (when (= open-char other-open)
            (delete-backward-char 1)
            (insert other-close))))))
  (when blink-paren-function (funcall blink-paren-function)))

(defun cluck-insert-closing-paren (&optional prefix)
  (interactive "P")
  (cluck-insert-closing prefix ?\) ?\[ ?\]))

(defun cluck-insert-closing-bracket (&optional prefix)
  (interactive "P")
  (cluck-insert-closing prefix ?\] ?\( ?\)))

;; Opening-Paren Insert:

(defun cluck-insert-opening (prefix char)
  (insert (if (or prefix (not cluck-smart-open-paren-p)) char ?\())
  (when blink-paren-function (funcall blink-paren-function)))

(defun cluck-insert-opening-paren (&optional prefix)
  (interactive "P")
  (cluck-insert-opening prefix ?\())

(defun cluck-insert-opening-bracket (&optional prefix)
  (interactive "P")
  (cluck-insert-opening prefix ?\[))

;; Definition Lambda Syntax Toggling:

(defconst cluck-toggle-lambda-re-1
  (concat "define\\*?"
          (cluck-re-alt "-for-syntax"
                        "-public"
                        "/override"
                        "/private"
                        "/public"
                        "")))

(defconst cluck-toggle-lambda-re-2
  (let ((ws-opt      "[ \t\r\n\f]*")
        (symbol      "[^][() \t\r\n\f]+")
        (open-paren  "[[(]")
        (close-paren "[])]"))
    (concat ws-opt
            (cluck-re-alt               ; #=1
             (concat "\\("              ; #<2 `NAME (lambda ('
                     "\\("              ; #<3 name
                     symbol
                     "\\)"              ; #>3
                     ws-opt
                     open-paren
                     ws-opt
                     "lambda"
                     ws-opt
                     open-paren
                     ws-opt
                     "\\)")
             (concat "\\("              ; #<4 `(NAME'
                     open-paren
                     ws-opt
                     "\\("              ; #<5 name
                     symbol
                     "\\)"              ; #>5
                     ws-opt
                     "\\)"))
            "\\("                       ; #<6 optional close paren
            close-paren
            "\\)?"                      ; #>6
            )))

(defun cluck-toggle-lambda ()
  (interactive)
  (save-match-data
    (let ((found (cluck-parent-sexp-search cluck-toggle-lambda-re-1))
          last-paren-marker
          leave-point-marker)
      (unless found
        (error "Sorry, this does not appear to be a definition form."))
      (unwind-protect
          (let ((lexeme-end (nth 1 found))
                (define-beg (nth 3 found)))

            ;; Make the markers.
            (setq last-paren-marker  (make-marker))
            (setq leave-point-marker (point-marker))

            ;; Move to right after the define form keyword, and match the
            ;; pattern of the two possible syntaxes.  Error if no match.
            (goto-char lexeme-end)
            (unless (looking-at cluck-toggle-lambda-re-2)
              (error "Sorry, we can't grok this definition syntax."))

            ;; Pattern matched, so find the closing paren of the define form.
            (let ((pt (condition-case nil
                          (scan-sexps define-beg 1)
                        (error          ; scan-error
                         nil))))
              (if pt
                  (set-marker last-paren-marker (1- pt))
                (cluck-warning
                 "This definition form sexp is unclosed.  Consider undo.")))

            ;; Now act based on which syntax we saw.
            (cond

             ((match-beginning 2)
              ;; We saw the syntax `NAME (lambda ('.
              (let ((name (match-string-no-properties 3)))
                (when (marker-position last-paren-marker)
                  (goto-char last-paren-marker)
                  (let ((victim-beg (cluck-looking-at-close-paren-backward)))
                    (unless victim-beg
                      (error "This definition form should end with `))'."))
                    (delete-region victim-beg (point))))
                (goto-char lexeme-end)
                (delete-region lexeme-end (match-end 2))
                (insert " (" name (if (match-beginning 6) "" " "))))

             ((match-beginning 4)
              ;; We saw the syntax `(NAME'.
              (let ((name (match-string-no-properties 5)))
                (when (marker-position last-paren-marker)
                  (goto-char last-paren-marker)
                  (insert ")"))
                (goto-char lexeme-end)
                (delete-region lexeme-end (match-end 4))
                (insert " " name "\n")
                (set-marker leave-point-marker (point))
                (insert "(lambda (")
                (set-marker-insertion-type leave-point-marker t)))

             (t (cluck-internal-error)))

            ;; Reindent, which also takes care of font-lock updating of deleted
            ;; and inserted text.
            (indent-region define-beg
                           (or (marker-position last-paren-marker)
                               (max (marker-position leave-point-marker)
                                    (point)))
                           nil))

        ;; unwind-protect cleanup
        (goto-char (marker-position leave-point-marker))
        (set-marker leave-point-marker nil)))))

;; Buffer Tidying:

;; TODO: Maybe have an option to automatically tidy the buffer on save.  Make
;;       default off.  This can be slow for larger buffers on older computers,
;;       especially if font-lock is activated.  It can also annoy people who
;;       have a CM system full of improperly formatted files, or who like
;;       things like formfeed characters in their files.

(defun cluck-delete-all-in-buffer (regexp &optional subexp)
  (unless subexp (setq subexp 0))
  ;; Note: This moves the point and changes the match data.
  (goto-char (point-min))
  (while (re-search-forward regexp nil t)
    (goto-char (match-end subexp))
    (delete-region (match-beginning subexp) (point))))

(defun cluck-tidy-buffer ()

  ;; TODO: Make sure this works with odd eol conventions and the various
  ;;       codeset representations in various versions of Emacs.

  ;; TODO: Maybe detect DrScheme ASCII-art "big letters" and protect them from
  ;;       reindenting.

  "Tidy the formatting of the current Scheme buffer.

This reindents, converts tabs to spaces, removes trailing whitespace on lines,
removes formfeed characters, removes extraneous blank lines, and makes sure
the buffer ends with a newline.

This can conceivably corrupt multi-line string literals, but not in any way
they wouldn't be corrupted by Usenet, various mailers, typesetting for print,
etc.

This may also result in large diffs when the tidied file is commited back to a
version control or configuration management system.  Consider making a VC or CM
delta that consists only of changes made by `cluck-tidy-buffer'."
  (interactive)
  (if (= (point-min) (point-max))
      (message "Buffer is empty; no tidying necessary.")
    (let ((marker      (point-marker))
          (fill-prefix nil))
      (unwind-protect
          (save-excursion
            (save-match-data
              (cluck-activity
               "Tidying buffer"

               ;; Make sure last character is a newline.
               (unless (string= "\n" (buffer-substring-no-properties
                                      (1- (point-max))
                                      (point-max)))
                 (goto-char (point-max))
                 (insert "\n"))

               ;; Remove form-feed characters.
               (cluck-delete-all-in-buffer "\f")

               ;; Reindent buffer (without inserting any new tabs).
               ;; Note: This is the time-consuming pass.
               (let ((saved-indent-tabs-mode indent-tabs-mode))
                 (unwind-protect
                     (progn (setq indent-tabs-mode nil)
                            (indent-region (point-min) (point-max) nil))
                   ;; unwind-protect cleanup
                   (setq indent-tabs-mode saved-indent-tabs-mode)))

               ;; Expand any remaining tabs.
               (untabify (point-min) (point-max))

               ;; Remove trailing whitespace on each line.
               (cluck-delete-all-in-buffer "\\([ \t\r]+\\)\n" 1)

               ;; Remove blank lines from top.
               (goto-char (point-min))
               (when (looking-at "[ \t\r\n]+")
                 (delete-region (match-beginning 0) (match-end 0)))

               ;; Remove excess adjacent blank lines.
               (cluck-delete-all-in-buffer "\n\n\\(\n+\\)" 1)

               ;; Remove blank lines from bottom.
               (goto-char (point-max))
               (when (cluck-looking-at-backward
                      "\n\\(\n\\)"
                      (max (point-min) (- (point-max) 3)))
                 (delete-region (match-beginning 1) (match-end 1))))))

        ;; unwind-protect cleanup
        (goto-char (marker-position marker))
        (set-marker marker nil)))))

;; SRFIs:

;; TODO: Archive local copies of SRFIs?  Have to update them when modified, but
;;       without unnecessarily downloading from the master site.  This is
;;       doable with wget mirroring, but not with things like "lynx -source".

(defconst cluck-srfi-subindex-kinds '(draft final withdrawn)
  "List of symbols representing the three possible states of an SRFI (`draft',
`final', and `withdrawn'), in order of increasing precedence (e.g., final
follows draft,since a final version supercedes a draft version).")

(defvar cluck-srfi-completes-cache 'invalid)
(defvar cluck-srfi-menu-cache      'invalid)

(defun cluck-srfi-completes ()
  (when (eq cluck-srfi-completes-cache 'invalid)
    (cluck-process-srfi-subindex-files))
  cluck-srfi-completes-cache)

(defun cluck-srfi-menu (&optional noninteractive)
  (when (eq cluck-srfi-menu-cache 'invalid)
    (cluck-process-srfi-subindex-files noninteractive))
  cluck-srfi-menu-cache)

(defun cluck-srfi-master-url (path)
  (cluck-build-url cluck-srfi-master-base-url path))

(defun cluck-srfi-subindex-master-url (kind)
  (cluck-srfi-master-url (list (cluck-srfi-subindex-basename kind))))

(defun cluck-srfi-dir ()
  (file-name-as-directory (expand-file-name "srfi" cluck-dir)))

(defun cluck-srfi-subindex-file (kind)
  (expand-file-name (cluck-srfi-subindex-basename kind) (cluck-srfi-dir)))

(defun cluck-srfi-subindex-basename (kind)
  (format "%S-srfis.html" kind))

(defun cluck-invalidate-srfi-index-caches ()
  (setq cluck-srfi-completes-cache 'invalid)
  (setq cluck-srfi-menu-cache      'invalid))

(defun cluck-update-srfi-index ()
  (interactive)
  (cluck-activity
   "Updating SRFI index"
   (cluck-download-srfi-subindex-files)))

(defun cluck-download-srfi-subindex-files ()
  (cluck-invalidate-srfi-index-caches)
  (mapcar (function
           (lambda (kind)
             (cluck-activity
              (format "Downloading %s SRFI subindex" kind)
              (cluck-web-get-to-file (cluck-srfi-subindex-master-url kind)
                                     (cluck-srfi-subindex-file       kind)))))
          cluck-srfi-subindex-kinds))

(defun cluck-download-srfi-subindex-files-if-missing ()
  (let ((missing '()))
    (mapc (function
           (lambda (kind)
             (unless (file-exists-p (cluck-srfi-subindex-file kind))
               (setq missing (nconc missing (list kind))))))
          cluck-srfi-subindex-kinds)
    (when (and missing
               (y-or-n-p "Some cached SRFI subindexes are missing. Update? "))
      (cluck-update-srfi-index))))

(defun cluck-process-srfi-subindex-files (&optional noninteractive)
  (let ((index      '())
        (completes  '())
        (menu       (mapcar (function (lambda (kind) (cons kind nil)))
                            cluck-srfi-subindex-kinds)))

    ;; Invalidate dependent caches.
    (cluck-invalidate-srfi-index-caches)

    ;; Give user a chance to download any missing cache files all at once,
    ;; instead of prompting individually later.
    (unless noninteractive
      (cluck-download-srfi-subindex-files-if-missing))

    ;; Parse the index files, letting entries for successive states supercede.
    (mapc (function
           (lambda (kind)
             (mapc (function
                    (lambda (new)
                      (let (old)
                        (if (setq old (assq (car new) index))
                            (setcdr old (cdr new))
                          (setq index (cons new index))))))
                   (cluck-parse-srfi-subindex-file kind noninteractive))))
          cluck-srfi-subindex-kinds)

    ;; Sort the parse form in reverse order, since the cache-building functions
    ;; will reverse this.
    (setq index (sort index (function (lambda (a b) (>= (car a) (car b))))))

    ;; Build the completions and menu caches.
    (let ((fmt (concat "%"
                       (if index
                           (number-to-string
                            (length (number-to-string (car (car index)))))
                         "")
                       "d  %s")))
      (mapc (function
             (lambda (n)
               (let ((num      (nth 0 n))
                     (kind     (nth 1 n))
                     (title    (nth 2 n)))
                 (unless kind (cluck-internal-error))
                 (setq completes
                       (cons (cons (if (eq kind 'final)
                                       (format "%d  %s" num title)
                                     (format "%d  [%s] %s" num kind title))
                                   num)
                             completes))
                 (let ((pair (or (assq kind menu)
                                 (cluck-internal-error))))
                   (setcdr pair (cons `[,(format fmt num title)
                                        (cluck-view-srfi ,num)]
                                      (cdr pair)))))))
            index))

    ;; Finish the menu.
    (mapc (function (lambda (n)
                      (setcar n (cdr (assoc (car n)
                                            '((draft     . "Draft")
                                              (final     . "Final")
                                              (withdrawn . "Withdrawn")))))))
          menu)
    (setq menu `(["Update SRFI Index" cluck-update-srfi-index]
                 "---"
                 ,@menu
                 ["Other SRFI..." cluck-view-srfi]))

    ;; Store the results.
    (setq cluck-srfi-menu-cache      menu)
    (setq cluck-srfi-completes-cache completes)))

(defun cluck-parse-srfi-subindex-file (kind &optional noninteractive)
  (save-excursion
    (let ((file (cluck-srfi-subindex-file kind)))
      (unless (file-exists-p file)
        (error "No SRFI index file %S" file))
      (let* ((buf                (get-file-buffer file))
             (already-visiting-p buf))
        (unless buf
          (setq buf (find-file-noselect file t t)))
        (unwind-protect
            (progn (set-buffer buf)
                   (cluck-parse-srfi-subindex-buffer kind))
          ;; unwind-protect-cleanup
          (unless already-visiting-p
            (kill-buffer buf)))))))

(defconst cluck-parse-srfi-index-buffer-re-1
  (concat
   "<LI><A HREF=\"?srfi-[0-9]+/?\"?>SRFI[ \t]+"
   "\\([0-9]+\\)"                       ; #=1 srfi number
   "</A>:?[ \t]*"
   "\\("                                ; #<2 srfi title
                                        ; #=3
   (cluck-re-alt "[^\r\n<>]" "</?[a-z]+>")
   "+"
   "\\)"))

(defun cluck-parse-srfi-subindex-buffer (kind)
  (save-excursion
    (let ((case-fold-search t)
          (alist            '()))
      (goto-char (point-min))
      (while (re-search-forward cluck-parse-srfi-index-buffer-re-1 nil t)
        (let ((number (string-to-number (match-string-no-properties 1)))
              (title  (cluck-without-side-whitespace
                       (cluck-strip-limited-html-tags
                        (match-string-no-properties 2)))))
          (setq alist (cons

                       ;;(cons number
                       ;;      (if (and kind (not (eq kind 'final)))
                       ;;          (format "[%s] %s" kind title)
                       ;;        title))
                       (list number kind title)

                       alist))))
      (setq alist (reverse alist)))))

(defun cluck-srfi-num-url (num)
  (cluck-srfi-master-url (list (format "srfi-%d"      num)
                               (format "srfi-%d.html" num))))

(defconst cluck-srfi-num-at-point-re-1
  "srfi[-: \t]*\\([0-9]+\\)")

(defconst cluck-srfi-num-at-point-re-2
  ;; Note: We can't have "[^\r\n]*" as a prefix, since it's too slow.
  (concat cluck-srfi-num-at-point-re-1 "[^\r\n]*"))

(defun cluck-srfi-num-at-point ()
  ;; TODO: Make this get the nearest SRFI number in all cases.
  (save-match-data
    (let ((case-fold-search t))
      (cond ((thing-at-point-looking-at cluck-srfi-num-at-point-re-1)
             (string-to-number (match-string-no-properties 1)))
            ((thing-at-point-looking-at "[0-9]+")
             (string-to-number (match-string-no-properties 0)))
            ((thing-at-point-looking-at cluck-srfi-num-at-point-re-2)
             (string-to-number (match-string-no-properties 1)))
            ((let ((str (cluck-line-at-point)))
               (when (string-match cluck-srfi-num-at-point-re-1 str)
                 (string-to-number
                  (match-string-no-properties 1 str)))))))))

(defun cluck-view-srfi (num)
  (interactive (list (cluck-srfi-num-prompt "View SRFI number")))
  (when num
    (unless (and (integerp num) (>= num 0))
      (error "Not a valid SRFI number: %S" num))
    (cluck-browse-url (cluck-srfi-num-url num))))

(defun cluck-srfi-num-prompt (prompt)
  (let* ((completes (cluck-srfi-completes))
         (default   (cluck-srfi-num-at-point))
         (input     (cluck-without-side-whitespace
                     (completing-read
                      (if default
                          (format "%s (default %d): " prompt default)
                        (concat prompt ": "))
                      completes)))
         v)
    (cond ((or (not input) (string= "" input)) default)
          ((setq v (assoc input completes))      (cdr v))
          ((and (setq v (condition-case nil
                            (string-to-number input)
                          (error nil)))
                (integerp v)
                (>= v 0))
           v)
          (t (error "Invalid SRFI number: %s" input)))))

;; Documentation Object:

(defmacro cluck-doc-get-type         (o) `(aref ,o 0))
(defmacro cluck-doc-get-sym          (o) `(aref ,o 1))
(defmacro cluck-doc-get-title        (o) `(aref ,o 2))
(defmacro cluck-doc-get-url          (o) `(aref ,o 3))

(defmacro cluck-doc-set-type         (o v) `(aset ,o 0 ,v))
(defmacro cluck-doc-set-sym          (o v) `(aset ,o 1 ,v))
(defmacro cluck-doc-set-title        (o v) `(aset ,o 2 ,v))
(defmacro cluck-doc-set-url          (o v) `(aset ,o 3 ,v))

(defun cluck-manual-to-doc (manual)
  ;; Accepts a user's manual preference object of the list form:
  ;;
  ;;     (SYM TITLE REMOTE-URL LOCAL-URL)
  ;;
  ;; and creates a manual doc object of the vector form:
  ;;
  ;;     [manual SYM TITLE URL]
  (let ((sym            (nth 0 manual))
        (title          (nth 1 manual))
        (remote-url     (nth 2 manual))
        (local-url      (nth 3 manual))
        (url            nil))
    (if local-url
        (setq url local-url)
        (setq url remote-url))
    (vector 'manual sym title url)))

(defun cluck-read-sexp-file (filename)
  (save-excursion
    (let* ((buf (generate-new-buffer "*cluck-read-sexp-file*")))
      (set-buffer buf)
      (unwind-protect
          (progn (insert-file-contents-literally filename)
                 (goto-char (point-min))
                 (read buf))
        ;; unwind-protect cleanup
        (kill-buffer buf)))))

;; Documentation Database:

(defvar cluck-docs 'invalid)

(defun cluck-docs ()
  (when (eq cluck-docs 'invalid)
    (cluck-docs-build))
  cluck-docs)

(defun cluck-docs-build ()
  (cluck-activity
   "Building Cluck docs database"
   (cluck-invalidate-manuals-caches)
   (setq cluck-docs (mapcar 'cluck-manual-to-doc cluck-manuals))))

(defun cluck-docs-manual-lookup (sym)
  (let ((docs  (cluck-docs))
        (found nil))
    (while (and docs (not found))
      (let ((doc (car docs)))
        (setq docs (cdr docs))
        (when (eq (cluck-doc-get-sym doc) sym)
          (setq found doc))))
    found))

;; Manual Viewing:

(defun cluck-view-manual (&optional sym)
  "View a manual."
  (interactive
   (list
    (let* ((completes (or (cluck-manuals-completes)
                          (error
                           "Sorry, variable \"cluck-manuals\" is empty.")))
           (default   "R5RS")
           (input     (let ((completion-ignore-case t))
                        (completing-read
                         (format "Cluck Manual (default %S): " default)
                         completes nil t nil nil default))))
      (cdr (or (assoc input completes)
               (error "No manual %S." input))))))
  (cluck-activity
   (format "Viewing manual \"%S\"" sym)
   (cluck-browse-url (or (cluck-doc-get-url
                          (or (cluck-docs-manual-lookup sym)
                              (error "Manual \"%S\" not found." sym)))
                         (error "Don't know a URL for manual \"%S\"." sym)))))

(defvar cluck-manuals-menu-cache      'invalid)
(defvar cluck-manuals-completes-cache 'invalid)

(defun cluck-invalidate-manuals-caches ()
  (setq cluck-docs                    'invalid)
  (setq cluck-manuals-completes-cache 'invalid)
  (setq cluck-manuals-menu-cache      'invalid))

;;(cluck-invalidate-manuals-caches)

;; This version maps completion strings to URLs.
;; (defun cluck-manuals-completes ()
;;   (when (eq cluck-manuals-completes-cache 'invalid)
;;     (let ((completes '()))
;;       (mapcar (function
;;                (lambda (doc)
;;                  (let ((sym (cluck-doc-get-sym doc))
;;                        (url (cluck-doc-get-start-url doc)))
;;                    (setq completes
;;                          (cons (cons (cluck-doc-get-title doc) url)
;;                                (cons (cons (symbol-name sym) url)
;;                                      completes))))))
;;               (cluck-docs))
;;       (setq cluck-manuals-completes-cache (reverse completes))))
;;   cluck-manuals-completes-cache)

(defun cluck-manuals-completes ()
  (when (eq cluck-manuals-completes-cache 'invalid)
    (let ((completes '()))
      (mapc (function
             (lambda (doc)
               (let ((sym (cluck-doc-get-sym doc))
                     ;;(url (cluck-doc-get-start-url doc))
                     )
                 (setq completes
                       (cons (cons (cluck-doc-get-title doc) sym)
                             ;;(cons (cons (symbol-name sym) sym)
                             completes
                             ;;)
                             )))))
            (cluck-docs))
      (setq cluck-manuals-completes-cache (reverse completes))))
  cluck-manuals-completes-cache)

(defun cluck-manuals-menu ()
  (when (eq cluck-manuals-menu-cache 'invalid)
    (setq cluck-manuals-menu-cache
          (mapcar (function
                   (lambda (manual)
                     (let ((sym   (nth 0 manual))
                           (title (nth 1 manual)))
                       `[,title (cluck-view-manual (quote ,sym))])))
                  cluck-manuals)))
  cluck-manuals-menu-cache)

(defun cluck-manuals-webjump-sites ()
  "Returns `webjump' entries for manuals in `cluck-manuals'.

Can be used in your `~/.emacs' file something like this:

    (require 'cluck)
    (require 'webjump)
    (require 'webjump-plus)
    (setq webjump-sites
          (append my-own-manually-maintained-webjump-sites
                  (cluck-manuals-webjump-sites)
                  webjump-plus-sites
                  webjump-sample-sites))"

  (let ((result '()))
    (mapc (function
           (lambda (doc)
             (let ((url (cluck-doc-get-url doc)))
               (when url
                 (setq result (cons (cons (cluck-doc-get-title doc) url)
                                    result))))))
          (cluck-docs))
    result))

;; call-cc.org never made the transition to 4 and seems to be abandoned.  In
;; paritular, "?inexact=1" no longer seems to work reliably and you often get
;; dumped into the syntax-case egg.
;;
;; (defun cluck-view-on-callcc ()
;;   "Searches callcc.org for symbol under point."
;;   (interactive)
;;   (let* ((symbol-at-point (symbol-at-point))
;;          (thing (symbol-name symbol-at-point)))
;;     (cluck-browse-url (concat "http://callcc.org/" thing "?noframes=1?inexact=1"))))

(defun cluck-chicken-doc ()
   (interactive)
   (let ((func (current-word)))
     (if func
         (process-send-string "*scheme*"
          (concat "(require-library chicken-doc) ,doc " func "\n"))))
   (if cluck-chicken-doc-switch-to-scheme-p
       (switch-to-scheme t)))
 
(defun cluck-chicken-doc-sexp ()
  (interactive)
  (let ((func (sexp-at-point)))
    (if func
        (process-send-string "*scheme*"
                             (format "(require-library chicken-doc) ,doc %S\n" func))))
  (if cluck-chicken-doc-switch-to-scheme-p
      (switch-to-scheme t)))

;; Inferior Process:

(defvar cluck-run-scheme-prompt-history '())

(defun cluck-remember-program-maybe (program)
  (when (and cluck-remember-new-programs-p
             (not (member program cluck-programs)))
    (cluck-option-set 'cluck-programs (cons program cluck-programs) t)
    (message "Remembering program %S." program)))

(defun cluck-run-scheme-prompt ()
  (let* ((last    (car cluck-run-scheme-prompt-history))
         (default (or (and cluck-run-scheme-prompt-defaults-to-last-p
                           last)
                      cluck-default-program
                      scheme-program-name
                      last
                      "csi"))
         (program (let ((minibuffer-allow-text-properties nil))
                    (completing-read
                     (concat "Run Scheme"
                             (if default
                                 (format " (default %S)" default)
                               "")
                             ": ")
                     (cluck-run-scheme-prompt-completion-collection)
                     nil nil nil
                     'cluck-run-scheme-prompt-history
                     default))))
    (cluck-remember-program-maybe program)
    program))

(defun cluck-run-scheme-prompt-completion-collection ()
  (let ((program-list cluck-programs))
    (mapc (function (lambda (program)
                      (and program
                           (not (member program program-list))
                           (setq program-list (cons program program-list)))))
          (list cluck-default-program
                scheme-program-name))
    (mapc (function (lambda (program) (cons program nil)))
          program-list)))

(defadvice run-scheme (around cluck-ad-run first nil activate)
  "Adds prompting for which Scheme interpreter program to run."
  ;; We don't want to prompt if there's already a Scheme running, but it's
  ;; possible for process to die between the comint check in `interactive' form
  ;; of this advice and the comint check in the `run-scheme' function.  We
  ;; should override `run-scheme' altogether, but for now let's only call the
  ;; original in the case that we do not detect a running Scheme.
  (interactive (list (cond ((comint-check-proc "*scheme*") nil)
                           ((or current-prefix-arg
                                cluck-run-scheme-always-prompts-p)
                            (cluck-run-scheme-prompt))
                           (t cluck-default-program))))
  (if cmd
      ;; We will assume there is no running Scheme, so...  Since `run-scheme'
      ;; calls `pop-to-buffer' rather than `switch-to-scheme', our options for
      ;; Scheme process window management, such as putting the process buffer
      ;; window in its own frame, do not take effect when the process buffer is
      ;; displayed by `run-scheme'.  So, unless we are using the `cmuscheme'
      ;; window management behavior, we attempt to undo whatever window changes
      ;; and buffer changes `run-scheme' makes, then just call
      ;; `switch-to-scheme'.  (This code will be revisited once we decide how
      ;; to handle multiple Schemes, if not before then.)
      (let ((buf (current-buffer))
            (wg  (current-window-configuration)))
        ad-do-it
        (unless (or (not cluck-switch-to-scheme-method)
                    (eq cluck-switch-to-scheme-method 'cmuscheme))
          (set-window-configuration wg)
          (set-buffer buf)
          (switch-to-scheme t))
        (message "Started Scheme: %s" scheme-program-name))
    ;; There is a running Scheme, so don't call the `run-scheme' function at
    ;; all -- just call `switch-to-scheme' or duplicate the `cmuscheme'
    ;; package's `pop-to-buffer' behavior.
    (if (or (not cluck-switch-to-scheme-method)
            (eq cluck-switch-to-scheme-method 'cmuscheme))
        (pop-to-buffer "*scheme*")
      (switch-to-scheme t))
    (message "Switched to running Scheme: %s" scheme-program-name)))

(defadvice scheme-interactively-start-process (around
                                               cluck-ad-sisp 
                                               first
                                               (&optional cmd)
                                               activate)
  ;; (save-window-excursion
  (call-interactively 'run-scheme)
  ;; )
  )

(defadvice scheme-proc (around cluck-ad-scheme-proc first nil activate)
  (condition-case nil
      ad-do-it
    (error (message "Oops, we must start a Scheme process!")
           (call-interactively 'run-scheme)
           (setq ad-return-value (scheme-proc)))))

;; Switch-to-Scheme:

(defun cluck-force-frame-switch-to-window (win)
  (let ((frame (window-frame win)))
    (unless (eq frame (selected-frame))
      (and window-system
           cluck-warp-pointer-to-frame-p
           (set-mouse-position frame 0 0))
      (select-frame frame))
    (select-window win)))

(defadvice switch-to-scheme (before cluck-ad-switch last nil activate)
  "Adds support for the `cluck-switch-to-scheme-method' option."
  ;; This can be done as before-advice since the `pop-to-buffer' that
  ;; `switch-to-scheme' is using appears to always be a no-op when the target
  ;; buffer is already the current buffer.
  (require 'cmuscheme)
  ;; The `eval' below is to avoid problems with the byte-compiler and advising.
  ;; It doesn't seem to like: (and (boundp 'SYM) SYM)
  (let ((repl-buf (eval '(and (boundp 'scheme-buffer)
                              scheme-buffer
                              (get-buffer scheme-buffer)))))
    (cond ((not repl-buf)
           (error (concat "No process current buffer."
                          " Set `scheme-buffer' or execute `run-scheme'")))

          ((or (not cluck-switch-to-scheme-method)
               (eq cluck-switch-to-scheme-method 'cmuscheme))
           nil)

          ((eq (current-buffer) repl-buf) nil)

          ((eq cluck-switch-to-scheme-method 'other-window)
           (switch-to-buffer-other-window repl-buf))

          ;; The following code may be revived if anyone reports problems with
          ;; the use of `special-display-popup-frame'.
          ;;
          ;; ((eq cluck-switch-to-scheme-method 'own-frame)
          ;;  (let ((pop-up-frames                t)
          ;;        (same-window-buffer-names     nil)
          ;;        (same-window-regexps          nil)
          ;;        (special-display-buffer-names nil)
          ;;        (special-display-regexps      nil))
          ;;    (switch-to-buffer (pop-to-buffer repl-buf))))

          ((eq cluck-switch-to-scheme-method 'own-frame)
           (cluck-force-frame-switch-to-window
            (special-display-popup-frame repl-buf)))

          (t (error "Invalid cluck-switch-to-scheme-method: %S"
                    cluck-switch-to-scheme-method)))))

;; Customize:

(defun cluck-customize ()
  "Customize the Cluck package."
  (interactive)
  (customize-group 'cluck))

;; Syntax Table:

(defmacro cluck-str-syntax (str)
  `(,(if (>= emacs-major-version 21)
         'string-to-syntax
       'cluck-kluged-string-to-syntax)
    ,str))

(defun cluck-kluged-string-to-syntax (str)
  (let* ((str-len (length str))
         (code    (aref str 0))
         (matches (if (> str-len 1) (aref str 1)))
         (result  (cond ((= code 32) 0)
                        ((= code ?_) 3)
                        (t (cluck-internal-error))))
         (i       2))
    (while (< i str-len)
      (let ((c (aref str i)))
        (setq i (1+ i))
        (setq result (logior result
                             (lsh 1 (cond ((= c ?1) 16)
                                          ((= c ?2) 17)
                                          ((= c ?3) 18)
                                          ((= c ?4) 19)
                                          ((= c ?p) 20)
                                          ((= c ?b) 21)
                                          ((= c ?n) 21)
                                          (t (cluck-internal-error))))))))
    (cons result (if (= matches 32) nil matches))))

;; Note: We are assuming that it is better to endeavor to fontify all "#|"
;;       block comments as nestable rather than as unnestable, regardless of
;;       whether or not a user's target Scheme dialect supports nested.

(defconst cluck-pound-syntax-string "_ p14bn")
;; (defconst cluck-bar-syntax-string   "  23bn")
(defconst cluck-bar-syntax-string "_ 23bn")

(defconst cluck-pound-syntax (cluck-str-syntax cluck-pound-syntax-string))
(defconst cluck-bar-syntax   (cluck-str-syntax cluck-bar-syntax-string))

(modify-syntax-entry ?# cluck-pound-syntax-string scheme-mode-syntax-table)
(modify-syntax-entry ?| cluck-bar-syntax-string   scheme-mode-syntax-table)

;; Note: Unclear why, but `scheme.el' in GNU Emacs 21.2 is doing
;;       `(set-syntax-table scheme-mode-syntax-table)' in whatever buffer is
;;       active at the time the Elisp package is loaded.

;; Indent Properties:

(put 'case-lambda        'scheme-indent-function 0)
(put 'chicken-setup      'scheme-indent-function 1)
(put 'dynamic-wind       'scheme-indent-function 0)
(put 'let*-values        'scheme-indent-function 1)
(put 'let-optionals      'scheme-indent-function 1)
(put 'let-optionals*     'scheme-indent-function 1)
(put 'let-values         'scheme-indent-function 1)
(put 'module             'scheme-indent-function 1)
(put 'parameterize       'scheme-indent-function 1)
(put 'receive            'scheme-indent-function 1)
(put 'unless             'scheme-indent-function 1)
(put 'when               'scheme-indent-function 1)

;; Keymaps:

(defvar cluck-scheme-mode-keymap nil)

(setq cluck-scheme-mode-keymap (make-sparse-keymap))

;; TODO: Maybe have an option to also map the Ctrl variants of each of these
;;       keys to their respective bindings.  As Eli pointed out, `C-c C-q C-x'
;;       is arguably easier to type than `C-c C-q x'.  Actually, though, I
;;       don't like the `C-c C-q' prefix at all -- it signifies everything that
;;       is wrong with traditional modifier-happy Emacs keybindings.  Maybe we
;;       should encourage users to set the prefix to some other key, like an
;;       unmodified function key.

(define-key cluck-scheme-mode-keymap "m" 'cluck-view-manual)
(define-key cluck-scheme-mode-keymap "s" 'cluck-view-srfi)
(define-key cluck-scheme-mode-keymap "d" 'cluck-chicken-doc)
(define-key cluck-scheme-mode-keymap "x" 'cluck-chicken-doc-sexp)
(define-key cluck-scheme-mode-keymap "r" 'run-scheme)
(define-key cluck-scheme-mode-keymap "z" 'switch-to-scheme)
(define-key cluck-scheme-mode-keymap "l" 'cluck-toggle-lambda)
(define-key cluck-scheme-mode-keymap "t" 'cluck-tidy-buffer)

;; Menus:

(defmacro cluck-bool-menuitem (title var &rest rest)
  (unless (stringp title) (cluck-internal-error))
  (unless (symbolp var)   (cluck-internal-error))
  `[,title (cluck-option-toggle (quote ,var)) :style toggle :selected ,var
           ,@rest])

(defmacro cluck-radio-menuitems (var alist)
  (unless (symbolp var)   (cluck-internal-error))
  (unless (listp   alist) (cluck-internal-error))
  `(quote ,(mapcar
            (function (lambda (pair)
                        (let ((title (car pair))
                              (value (cdr pair)))
                          (unless (stringp title) (cluck-internal-error))
                          (unless (symbolp value) (cluck-internal-error))
                          `[,title
                            (cluck-option-set (quote ,var) (quote ,value))
                            :style    radio
                            :selected (eq ,var (quote ,value))])))
            alist)))

(defconst cluck-global-menuspec
  `("Cluck"
    ["About Cluck..." cluck-about]
    "---"
    ["Run Scheme"              run-scheme]
    ["Switch to Scheme Buffer" switch-to-scheme]
    "---"
    ("View Manual" :filter cluck-view-manual-menufilter)
    ("View SRFI"   :filter cluck-view-srfi-menufilter)
    "---"
    ["Lookup Chicken Doc for point"  cluck-chicken-doc]
    ["Lookup Chicken Doc for sexp"   cluck-chicken-doc-sexp]
    ,(cluck-bool-menuitem "Switch to Scheme Buffer after Doc" cluck-chicken-doc-switch-to-scheme-p)
    "---"
    ["Tidy Buffer"      cluck-tidy-buffer]
    ["Toggle Lambda"    cluck-toggle-lambda]
    "---"
    ,(cluck-bool-menuitem "Use Smart Open Parens" cluck-smart-open-paren-p)
    ,(cluck-bool-menuitem "Expunge Tab Characters" cluck-tabs-are-evil-p)
    "---"
    ("Fontification"
     ,@(cluck-radio-menuitems cluck-fontify-style
                              (("Quack PLT Style"          . plt)
                               ("Emacs Scheme Default"     . nil)))
     "---"
     ,(cluck-bool-menuitem "Use Pretty Lambda"
                           cluck-pretty-lambda-p
                           :active (and cluck-pretty-lambda-supported-p
                                        (memq cluck-fontify-style '(plt))))
     ,(cluck-bool-menuitem "Fontify Definition Names"
                           cluck-pltish-fontify-definition-names-p
                           :active (eq cluck-fontify-style 'plt))
     ,(cluck-bool-menuitem "Fontify Syntax Keywords"
                           cluck-pltish-fontify-syntax-keywords-p
                           :active (eq cluck-fontify-style 'plt))
     ,(cluck-bool-menuitem "Fontify 3+ Semicolon Comments"
                           cluck-fontify-threesemi-p
                           :active (memq cluck-fontify-style '(plt)))
     )
    ("Newline Behavior"
     ,@(cluck-radio-menuitems
        cluck-newline-behavior
        (("Newline"               . newline)
         ("Newline-Indent"        . newline-indent)
         ("Indent-Newline-Indent" . indent-newline-indent))))
    ("Switch-to-Scheme Method"
     ,@(cluck-radio-menuitems cluck-switch-to-scheme-method
                              (("Other Window"       . other-window)
                               ("Own Frame"          . own-frame)
                               ("Cmuscheme Behavior" . cmuscheme)))
     "---"
     ,(cluck-bool-menuitem
       "Warp Pointer to Frame"
       cluck-warp-pointer-to-frame-p
       :active (eq cluck-switch-to-scheme-method 'own-frame)))
    "---"
    ["Customize..." cluck-customize]
    ))

(defun cluck-install-global-menu ()
  (when cluck-global-menu-p
    (unless (assq 'Cluck menu-bar-final-items)
      (setq menu-bar-final-items (cons 'Cluck menu-bar-final-items)))
    (easy-menu-define cluck-global-menu global-map ""
                      cluck-global-menuspec)))

;; TODO: We should make sure the user's custom settings have been loaded
;; before we do this.
(cluck-install-global-menu)

(defconst cluck-scheme-mode-menuspec
  `("Scheme"
    ["Comment or Uncomment Region"   comment-or-uncomment-region]
    "---"
    ["Evaluate Last S-expression"    scheme-send-last-sexp]
    ["Evaluate Region"               scheme-send-region]
    ["Evaluate Region & Go"          scheme-send-region-and-go]
    ["Evaluate Last Definition"      scheme-send-definition]
    ["Evaluate Last Definition & Go" scheme-send-definition-and-go]
    "---"
    ["Compile Definition"            scheme-compile-definition]
    ["Compile Definition & Go"       scheme-compile-definition-and-go]
    ["Load Scheme File"              scheme-load-file]
    ["Compile Scheme File"           scheme-compile-file]))

(defvar cluck-scheme-mode-menu)
(let ((map (make-sparse-keymap)))
  (setq cluck-scheme-mode-menu nil)
  (easy-menu-define cluck-scheme-mode-menu map ""
                    cluck-scheme-mode-menuspec)
  (define-key scheme-mode-map [menu-bar scheme]
    (cons "Scheme"
          (or (lookup-key map [menu-bar Scheme])
              (lookup-key map [menu-bar scheme])))))

(defun cluck-view-manual-menufilter (arg)
  (cluck-menufilter-return "cluck-view-manual-menufilter-menu"
                           (cluck-manuals-menu)))

(defun cluck-view-srfi-menufilter (arg)
  (cluck-menufilter-return
   "cluck-view-srfi-menufilter-menu"
   (condition-case nil
       (cluck-srfi-menu t)
     ;; TODO: Move the generation of this fallback menu down to
     ;;       cluck-srfi-menu.
     (error '(["Update SRFI Index" cluck-update-srfi-index]
              "---"
              ("Draft"     :active nil "")
              ("Final"     :active nil "")
              ("Withdrawn" :active nil "")
              ["Other SRFI..."     cluck-view-srfi])))))

(defun cluck-defaultprogram-menufilter (arg)
  (cluck-menufilter-return
   "cluck-defaultprogram-menufilter-menu"
   `(,@(cluck-optionmenu-items-setdefaultprogram)
     "---"
     ["Other Program..." cluck-set-other-default-program]
     "---"
     ("Forget Program"
      ,@(mapcar
         (function
          (lambda (program)
            `[,(format "Forget  %s" program)
              (cluck-forget-program ,program)]))
         cluck-programs)))))

(defun cluck-optionmenu-items-setdefaultprogram ()
  (let* ((programs      (cluck-sort-string-list-copy cluck-programs))
         (add-default-p (and cluck-default-program
                             (not (member cluck-default-program programs)))))
    (and add-default-p
         (setq programs (cons cluck-default-program programs)))
    (mapcar
     (function
      (lambda (program)
        (let* ((selected-p (and cluck-default-program
                                (equal program cluck-default-program))))
          `[,(format "%s%s"
                     program
                     (if (and add-default-p
                              (equal program cluck-default-program))
                         " (temporary)"
                       ""))
            (cluck-option-set 'cluck-default-program ,program)
            :style radio :selected ,selected-p])))
     programs)))

(mapc (function (lambda (sym) (put sym 'menu-enable 'mark-active)))
      '(comment-region
        indent-region
        cluck-uncomment-region
        scheme-send-region
        scheme-send-region-and-go))

;; Option Menu Callbacks:

(defun cluck-custom-set (sym value)
  ;; Clean up the value based on the variable symbol.
  (cond ((eq sym 'cluck-programs)
         (setq value (cluck-sort-string-list-copy value))))

  ;; Set default binding.  Set local binding just for the halibut, although if
  ;; there are local bindings, then other things will likely break.  \(We used
  ;; to have a check here, but removed it while porting to XEmacs.\)
  (set         sym value)
  (set-default sym value)

  ;; TODO: Probably don't do this during Emacs initialization time, to avoid
  ;;       unnecessary behavior like:
  ;;
  ;;           Loading ~/emacs/my-custom.el (source)...
  ;;           Updating Scheme Mode buffers...done
  ;;           Updating Scheme Mode buffers...done
  ;;           Updating Scheme Mode buffers...done
  ;;           Updating Scheme Mode buffers...done
  ;;           Updating Scheme Mode buffers...done
  ;;           Loading ~/emacs/my-custom.el (source)...done
  
  ;; Update dependent program state.
  (cond ((memq sym '(cluck-fontify-style
                     cluck-fontify-threesemi-p
                     cluck-pltish-fontify-definition-names-p
                     cluck-pltish-fontify-syntax-keywords-p
                     cluck-pltish-syntax-keywords-to-fontify
                     cluck-pretty-lambda-p))
         (cluck-update-scheme-mode-buffers))))

(defun cluck-option-set (sym value &optional silently)
  (if cluck-options-persist-p
      (customize-save-variable sym value)
    (cluck-custom-set sym value))
  (or silently
      (message "Set %s%s to: %S"
               sym
               (if cluck-options-persist-p "" " (non-persistently)")
               value)))

(defun cluck-option-toggle (sym &optional silently)
  (cluck-option-set sym (not (symbol-value sym)) t)
  (or silently
      (message "Set %s%s %s."
               sym
               (if cluck-options-persist-p "" " (non-persistently)")
               (if (symbol-value sym) "ON" "OFF"))))

(defun cluck-update-scheme-mode-buffers ()
  (save-excursion
    (cluck-activity
     "Updating Scheme Mode buffers"
     (mapcar (function
              (lambda (buf)
                (set-buffer buf)
                (when (eq major-mode 'scheme-mode)
                  (cluck-activity (format "Updating buffer %S" (buffer-name))
                                  (scheme-mode)))))
             (buffer-list)))))

;; Pretty Lambda:

(defconst cluck-lambda-char (make-char 'greek-iso8859-7 107))

(defconst cluck-pretty-lambda-supported-p
  (>= emacs-major-version 21))

;; Font Lock:

(defvar cluck-pltish-font-lock-keywords nil)

(defun cluck-pltish-num-re (radix digit base16-p)
  ;; These regexps started as a transliteration of the R5RS BNF to regular
  ;; expressions, adapted for PLTisms, and with a few optimizations.
  ;;
  ;; PLTisms are that 'e' is not permitted as an exponent marker in base-16
  ;; literals, and that "decimal-point" forms are permitted in any radix.
  ;;
  ;; There's obvious opportunity for further optimization, especially if we
  ;; relax the accepted syntax a little.  These regexps have not been tested
  ;; much, but, since this is only Emacs syntax fontification, false-positives
  ;; and false-negatives will be obvious yet benign.
  (let* ((uint    (concat digit "+#*"))
         (sign    "[-+]?")
         (suffix  (cluck-re-optional (if base16-p "[sSfFdDlL]" "[eEsSfFdDlL]")
                                     sign
                                     "[0-9]+"))
         (decimal (cluck-re-alt
                   (concat uint suffix)
                   (concat "\\." digit "+#*" suffix)
                   (concat digit
                           "+"
                           (cluck-re-alt (concat "\\." digit "*")
                                         "#+\\.")
                           "#*")))
         (ureal   (cluck-re-alt uint
                                (concat uint "/" uint)
                                decimal))
         (real    (concat sign ureal))
         (complex (cluck-re-alt
                   (concat real
                           (cluck-re-alt (concat "@" real)
                                         (cluck-re-optional
                                          "[-+]"
                                          (cluck-re-optional ureal)
                                          "i")
                                         ""))
                   (concat "[-+]" (cluck-re-optional ureal) "i")))
         (exact   (cluck-re-optional "#[eEiI]"))
         (prefix  (cluck-re-alt (concat radix exact)
                                (concat exact radix))))
    (concat "\\<" prefix complex "\\>")))

(defconst cluck-pltish-fls-base
  `(
    ("\\`\\(MrEd\\|Welcome to MzScheme\\) v[^\n]+" . cluck-banner-face)
    ("\\`Gambit Version 4\\.0[^\n]*" . cluck-banner-face)
    ("\\`Welcome to scsh [0-9][^\n]+\nType ,\\? for help[^\n]+"
     . cluck-banner-face)
    ("\\`MIT/GNU Scheme running under [^\n]+" . cluck-banner-face)
    ;;("\\`; This is the CHICKEN interpreter - Version [^\n]+\n; (c)[^\n]+"
    ;; . cluck-banner-face)
    ;;("\\`Scheme Microcode Version[^\n]+\nMIT Scheme[^\n]+\n\\([^\n]+\n\\)+" .
    ;;cluck-banner-face)
    ;; Unix cookie line.
    ("\\`#![^\r\n]*" . cluck-pltish-comment-face)
    ;; Colon keywords:
    ("\\<#:\\sw+\\>" . cluck-pltish-colon-keyword-face)
    ;; Self-evals:
    ("'\\sw+\\>"                . cluck-pltish-selfeval-face)
    ("'|\\(\\sw\\| \\)+|"       . cluck-pltish-selfeval-face)
    ;; Note: The first alternative in the following rule will misleadingly
    ;; fontify some invalid syntax, such as "#\(x".
    ("\\<#\\\\\\([][-`~!@#$%&*()_+=^{}\;:'\"<>,.?/|\\\\]\\|\\sw+\\>\\)"
     . cluck-pltish-selfeval-face)
    ("[][()]"                   . cluck-pltish-paren-face)
    ("\\<#\\(t\\|f\\)\\>"       . cluck-pltish-selfeval-face)
    ("\\<+\\(inf.0\\|nan\\)\\>" . cluck-pltish-selfeval-face)
    ("\\<-inf.0\\>"             . cluck-pltish-selfeval-face)
    ,@(mapcar (function (lambda (args)
                          (cons (apply 'cluck-pltish-num-re args)
                                'cluck-pltish-selfeval-face)))
              '(("#b"        "[01]"        nil)
                ("#o"        "[0-7]"       nil)
                ("\\(#d\\)?" "[0-9]"       nil)
                ("#x"        "[0-9a-fA-F]" t)))))

(defconst cluck-pltish-fls-defnames
  ;; TODO: Optimize these once they're fairly complete and correct.

  ;; TODO: Would be nice to fontify binding names everywhere they are
  ;;       introduced, such as in `let' and `lambda' forms.  That may require
  ;;       real parsing to do reasonably well -- the kluges get too bad and
  ;;       slow, and font-lock gets in the way more than it helps.

  `(
                                        ;,@cluck-pltish-font-lock-keywords

    ;; Lots of definition forms that start with "define".
    (,(concat "[[(]"
              "define\\*?"
              ;; TODO: make this into regexp-opt
              (cluck-re-alt ""
                            "define-syntax"
                            "define-record"
                            "define-compiled-syntax")
              "\\>"
              "[ \t]*[[(]?"
              "\\(\\sw+\\)")
     (2 (let ((name (match-string-no-properties 2)))
          (if (= (aref name (1- (length name))) ?%)
              cluck-pltish-class-defn-face
            cluck-pltish-defn-face))
        nil t))

    ;; `defmacro' and related SCM forms.
    (,(concat "[[(]def"
              (cluck-re-alt (concat "macro"
                                    (cluck-re-alt "" "-public"))
                            "syntax")
              "\\>[ \t]+\\(\\sw+\\)")
     3 cluck-pltish-defn-face nil t)

    ;; `defmac' from SIOD.
    ("[[(]defmac[ \t]+[[(][ \t]*\\(\\sw+\\)"
     1 cluck-pltish-defn-face nil t)

    ;; `defvar' and `defun' from SIOD.
    (,(concat "[[(]def"
              (cluck-re-alt "un"
                            "var")
              "[ \t]+\\(\\sw+\\)")
     2 cluck-pltish-defn-face nil t)

    ;; `define-values'
    (,(concat "[[(]define-"
              (cluck-re-alt "values")
              "\\>[ \t]+[[(][ \t]*\\(\\sw+\\([ \t]+\\sw+\\)*\\)")
     2 cluck-pltish-defn-face nil t)

    ;; PLT `module'.
    ("[[(]module\\>[ \t]+\\(\\sw+\\)"
     1 cluck-pltish-module-defn-face nil t)

    ;; Named `let'.  (Note: This is disabled because it's too incongruous.)
    ;;("[[(]let\\>[ \t]+\\(\\sw+\\)"
    ;; 1 cluck-pltish-defn-face nil t)
    ))

;; TODO: Adding PLT-style (quasi)quoted list fontifying is obviously not doable
;;       with just regexps.  Probably requires either cloning
;;       `font-lock-default-fontify-region' just to get it to call our
;;       replacement syntactic pass fontification function, *or*
;;       before-advising `font-lock-fontify-keywords-region' to perform our
;;       syntactic pass when in scheme-mode, and around-advising
;;       `font-lock-fontify-syntactically-region' to not do anything for
;;       scheme-mode (or maybe setting `font-lock-keywords-only' to non-nil,
;;       unless that breaks something else).  Or just ditch font-lock.  See
;;       `font-lock-fontify-region-function' variable in font-lock specs.

;; (defconst cluck-pltish-fls-keywords
;;   `((,(concat
;;        "[[(]\\("
;;        (regexp-opt cluck-pltish-syntax-keywords-to-fontify)
;;        "\\)\\>")
;;      (1 cluck-pltish-keyword-face))))

(defun cluck-install-fontification ()

  (when (eq cluck-fontify-style 'plt)
    (set (make-local-variable 'font-lock-comment-face)
         'cluck-pltish-comment-face)
    (set (make-local-variable 'font-lock-string-face)
         'cluck-pltish-selfeval-face))

  (let* ((sk  `(("\\(#\\)\\(|\\)"
                 (1 ,cluck-pound-syntax)
                 (2 ,cluck-bar-syntax))
                ("\\(|\\)\\(#\\)"
                 (1 ,cluck-bar-syntax)
                 (2 ,cluck-pound-syntax))))
         (pl  (if (and cluck-pretty-lambda-supported-p cluck-pretty-lambda-p)
                  '(("[[(]\\(case-\\|match-\\|opt-\\)?\\(lambda\\)\\>"
                     2
                     (progn (compose-region (match-beginning 2)
                                            (match-end       2)
                                            cluck-lambda-char)
                            nil)))
                '()))
         (threesemi
          (if cluck-fontify-threesemi-p
              `(
                (,(concat "^\\(\;\;\;\\)"
                          ;; TODO: Make this enforce space or newline after the
                          ;; three semicolons.
                          "\\("
                          "[ \t]*"
                          "\\("
                          "[^\r\n]*"
                          "\\)"
                          "\r?\n?\\)")
                 (1 cluck-threesemi-semi-face prepend)
                 (2 cluck-threesemi-text-face prepend)
                 ;;(4 cluck-threesemi-h1-face   prepend)
                 ;;(5 cluck-threesemi-h2-face   prepend)
                 )

                ;; Funcelit:
                ("^\;\;\; @\\(Package\\|section\\|unnumberedsec\\)[ \t]+\\([^\r\n]*\\)"
                 (2 cluck-threesemi-h1-face prepend))
                ("^\;\;\; @subsection[ \t]+\\([^\r\n]*\\)"
                 (1 cluck-threesemi-h2-face prepend))

                ;; semiscribble:
                ("^\;\;\; package +\"\\([^\r\n\"]*\\)\" *"
                 (1 cluck-threesemi-h1-face prepend))
                ("^\;\;\; @section\\(?:\\[[^]]*\\]\\)?{\\([^\r\n]*\\)}"
                 (1 cluck-threesemi-h1-face prepend))
                ("^\;\;\; @subsection\\(?:\\[[^]]*\\]\\)?{\\([^\r\n]*\\)}"
                 (1 cluck-threesemi-h2-face prepend))
                
                
                )
            '()))
         (fld `(,(cond
                  ((eq cluck-fontify-style 'plt)
                   (set (make-local-variable
                         'cluck-pltish-font-lock-keywords)
                        `(,@cluck-pltish-fls-base
                          ,@(if cluck-pltish-fontify-definition-names-p
                                cluck-pltish-fls-defnames
                              '())
                          ,@pl
                          ,@(if cluck-pltish-fontify-syntax-keywords-p
                                ;; cluck-pltish-fls-keywords
                                `((,(concat
                                     "[[(]\\("
                                     (regexp-opt
                                      cluck-pltish-syntax-keywords-to-fontify)
                                     "\\)\\>")
                                   (1 cluck-pltish-keyword-face)))
                              '())
                          ,@threesemi
                          ))
                   'cluck-pltish-font-lock-keywords)
                  (t (cluck-internal-error)))
                nil
                t
                ((?! . "w") (?$ . "w") (?% . "w") (?& . "w") (?* . "w")
                 (?+ . "w") (?- . "w") (?. . "w") (?/ . "w") (?: . "w")
                 (?< . "w") (?= . "w") (?> . "w") (?? . "w") (?@ . "w")
                 (?^ . "w") (?_ . "w") (?~ . "w")
                 ,@(if (eq cluck-fontify-style 'plt)
                       '((?# . "w"))
                     '()))
                ;; TODO: Using `beginning-of-defun' here could be very slow,
                ;;       say, when you have a large buffer that is wrapped in a
                ;;       `module' form.  Look into whether this is a problem.
                beginning-of-defun
                ,@(if t                 ; cluck-gnuemacs-p
                      `((font-lock-mark-block-function . mark-defun)
                        (font-lock-syntactic-keywords  . ,sk))
                    '()))))

    (set (make-local-variable 'font-lock-defaults) fld)))

;; Scheme Mode Startup Hook:

(defun cluck-locally-steal-key-bindings (old-func new-func)
  (mapcar (function (lambda (key)
                      (unless (and (vectorp key)
                                   (eq (aref key 0) 'menu-bar))
                        (local-set-key key new-func))))
          (where-is-internal old-func)))

(defun cluck-shared-mode-hookfunc-stuff ()

  ;; Install the Cluck keymap and menu items.
  (local-set-key cluck-scheme-mode-keymap-prefix cluck-scheme-mode-keymap)

  ;; Bind the paren-matching keys.
  (local-set-key ")" 'cluck-insert-closing-paren)
  (local-set-key "]" 'cluck-insert-closing-bracket)

  (local-set-key "(" 'cluck-insert-opening-paren)
  (local-set-key "[" 'cluck-insert-opening-bracket)

  ;; Fight against tabs.
  (when cluck-tabs-are-evil-p
    (setq indent-tabs-mode nil))

  ;; Remove character compositions, to get rid of any pretty-lambda.  (Note:
  ;; This is bad, if it turns out compositions are used for other purposes in
  ;; buffers that are edited with Scheme Mode.)
  (when cluck-pretty-lambda-supported-p
    (eval '(decompose-region (point-min) (point-max))))

  ;; Install fontification
  (when cluck-fontify-style
    (when (and (boundp 'font-lock-keywords)
               (symbol-value 'font-lock-keywords)
               (not (featurep 'noweb-mode)))
      ;; This warning is not given if the `noweb-mode' package is installed.
      (cluck-warning "`font-lock-keywords' already set when hook ran."))
    (cluck-install-fontification)))

(defun cluck-inferior-scheme-mode-hookfunc ()
  (cluck-shared-mode-hookfunc-stuff))

(defun cluck-scheme-mode-hookfunc ()
  (cluck-shared-mode-hookfunc-stuff)

  ;; Bind Return/Enter key.
  (local-set-key "\r" 'cluck-newline))

(add-hook 'scheme-mode-hook          'cluck-scheme-mode-hookfunc)
(add-hook 'inferior-scheme-mode-hook 'cluck-inferior-scheme-mode-hookfunc)

;; Compilation Mode:

;; TODO: Add compilation-directory-matcher support for "setup-plt:  in".

(defvar cluck-saved-compilation-error-regexp-alist nil)

(defconst cluck-compilation-error-regexp-alist-additions
  (let ((no-line 'cluck-compile-no-line-number))
    `(

      ;; PLT MzScheme 4.1.4 "=== context ===" traceback when there is only file,
      ;; line, and column info, but potentially no following ":" and additional
      ;; info like procedure name.
      ("^\\([^:\n\" ]+\\):\\([0-9]+\\):\\([0-9]+\\)" 1 2 3)

      ;; PLT MzScheme 205 "setup-plt"
      ;;   load-handler: expected a `module' declaration for `bar-unit' in
      ;;   "/u/collects/bar/bar-unit.ss", but found something else
      (,(concat "load-handler: expected a `module' declaration for `[^']+' in "
                "\"\\([^:\n\"]+\\)\", but found something else")
       1 ,no-line)

      ;; PLT MzScheme 205 "setup-plt".
      ;;   setup-plt: Error during Compiling .zos for Foo Bar (/u/collects/fb)
      ("setup-plt: Error during Compiling .zos for [^\n]+ \(\\([^\n\)]+\\)\)"
       1 ,no-line)

      ;; PLT MzScheme 4.0.1 "setup-plt".
      ("setup-plt: +\\(?:WARNING: +\\)\\([^:\n]+\\)::"
       1 ,no-line)

      ;; PLT MzScheme 4.0.1 "setup-plt".
      ("setup-plt: +\\(?:WARNING: +\\)\\([^:\n ][^:\n]*\\):\\([0-9]+\\):\\([0-9]+\\)"
       1 2 3)

      ;; PLT MzScheme 4.0.1 "setup-plt":
      ("load-handler: expected a `module' declaration for `[^'\n]+' in #<path:\\([^>\n]+\\)>[^\n]+"
       1 ,no-line)

      ;; PLT Scheme 4.1.2 "default-load-handler" error without useful filename:
      ("default-load-handler: cannot open input-file: "
       nil ,no-line)

      )))

(defun cluck-compile-no-line-number (filename column)
  (list (point-marker) filename 1 (and column (string-to-number column))))

(defun cluck-install-compilation-mode-stuff ()
  (unless cluck-saved-compilation-error-regexp-alist
    (setq cluck-saved-compilation-error-regexp-alist 
          compilation-error-regexp-alist))
  (setq compilation-error-regexp-alist
        (append cluck-compilation-error-regexp-alist-additions
                cluck-saved-compilation-error-regexp-alist)))

(cluck-install-compilation-mode-stuff)

;; Interpreter-mode-alist:

(defvar cluck-saved-interpreter-mode-alist nil)

(defvar cluck-interpreter-mode-alist-additions
  (mapcar (function (lambda (x)
                      (cons x 'scheme-mode)))
          '("csi"
            "gsi"
            "scheme")))

(defun cluck-install-interpreter-mode-alist ()
  (unless cluck-saved-interpreter-mode-alist
    (setq cluck-saved-interpreter-mode-alist
          interpreter-mode-alist))
  (setq interpreter-mode-alist
        (append cluck-interpreter-mode-alist-additions
                cluck-saved-interpreter-mode-alist)))

(cluck-install-interpreter-mode-alist)

;; To byte-compile:
;;
;; emacs -batch -no-site-file -f batch-byte-compile cluck.el

;; End:

(provide 'cluck)

;; cluck.el ends here
