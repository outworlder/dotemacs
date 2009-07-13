(setq
 user-full-name "Stephen Eilert"
 user-mail-address "stephen@atlantico.com.br"
 nnmail-spool-file "/var/mail/stephen"
 display-time-mail-file "/var/mail/stephen"
 message-default-charset `utf-8
 nnml-directory "~/Mail/"
 gnus-select-method '(nnml ""))

(require 'mm-url)
(defadvice mm-url-insert (after DE-convert-atom-to-rss () )
  "Converts atom to RSS by calling xsltproc."
  (when (re-search-forward "xmlns=\"http://www.w3.org/.*/Atom\"" 
			   nil t)
    (goto-char (point-min))
    (message "Converting Atom to RSS... ")
    (call-process-region (point-min) (point-max) 
			 "xsltproc" 
			 t t nil 
			 (expand-file-name "~/.emacs.children/support/atom2rss.xsl") "-")
    (goto-char (point-min))
    (message "Converting Atom to RSS... done")))

(ad-activate 'mm-url-insert)
