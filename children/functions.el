
;; Custom functions

(defun set-key (kbd funct)
  (global-set-key (read-kbd-macro kbd) funct))

(defun count-todos-in-buffer ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (message "Number of TODO's in the current buffer: %d" (count-matches "TODO:"))))

;; Custom JIRA functions

(defvar *current-project* "lvs")

(defun jira-show-by-filter-name (filter-name)
  "Shows a list of issues with a given filter name in another buffer."
  ;; Getting a list of filters, to look for the specified filter.
  (let ((filter-alist (jira-get-filter-alist)))
    (jira-list-issues
     (cdr (assoc filter-name filter-alist)))))

(defun jira-show-assigned-me ()
  "Shows a list of issues assigned to me in another buffer."
  (interactive)
  (jira-show-by-filter-name "Open Issues - Assigned to Me"))

(defun jira-show-participating ()
  "Shows a list of issues assigned to me in another buffer."
  (interactive)
  (jira-show-by-filter-name "Participating Issues"))

(defun ido-goto-symbol ()
    "Will update the imenu index and then use ido to select a symbol to navigate to"
    (interactive)
    (imenu--make-index-alist)
    (let ((name-and-pos '())
          (symbol-names '()))
      (flet ((addsymbols (symbol-list)
                         (when (listp symbol-list)
                           (dolist (symbol symbol-list)
                             (let ((name nil) (position nil))
                               (cond
                                ((and (listp symbol) (imenu--subalist-p symbol))
                                 (addsymbols symbol))
   
                                ((listp symbol)
                                 (setq name (car symbol))
                                 (setq position (cdr symbol)))
   
                                ((stringp symbol)
                                 (setq name symbol)
                                 (setq position (get-text-property 1 'org-imenu-marker symbol))))
   
                               (unless (or (null position) (null name))
                                 (add-to-list 'symbol-names name)
                                 (add-to-list 'name-and-pos (cons name position))))))))
        (addsymbols imenu--index-alist))
      (let* ((selected-symbol (ido-completing-read "Symbol? " symbol-names))
             (position (cdr (assoc selected-symbol name-and-pos))))
        (goto-char position))))


;;----------------------------------------------------------------------------
;; ido completion in M-x
;;----------------------------------------------------------------------------
;; See http://www.emacswiki.org/cgi-bin/wiki/InteractivelyDoThings#toc5
(defun ido-execute ()
  (interactive)
  (call-interactively
   (intern
    (ido-completing-read
     "M-x "
     (let (cmd-list)
       (mapatoms (lambda (S) (when (commandp S) (setq cmd-list (cons (format "%S" S) cmd-list)))))
       cmd-list)))))

;; -----------------------------------------------------------------------------
;; Faster editing of .emacs
;; -----------------------------------------------------------------------------

(defun edit-dot-emacs()
  (interactive)
  (find-file "~/.emacs"))

