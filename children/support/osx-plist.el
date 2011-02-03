
(require 'xml)

;; Placate the byte compiler and elint
(defvar exec-path)

(defvar osx-env-file "~/.MacOSX/environment.plist"
  "File in which Mac OS X stores your environment.")

(defun osx-plist-process-array (xml)
  "Process the plist array element XML."
  (let ((real-children (list)))
    (mapc (lambda (child)
            (when (not (stringp child))
              (push (osx-plist-node-value child) real-children)))
          (xml-node-children xml))
    (apply 'vector (nreverse real-children))))

(defun osx-plist-process-dict (xml)
  "Place the key-value pairs of plist XML into HASH."
  (let ((hash (make-hash-table :test 'equal))
        (current-key nil))
    (mapc
     (lambda (child)
       (when (not (stringp child)) ; ignore inter-tag whitespace
         (let ((name (xml-node-name child))
               (children (xml-node-children child)))
           (if (eq name 'key)
               (setq current-key (osx-plist-node-value child))
             (puthash current-key
                      (osx-plist-node-value child)
                      hash)))))
     (xml-node-children xml))
    hash))

(defun osx-plist-node-value (node)
  "Return a Lisp value equivalent of plist node NODE."
  (let ((name (xml-node-name node))
        (children (xml-node-children node)))
    (cond ((eq name 'false) nil)
          ((eq name 'true)  t)
          ((memq name '(key string))
           (apply 'concat children))
          ((eq name 'dict)
           (osx-plist-process-dict node))
          ((eq name 'array)
           (osx-plist-process-array node)))))

(defun osx-plist-p (xml)
  "Non-null if XML appears to be an Apple plist."
  (and xml (listp xml) (eq (xml-node-name xml) 'plist)))

(defun osx-plist-parse-file (file)
  "Parse the plist file FILE into an elisp hash table."
  (let ((xml (car (xml-parse-file file))))
    (when (osx-plist-p xml)
      (osx-plist-node-value (car (xml-get-children xml 'dict))))))

;; Example code: initialize Emacs' environment based on your
;; environment.plist.

(defun osx-plist-update-exec-path ()
  "Update `exec-path' from the PATH environment variable."
  (let ((path (getenv "PATH")))
    (mapc (lambda (dir)
            (add-to-list 'exec-path dir))
          (parse-colon-path path)))
  exec-path)

(defun osx-plist-update-environment ()
  "Load environment variables in `osx-env-file' into Emacs' environment."
  (let ((plist (osx-plist-parse-file osx-env-file)))
    (maphash 'setenv plist)
    (when (gethash "PATH" plist)
      (osx-plist-update-exec-path))))

(provide 'osx-plist)
;;; osx-plist.el ends here
