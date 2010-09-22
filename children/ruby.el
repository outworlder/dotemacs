(require 'ruby-mode)
;; (require 'ruby-electric)
(require 'inf-ruby)

(defun ruby-insert-end ()
  (interactive)
  (insert "end")
  (ruby-indent-line t)
  (end-of-line))

(setq ri-ruby-script (expand-file-name "~/.emacs.children/support/ri-emacs.rb"))
(autoload 'ri "~/.emacs.children/support/ri-ruby.el" nil t)

(maybe-require 'rdebug)

;; Loading nXHTML
(load "~/.emacs.d/nxhtml/autostart")
(require 'nxhtml-mumamo)
;;(require 'nxml-mode)

(require 'speedbar)

;; Loading emacs rails
;; (require 'snippet)
;; (require 'ruby-mode)
(require 'find-recursive)

(defface erb-face
  `((t (:background "dark blue")))
  "Default inherited face for ERB tag body"
  :group 'rhtml-faces)

(defface erb-delim-face
  `((t (:background "dark blue")))
  "Default inherited face for ERB tag delimeters"
  :group 'rhtml-faces)

;;(set-face-background 'mumamo-background-chunk-submode "midnight blue")
;; Lighter shade of blue for the mumamo chunks (used in nxhtml-mode)
(set-face-background 'mumamo-background-chunk-major "#111133")
(set-face-background 'mumamo-background-chunk-submode1 "#111166")

(setq
 nxhtml-global-minor-mode t
 mumamo-chunk-coloring 'submode-colored
 nxhtml-skip-welcome t
 indent-region-mode t
 rng-nxml-auto-validate-flag nil
 nxml-degraded t)

;; (set-face-background 'mumamo-background-chunk-submode "midnight blue")

(maybe-require 'rails)
(maybe-require 'rails-view-minor-mode)

(defun nxml-rails-mode ()
  (interactive)
  (eruby-nxhtml-mumamo))

;; Changing rails' filenames list to use nxml-mode instead of html-mode for views.
;; TODO: This works. However, the rails mode is not activated. The problem might be a hook
(if (assoc "\\.rhtml$" auto-mode-alist)
    (setcdr
     (assoc "\\.rhtml$" auto-mode-alist) 'nxml-rails-mode)
  (add-to-list 'auto-mode-alist '("\\.rhtml$" . nxml-rails-mode)))

;; ;; Converting eruby-nxhtml-mumamo to nxml-rails-mode
(mapc (lambda(element)
	(if (eql (cdr element) 'eruby-nxhtml-mumamo-mode)
	    (setcdr element 'nxml-rails-mode))) auto-mode-alist)

(add-to-list 'auto-mode-alist '("\\.html.erb$" . nxml-rails-mode))

;; YAML Mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(add-hook 'yaml-mode-hook
          '(lambda ()
             (define-key yaml-mode-map "\C-m" 'newline-and-indent)))


;;(add-hook 'ruby-mode-hook 'turn-on-font-lock)
(add-hook 'ruby-mode-hook
	  (lambda ()
	    (define-key ruby-mode-map "\C-m" 'newline-and-indent)))
            ;(require 'ruby-electric)
            ;(ruby-electric-mode t)))

;; TODO: Only check this for .rb files.
;; Warns if we are saving a file with a debugger statement
(defun check-ruby-debugger-statement ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (word-search-forward "debugger" nil t nil)
        (message "Warning: There's a debugger statement in the code."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; find-file-in-project

(defvar rinari-project-files-table ())

(defun populate-project-files-table (file)
  (if (file-directory-p file)
      (mapc 'populate-project-files-table (directory-files file t "^[^\.]"))
    (let* ((file-name (file-name-nondirectory file))
           (existing-record (assoc file-name project-files-table))
           (unique-parts (get-unique-directory-names file (cdr existing-record))))
      (if existing-record
          (let ((new-key (concat file-name " - " (car unique-parts)))
                (old-key (concat (car existing-record) " - " (cadr unique-parts))))
            (setf (car existing-record) old-key)
            (setq project-files-table (acons new-key file project-files-table)))
        (setq project-files-table (acons file-name file project-files-table))))))

(defun get-unique-directory-names (path1 path2)
  (let* ((parts1 (and path1 (split-string path1 "/" t)))
         (parts2 (and path2 (split-string path2 "/" t)))
         (part1 (pop parts1))
         (part2 (pop parts2))
         (looping t))
    (while (and part1 part2 looping)
           (if (equal part1 part2)
               (setq part1 (pop parts1) part2 (pop parts2))
             (setq looping nil)))
    (list part1 part2)))

;; (defun find-file-in-project (file)
;;   (interactive (list (if (functionp 'ido-completing-read)
;;                          (ido-completing-read "Find file in project: " (mapcar 'car (project-files)))
;;                          (completing-read "Find file in project: " (mapcar 'car (project-files))))))
;;   (find-file (cdr (assoc file project-files-table))))


(defun find-file-in-project (file)
  (interactive (list (if (functionp 'ido-completing-read)
                         (ido-completing-read "Find file in project: " (mapcar 'car (project-files)))
                         (completing-read "Find file in project: " (mapcar 'car (project-files))))))
  (find-file (cdr (assoc file project-files-table))))


(defun rails-root (&optional dir)
  (or dir (setq dir default-directory))
  (if (file-exists-p (concat dir "config/environment.rb"))
      dir
    (if (equal dir  "/")
        nil
      (rails-root (expand-file-name (concat dir "../"))))))


;; TODO: Add only the lib directory. Currently adding the whole rails root directory.
(defun project-files (&optional file)
                                        ; uncomment these lines if it's too slow to load the whole project-files-table
                                        ;  (when (or (not project-files-table) ; initial load
                                        ;           (not (string-match (rails-root) (cdar project-files-table)))) ; switched projects
  (setq project-files-table nil)
  (append
   (populate-project-files-table (or file (concat (rails-root) "/app")))
   (populate-project-files-table (or file (concat (rails-root) "/lib"))))
  project-files-table)

(setq rinari-config-files
  '("config/environment.rb"
    "config/database.yml"
    "config/routes.rb"
    "config/deploy.rb"
    "config/initializers/fortaleza.rb"
    "db/schema.rb"))

;; Flymake stuff
(require 'flymake)

;; I don't like the default colors :)
(set-face-background 'flymake-errline "red4")
(set-face-background 'flymake-warnline "dark slate blue")

;; Invoke ruby with '-c' to get syntax checking
(defun flymake-ruby-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
	 (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "ruby" (list "-c" local-file))))

(push '(".+\\.rb$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("Rakefile$" flymake-ruby-init) flymake-allowed-file-name-masks)

(push '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3) flymake-err-line-patterns)

(defun ruby-insert-hash-string ()
  (interactive)
  "Inserts a #{} but only if inside a string"
  (let ((stringp (fourth (syntax-ppss)))
	(skeleton-end-newline nil))
    (if stringp
	(skeleton-insert '(nil ?# ?{ _ ?} nil))
      (skeleton-insert '(nil ?# _)))))

(add-hook 'ruby-mode-hook
	  (lambda ()
	    (local-set-key "#" 'ruby-insert-hash-string)))


(add-hook 'ruby-mode-hook
          (lambda()
            (inf-ruby-keys)) t)


(add-hook 'ruby-mode-hook
          (lambda ()
	     ;; Don't want flymake mode for ruby regions in rhtml files and also on read only files
	     (if (and (not (null buffer-file-name)) (file-writable-p buffer-file-name))
		 (flymake-mode t))) t)

(add-hook 'ruby-mode-hook (lambda ()
			    (local-set-key [f1] 'ri)
			    (local-set-key "\M-\C-i" 'ri-ruby-complete-symbol)
			    (local-set-key [f4] 'ri-ruby-show-args)
			    ) t)

(add-hook 'ruby-mode-hook
	  (lambda ()
	    (rvm-activate-corresponding-ruby)))

(add-hook 'after-save-hook 'check-ruby-debugger-statement)
(add-hook 'ruby-mode-hook 'check-ruby-debugger-statement)

