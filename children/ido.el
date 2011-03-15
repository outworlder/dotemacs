;; Loading IDO
(require 'ido)

;; Turning IDO on
(ido-mode t)
(ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point nil)
(setq ido-auto-merge-work-directories-length -1) ;What's this?


;; Using IDO for M-x
(setq ido-execute-command-cache nil)
 (defun ido-execute-command ()
   (interactive)
   (call-interactively
    (intern
     (ido-completing-read
      "M-x "
      (progn
        (unless ido-execute-command-cache
          (mapatoms (lambda (s)
                      (when (commandp s)
                        (setq ido-execute-command-cache
                              (cons (format "%S" s) ido-execute-command-cache))))))
        ido-execute-command-cache)))))
    
 (add-hook 'ido-setup-hook
           (lambda ()
             (setq ido-enable-flex-matching t)
             (global-set-key "\M-x" 'ido-execute-command)))


;; Taken from http://stackoverflow.com/questions/905338/can-i-use-ido-completing-read-instead-of-completing-read-everywhere

(defvar ido-enable-replace-completing-read t
  "If t, use ido-completing-read instead of completing-read if possible.

Set it to nil using let in around-advice for functions where the
original completing-read is required.  For example, if a function
foo absolutely must use the original completing-read, define some
advice like this:

(defadvice foo (around original-completing-read-only activate)
  (let (ido-enable-replace-completing-read) ad-do-it))")

;; Replace completing-read wherever possible, unless directed otherwise
(defadvice completing-read
  (around use-ido-when-possible activate)
  (if (or (not ido-enable-replace-completing-read) ; Manual override disable ido
          (boundp 'ido-cur-list)) ; Avoid infinite loop from ido calling this
      ad-do-it
    (let ((allcomp (all-completions "" collection predicate)))
      (if allcomp
          (setq ad-return-value
                (ido-completing-read prompt
                               allcomp
                               nil require-match initial-input hist def))
        ad-do-it))))

