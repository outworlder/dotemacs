;;-----------------------------------------------------------------------------
;; Keyboard shortcuts
;;-----------------------------------------------------------------------------

;; Go to a line using meta-x g
(global-set-key "\M-g" 'goto-line)

;; shortcut to reparse the buffer
;;(set-key "<C-f8>" 'mmm-parse-buffer)
(set-key "<C-f8>" 'rinari-find-config-file)

;; (set-key "<C-left>" 'emms-previous)
;; (set-key "<C-right>" 'emms-next)
;; (set-key "<C-up>" 'emms-pause)
;; (set-key "<C-down>" 'emms-show)

(set-key "<f11>" 'gdb)
(set-key "<f12>" 'gdb-many-windows)

(set-key "<C-f1>" 'compile)

(set-key "<M-S-f9>" 'find-file-in-project)
(set-key "<C-f5>" 'jira-show-assigned-me)

(set-key "<M-S-f12>" 'egg-status)

(set-key "<C-f10>" 'vc-git-grep)
    
(set-key "<C-f7>" 'count-todos-in-buffer)

(global-set-key "\C-c\C-r" 'recentf-open-files)

(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cs" 'org-schedule)

(if (featurep 'ido)
    (progn
      (set-key "C-M-g" 'ido-goto-symbol)
      (global-set-key "\M-x" 'ido-execute)
      (set-key "<menu>" 'ido-execute)))

(global-set-key (kbd "M-/") 'hippie-expand)

(if (featurep 'anything)
    (progn
      (set-key "S-s-SPC" 'anything)
      (set-key "C-M-g" 'anything-imenu)))


