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

(set-key "<C-f9>" 'find-file-in-project)
(set-key "<C-f5>" 'jira-show-assigned-me)

(set-key "<C-f10>" 'find-grep-dired)
    
(set-key "<C-f7>" 'count-todos-in-buffer)
(set-key "C-M-g" 'ido-goto-symbol)

(global-set-key "\M-x" 'ido-execute)
(set-key "<menu>" 'ido-execute)
(global-set-key (kbd "M-/") 'hippie-expand)
