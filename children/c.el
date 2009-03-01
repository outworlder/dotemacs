;; Loading gdb-mi
(require 'gdb-ui)


;; Loading the cc mode script
(require 'cc-mode)
(require 'cc-styles)

;; Loading the Lua Mode (to edit premake scripts)
(require 'lua-mode)

;; Automatic return after {, } and ;
(setq c-auto-newline t)

;;-----------------------------------------------------------------------------
;; Setting the C style
;;-----------------------------------------------------------------------------

(add-hook 'c-mode-hook
          (lambda ()
            (c-set-style "ellemtel")
            (setq c-basic-offset 8)
            (c-toggle-auto-newline t)
            (turn-on-auto-fill)
            (c-toggle-hungry-state t)))

;; Setting the C++ style
(add-hook 'c++-mode-hook
          (lambda ()
            (c-set-style "ellemtel")
            (setq c-basic-offset 8)
            (c-toggle-auto-newline t)
            (turn-on-auto-fill)
            (c-toggle-hungry-state t)))
