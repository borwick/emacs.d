; darwin uses FQDN
(setq system-name (car (split-string system-name "\\.")))

(setq alert-default-style 'notifier)

; use JuliaMono if it's installed
(if (member "JuliaMono" (font-family-list))
    (set-frame-font "JuliaMono-16" nil t)
  (set-frame-font "Monaco-18" nil t))

; required for delete-by-moving-to-trash to work
(setq trash-directory "~/.Trash")

; https://emacs.stackexchange.com/a/68568
; -- fix for emacs package updates "error in process sentinel"
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

; make insert key work
; thanks to https://emacs.stackexchange.com/a/30288:
(global-set-key [C-help] #'clipboard-kill-ring-save)
(global-set-key [S-help] #'clipboard-yank)
(global-set-key [help] #'overwrite-mode)

; https://xenodium.com/my-emacs-eye-candy/
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . nil))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
