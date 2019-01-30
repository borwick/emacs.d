; cygwin uses FQDN
(require 'setup-cygwin)
(setq system-name (car (split-string system-name "\\.")))

; can't figure out what's broken with flymake :-(
(remove-hook 'find-file-hook 'flymake-find-file-hook)

(when (not (require 'cygwin-mount nil t))
  (package-refresh-contents)
  (package-install 'cygwin-mount))

(require 'cygwin-mount)
(cygwin-mount-activate)
