; cygwin uses FQDN
(setq system-name (car (split-string system-name "\\.")))
(require 'cygwin-mount)
(cygwin-mount-activate)
