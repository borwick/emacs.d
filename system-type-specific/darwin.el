; darwin uses FQDN
(setq system-name (car (split-string system-name "\\.")))

(if (>= emacs-major-version 23)
    (progn
      (when (not (require 'exec-path-from-shell nil t))
	(package-refresh-contents)
	(package-install 'exec-path-from-shell))
      ))
(exec-path-from-shell-initialize)

(add-hook 'org-pomodoro-overtime-hook
	  (lambda () (ns-do-applescript "display notification \"Pomodoro alert\"")))

(add-hook 'org-pomodoro-break-finished-hook
	  (lambda () (ns-do-applescript "display notification \"Break's over\"")))
