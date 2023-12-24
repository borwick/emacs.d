; IF YOU ARE HAVING ISSUES, ENSURE org-plus-contrib is installed!
; you can do this via
;  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
;  (package-initialize)
;  M-x package-list-packages
;  select org-plus-contrib and install
; then restart Emacs, maybe twice

; If you are on MacOS, you should also disable the keyboard shortcuts
; for changing input sources (which are bound to ctrl+space)

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;(package-initialize)

;; (require 'benchmark-init)
;; ;; To disable collection of benchmark data after init is done.
;; (add-hook 'after-init-hook 'benchmark-init/deactivate)



(setq dotfiles-dir (expand-file-name "~/.emacs.d/"))
(add-to-list 'load-path (car (file-expand-wildcards (concat dotfiles-dir "elpa/org-9*"))))
(require 'org)
; (require 'org-loaddefs)

(require 'package)
(let (
      (file-name-handler-alist nil)
      (gc-cons-threshold 100000000)
      )
     (org-babel-load-file
     (expand-file-name "settings.org"
                   user-emacs-directory))
     (put 'set-goal-column 'disabled nil)
     )
