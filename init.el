; credit to...
; https://github.com/topfunky/emacs-starter-kit/blob/master/init.el
; http://www.masteringemacs.org/articles/2010/10/04/beginners-guide-to-emacs/

; server
(server-start)

(setq dotfiles-dir (expand-file-name "~/.emacs.d/"))
(add-to-list 'load-path (concat dotfiles-dir "lisp/"))
(add-to-list 'load-path (concat dotfiles-dir "elpa/"))

; the high road to mastering emacs
(require 'no-easy-keys)
(no-easy-keys 1)

; interactive emacs package addition
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/")
	     '("marmalade" .
	       "http://marmalade-repo.org/packages/"))
(package-initialize)

; packages that are needed for the rest of this init.el:
(if (>= emacs-major-version 23)
    (progn
      (when (not (require 'pony-mode nil t))
	(package-refresh-contents)
	(package-install 'pony-mode))
      (when (not (require 'virtualenvwrapper nil t))
	(package-refresh-contents)
	(package-install 'virtualenvwrapper))
      (when (not (require 'yasnippet nil t))
	(package-refresh-contents)
	(package-install 'yasnippet))
      (when (not (require 'markdown-mode nil t))
	(package-refresh-contents)
	(package-install 'markdown-mode))
      (when (not (require 'graphviz-dot-mode nil t))
	(package-refresh-contents)
	(package-install 'graphviz-dot-mode))
      (when (not (require 'jedi nil t))
	(package-refresh-contents)
	(package-install 'jedi))
      (when (not (require 'magit nil t))
	(package-refresh-contents)
	(package-install 'magit))
      (when (not (require 'puppet-mode nil t))
	(package-refresh-contents)
	(package-install 'puppet-mode))
      (when (not (require 'yaml-mode nil t))
	(package-refresh-contents)
	(package-install 'yaml-mode))
      ))



; custom files for M-x customize
(setq custom-file (concat dotfiles-dir "custom.el"))
(load custom-file)

; no ads
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)

; completion thing
(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)

; subwords
(subword-mode 1)

; backup directories rather than *~ files

(setq backup-directory-alist
   `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
   `((".*" ,temporary-file-directory t)))

(require 'python)
; (require 'python-mode)
(require 'json-mode)
(require 'pony-mode)
(require 'flymake)

; https://github.com/akaihola/flymake-python
(when (load "flymake" t)
  (defun flymake-pylint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "~/.emacs.d/bin/pyflymake.py" (list local-file))))
      ;;     check path

  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pylint-init)))
(add-hook 'find-file-hook 'flymake-find-file-hook)

(defun flymake-html-init ())
(defun flymake-simple-tex-init ())

; http://nileshk.com/2009/06/13/prompt-before-closing-emacs.html
(defun ask-before-closing ()
  "Ask whether or not to close, and then close if y was pressed"
  (interactive)
  (if (y-or-n-p (format "Are you sure you want to exit Emacs? "))
      (if (< emacs-major-version 22)
          (save-buffers-kill-terminal)
        (save-buffers-kill-emacs))
    (message "Canceled exit")))

(when window-system
  (global-set-key (kbd "C-x C-c") 'ask-before-closing))

(require 'yasnippet)
(yas-global-mode 1)

; FAQ 5.50--only one space after period
(setq sentence-end-double-space nil)

(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; (autoload 'django-nxhtml-mumamo-mode "~/.emacs.d/lisp/nxhtml/autostart.el")
;; (setq auto-mode-alist
;;       (append '(("\\.html?$" . django-nxhtml-mumamo-mode)) auto-mode-alist))
(setq mumamo-background-colors nil)
(add-to-list 'auto-mode-alist '("\\.html$" . django-nxhtml-mumamo-mode))
(setq pony-enable-template-mode nil)

; From https://gist.github.com/tkf/3951163
;; Workaround the annoying warnings:
;;    Warning (mumamo-per-buffer-local-vars):
;;    Already 'permanent-local t: buffer-file-name
(when (and (>= emacs-major-version 24)
           (>= emacs-minor-version 2))
  (eval-after-load "mumamo"
    '(setq mumamo-per-buffer-local-vars
           (delq 'buffer-file-name mumamo-per-buffer-local-vars))))


; http://www-cdf.fnal.gov/~sthrlnd/emacs_help.html:
;; Kills all them buffers except scratch
;; optained from http://www.chrislott.org/geek/emacs/dotemacs.html
(defun nuke-all-buffers ()
  "kill all buffers, leaving *scratch* only"
  (interactive)
  (delete-other-frames)
  (delete-other-windows)
  (mapcar (lambda (x) (kill-buffer x))
	  (buffer-list))
  )

(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)                      ; optional
(setq jedi:complete-on-dot t)                 ; optional
(jedi:setup)

; just reload files if they are updated
(global-auto-revert-mode t)

; require is only so we can remove the vc hook:
(require 'vc)
(remove-hook 'find-file-hooks 'vc-find-file-hook)

(set-variable 'python-fill-docstring-style 'django)


(require 'virtualenvwrapper)
(venv-initialize-interactive-shells) ;; if you want interactive shell support
(venv-initialize-eshell) ;; if you want eshell support
(setq venv-location (expand-file-name "~/.virtualenvs/"))

; https://github.com/porterjamesj/virtualenvwrapper.el
; if you add a .dir-locals.el for 'project-venv-name' e.g.
;   ((python-mode . ((project-venv-name . "myproject-env"))))
; then the correct virtualenv will be run!
(add-hook 'python-mode-hook (lambda ()
                              (hack-local-variables)
                              (venv-workon project-venv-name)))

(autoload 'puppet-mode "puppet-mode" "Major mode for editing puppet manifests")
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))


(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

(require 'word-count)

(add-hook 'markdown-mode-hook
          (lambda ()
            ;; disable electric indent
            (setq-local electric-indent-mode nil)
	    ))
(global-set-key (kbd "C-x g") 'magit-status)

(add-hook 'markdown-mode-hook 'turn-on-visual-line-mode)






; SYSTEM-TYPE config
; (may change system-name)
(setq system-type-as-string (replace-regexp-in-string "/" "-" (prin1-to-string system-type)))
(setq system-type-specific-config (concat dotfiles-dir "system-type-specific/" system-type-as-string ".el"))
(if (file-exists-p system-type-specific-config) (load system-type-specific-config))

; SYSTEM-NAME config
(setq system-name-specific-config (concat dotfiles-dir "system-name-specific/" system-name ".el"))
(if (file-exists-p system-name-specific-config) (load system-name-specific-config))

; USER config
(setq user-specific-config (concat dotfiles-dir "user-specific/" user-login-name ".el"))
(if (file-exists-p user-specific-config) (load user-specific-config))

; LOCAL config
(setq local-specific-config (concat dotfiles-dir "local.el"))
(if (file-exists-p local-specific-config) (load local-specific-config))
