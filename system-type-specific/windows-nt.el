(setq venv-executables-dir "bin")
(global-set-key (kbd "<C-lwindow>") 'ignore)

(setq cygwin-config (concat dotfiles-dir "system-type-specific/cygwin.el"))
(if (and (file-exists-p "c:/cygwin64") (file-exists-p cygwin-config))
      (load cygwin-config))

(setq default-directory "~/")
