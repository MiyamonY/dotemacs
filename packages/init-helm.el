(use-package helm-conifg
  :bind
  (("C-x b" . helm-for-files)
   ("C-x C-b" . helm-for-files)
   ("M-x" . helm-M-x)
   ("C-x f" . helm-find-files)
   ("C-x C-f" . helm-find-files)
   ("M-y" . helm-show-kill-ring))
  :init
  (progn
    (setq helm-for-files-preferred-list
          '(helm-source-buffers-list
            helm-source-recentf
            helm-source-files-in-current-dir
            helm-source-ghq
            helm-source-locate))
    (setq helm-boring-buffer-regexp-list
          (-concat '("\*Compile-Log\*"
                     "magit.*" "~/\.Trash/.*") helm-boring-buffer-regexp-list))
    (setq helm-boring-file-regexp-list
          (-concat '("\\.pyc$" "\GPATH\..*$" "\GRTAG\..*$" "GSYMS$" "GTAGS$") helm-boring-file-regexp-list))
    (setq helm-ff-skip-boring-files t)))
