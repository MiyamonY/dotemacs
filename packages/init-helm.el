(use-package helm-conifg
  :bind
  (("C-x b" . helm-mini)
   ("C-x C-b" . helm-mini)
   ("M-x" . helm-M-x)
   ("C-x f" . helm-find-files)
   ("C-x C-f" . helm-find-files)
   ("M-y" . helm-show-kill-ring))
  :init
  (progn
    (setq helm-ff-skip-boring-files t))
  :config
  (progn
    (setq helm-boring-file-regexp-list (-concat '("\\.pyc$") helm-boring-file-regexp-list))))
