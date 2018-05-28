(setq init-emacs-local-files "~/.emacs.d/locals")

;; utility
(el-get-bundle use-package)
(require 'bind-key)

;; theme
(el-get-bundle dracula/emacs :name dracula-theme)

;; elscreen
(el-get-bundle elscreen)

;; git
(el-get-bundle magit)
(el-get-bundle git-gutter+)

;; interface
(el-get-bundle helm)
(el-get-bundle helm-swoop)
(el-get-bundle nyan-mode)
(el-get-bundle rainbow-delimiters)
(el-get-bundle smart-mode-line)

;; Japansese
(el-get-bundle ddskk)

;; ace-jump
(el-get-bundle ace-jump-mode)
(el-get-bundle ace-window)
(el-get-bundle ace-isearch)

;; prog
(el-get-bundle smartparens)
(el-get-bundle yasnippet)
(el-get-bundle yatemplate)
(el-get-bundle highlight-symbol)
