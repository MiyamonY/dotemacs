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
(el-get-bundle helm-ag)
(el-get-bundle helm-ghq)
(el-get-bundle helm-swoop)
(el-get-bundle yasuyk/ac-helm)
(el-get-bundle nyan-mode)
(el-get-bundle rainbow-delimiters)
(el-get-bundle smart-mode-line)
(el-get-bundle emacsmirror/hl-line-plus :name hl-line+)
(el-get-bundle pcre2el)
(el-get-bundle popwin)

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
(el-get-bundle aggressive-indent)
(el-get-bundle flycheck)
(el-get-bundle flycheck-pos-tip)

;;; golang
(el-get-bundle go-mode)
(el-get-bundle go-autocomplete)

;; operation
(el-get-bundle undohist)
(el-get-bundle undo-tree)
(el-get-bundle emacsmirror/sequential-command)

;; input
(el-get-bundle auto-complete)

;; doc
(el-get-bundle asciidoc)
(el-get-bundle adoc-mode)
