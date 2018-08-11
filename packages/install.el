(setq init-emacs-local-files "~/.emacs.d/locals")

;; utility
(el-get-bundle use-package)
(require 'bind-key)

;; theme
(el-get-bundle dracula/emacs :name dracula-theme)

;; elscreen
(el-get-bundle elscreen)

;; mode-line(elscreenの後に呼び出すこと)
(el-get-bundle powerline)

;; git
(el-get-bundle magit)
(el-get-bundle magit-gitflow)
(el-get-bundle git-gutter+)
(el-get-bundle git-gutter-fringe+)

;; interface
(el-get-bundle company-mode)
(el-get-bundle helm)
(el-get-bundle helm-ag)
(el-get-bundle helm-ghq)
(el-get-bundle helm-descbinds)
(el-get-bundle helm-swoop)
(el-get-bundle helm-company)
(el-get-bundle rainbow-delimiters)
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
(el-get-bundle yasnippet-snippets)
(el-get-bundle yatemplate)
(el-get-bundle highlight-symbol)
(el-get-bundle aggressive-indent)
(el-get-bundle flycheck)
(el-get-bundle flycheck-pos-tip)
(el-get-bundle flycheck-color-mode-line)
(el-get-bundle emmet-mode)
(el-get-bundle web-mode)
(el-get-bundle js2-mode)
(el-get-bundle gtags)
(el-get-bundle helm-gtags)
(el-get-bundle company-jedi)
(el-get-bundle py-yapf)
(el-get-bundle lua-mode)
(el-get-bundle yaml-mode)

;;; golang
(el-get-bundle go-mode)
(el-get-bundle go-company)

;; operation
(el-get-bundle undohist)
(el-get-bundle undo-tree)
(el-get-bundle emacsmirror/sequential-command)


;; doc
(el-get-bundle asciidoc)
(el-get-bundle adoc-mode)
(el-get-bundle recentf-ext)
