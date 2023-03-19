(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq use-package-enable-imenu-support t)

(straight-use-package 'use-package)

(use-package emacs
  :init
  (setq jit-lock-defer-time 0.05)

  (setq-default bidi-display-reordering nil)

  (setq gc-cons-threshold (* 256 1024 1024))

  (setq read-process-output-max (* 1024 1024))

  (let* ((font-name "Cica")
         (default-font (format "%s:weight=normal:slant=normal" font-name))
         (fontset-base-name "rdd")
         (fontset-name (format "fontset-%s" fontset-base-name)))
    (create-fontset-from-ascii-font default-font nil fontset-base-name)
    (set-fontset-font fontset-name 'unicode (font-spec :family font-name) nil 'append)
    (set-frame-font fontset-name)
    (setq default-frame-alist
	  `((vertical-scroll-bars . nil)
	    (font . ,fontset-name)))
    (set-frame-parameter (selected-frame) 'font fontset-name)
    (toggle-scroll-bar -1)
    (remove-hook 'after-make-frame-functions #'add-font-setting))

  (add-hook 'after-make-frame-functions #'add-font-setting)

  (setq auto-save-list-file-prefix nil)
  (defalias 'yes-or-no-p 'y-or-n-p)

  (tool-bar-mode -1)

  (toggle-scroll-bar -1)

  (show-paren-mode 1)
  (setq show-paren-dilay 0.2)
  (setq show-paren-style 'expression)

  (setq-default show-trailing-whitespace t)
  (add-hook 'before-save-hook 'delete-trailing-whitespace)

  (delete-selection-mode t)
  (setq kill-whole-line t)

  (setq bookmark-default-file
        (locate-user-emacs-file (convert-standard-filename "locals/bookmarks")))

  (setq make-backup-files nil)
  (setq auto-save-default nil)

  (setq-default prettify-symbols-alist
	        '(("lambda" . 955)
		  ("->" . 8594)
		  ("=>" . 8658)
		  ("!=" . 8800)
		  ("nil". 8709)
		  ("<=" . 8804)
		  (">=" . 8805)
		  ("==" . 10869)
		  (":=" . 8788)))

  (setq prettify-symbols-unprettify-at-point 'right-edge)

  (add-hook 'prog-mode-hook 'prettify-symbols-mode)

  (global-prettify-symbols-mode +1)

  (setq-default fill-column 120)

  (global-auto-revert-mode t)

  ;; tramp-mode
  (setq tramp-persistency-file-name
        (locate-user-emacs-file (convert-standard-filename "locals/tramp")))

  (menu-bar-mode -1)

  ;;  tab
  (tab-bar-mode)
  (tab-bar-history-mode +1)

  (setq-default indent-tabs-mode nil)

  (bind-key "C-; p" #'tab-previous)
  (bind-key "C-; n" #'tab-next)
  (bind-key "C-; c" #'tab-new)

  (setq warning-suppress-types '((lsp-mode)))

  (setenv "PATH"
          (concat (getenv "PATH")
                  ":"
                  (replace-regexp-in-string "\n$" ""
                                            (shell-command-to-string "npm bin -g"))))

  (setq enable-recursive-minibuffers nil))

(setq straight-use-package-by-default t)

(use-package exec-path-from-shell
  :init
  (setq exec-path-from-shell-shell-name "fish")
  :config
  (exec-path-from-shell-copy-envs '("PATH")))

(use-package dashboard
  :bind (:map dashboard-mode-map
	      ("f" . consult-buffer)
	      ("b" . consult-buffer)
	      ("a" . org-agenda))
  ;; ("g" . counsel-ghq))
  :config
  (setq dashboard-center-content t)
  (setq dashboard-set-navigator t)
  (setq dashboard-set-footer nil)

  (defun dashboard-insert-commands (list-size)
    (insert "   Recentf: (f)   Bookmarks: (b)   Agend: (a)  ghq: (g)"))

  (add-to-list 'dashboard-item-generators
	       '(commands . dashboard-insert-commands))
  (setq dashboard-items '((recents  . 10)
			  (commands . t)))
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))))
(dashboard-setup-startup-hook)

(use-package bind-key
  :bind (("C-h" . delete-backward-char)
	 ("C-c l" . toggle-truncate-lines)
	 ("C-c j" . split-window-horizontally)
	 ("C-c u" . split-window-vertically)
	 ("C-c d f" . describe-function)
	 ("C-c d v" . describe-variable)
	 ("C-c d m" . describe-mode)
	 ("C-c d s" . describe-symbol)
	 ("C-c d b" . describe-bindings)
	 ("C-c d k" . describe-key)
	 ("C-c i" .
	  (lambda ()
	    (interactive)
	    (find-file (locate-user-emacs-file "init.el"))))))

(use-package hydra
  :config
  (defhydra hydra-zoom (global-map "<f2>")
    "zoom"
    ("g" text-scale-increase "in")
    ("l" text-scale-decrease "out")))

(use-package which-key
  :hook (after-init . which-key-mode))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-height 20)
  (setq doom-modeline-icon t)
  (setq doom-modeline-major-mode-icon t)
  (setq inhibit-compacting-font-caches t)
  (setq find-file-visit-truename t))

(use-package magit
  :commands (magit-status)
  :bind (("C-x g" . magit-status))
  :init
  (setq transient-history-file
	(locate-user-emacs-file (convert-standard-filename "locals/transient/history.el")))
  (setq magit-diff-refine-ignore-whitespace t)
  (setq magit-repository-directories '(("~/src/github.com" . 2))))

(use-package magit-gitflow
  :after (magit)
  :hook (magit-mode . turn-on-magit-gitflow))

(use-package fringe-helper)

(use-package git-gutter+
  :after (fringe-helper)
  :hook (after-init . global-git-gutter+-mode))

(use-package git-gutter-fringe+
  :after (git-gutter+))

(use-package rainbow-delimiters
  :commands (rainbow-delimiters-mode)
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package all-the-icons)

(use-package symbol-overlay
  :hook (prog-mode . symbol-overlay-mode)
  :config
  (set-face-attribute 'symbol-overlay-default-face nil :background "gray40" :foreground "white"))

(use-package hl-line+
  :init
  (setq hl-line-idle-interval 0.1)
  :config
  (toggle-hl-line-when-idle))

(use-package display-line-numbers
  :hook
  ((prog-mode yaml-mode systemd-mode org-mode) . display-line-numbers-mode)
  :config
  (setq-default indicate-empty-lines t)
  (setq indicate-buffer-boundaries 'left)
  (set-face-attribute 'line-number-current-line nil :foreground "#bd93f9" :background "gray30"))

(use-package undo-tree
  :bind
  (("C-." . undo-tree-redo)
   ("C-/" . undo-tree-visualize)
   ("C-/" . undo-tree-visualize)
   :map undo-tree-visualizer-mode-map
   ("C-m" . undo-tree-visualizer-quit)
   ("C-g" . undo-tree-visualizer-quit)
   ("C-/" . undo-tree-visualize-undo)
   ("C-." . undo-tree-visualize-redo))
  :config
  (advice-add #'undo-tree-overridden-undo-bindings-p
	      :filter-return
	      (lambda (x) nil))
  (setq undo-tree-mode-lighter "")
  (global-undo-tree-mode 1))

(use-package sequential-command-config
  :straight sequential-command
  :hook (after-init . sequential-command-setup-keys)
  :config
  (define-sequential-command fill-unfill-paragraph fill-paragraph delete-indentation)
  (bind-key "M-q" #'fill-unfill-paragraph))


(use-package smartparens-config
  :straight smartparens
  :hook (after-init . smartparens-global-mode))

(use-package shrink-whitespace
  :bind ("M-SPC" . shrink-whitespace))

(use-package recentf
  :init
  (setq recentf-save-file
	(locate-user-emacs-file (convert-standard-filename "locals/recentf")))
  (setq recentf-max-saved-items 500)
  (setq recentf-max-menu-items 15)
  (setq recentf-auto-cleanup 'never))

(use-package company
  :hook (prog-mode . company-mode)
  :bind (:map company-active-map
	      ("M-n" . nil)
	      ("M-p" . nil)
	      ("C-n" . company-select-next)
	      ("C-p" . company-select-previous)
	      ("C-h" . nil))
  :config
  (setq completion-ignore-case t)
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefx-length 3)
  (setq company-selection-wrap-around t)
  (setq company-tooltip-limit 20)      ; 候補を何個出すか
  )

(use-package company-box
  :hook (company-mode . company-box-mode)
  :custom ((company-box-icons-alist 'company-box-icons-all-the-icons)))

(use-package vertico
  :custom
  ((vertico-cycle t)
   (vertico-count 20))
  :init
  (vertico-mode))

(use-package vertico-directory
  :after (vertico)
  :straight nil
  :ensure nil
  :load-path "straight/build/vertico/extensions"
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package savehist
  :init
  (savehist-mode))

(use-package orderless
  :after (vertico)
  :custom
  ((completion-styles '(orderless basic))
   (completion-category-defaults nil)
   (completion-category-overrides '((file (styles partial-completion))))))

(defun my-filename-upto-parent ()
  "Move to parent directory like \"cd ..\" in find-file."
  (interactive)
  (let ((sep (eval-when-compile (regexp-opt '("/" "\\")))))
    (save-excursion
      (left-char 1)
      (when (looking-at-p sep)
        (delete-char 1)))
    (save-match-data
      (when (search-backward-regexp sep nil t)
        (right-char 1)
        (filter-buffer-substring (point) (line-end-position)
                                 #'delete)))))

(use-package consult
  :after (vertico)
  :commands (consult-line consult-locate)
  :bind (("C-s" . consult-line)
         ("C-x b" . consult-buffer)
         ("C-x C-b" . consult-buffer)
         ("C-x l" . consult-locate)
         ("M-g g" . consult-goto-line)
         (:map vertico-map
               ("C-l" . my-filename-upto-parent))))

(use-package affe
  :after (consult)
  :bind
  (("C-x f" . affe-find)
   ("C-c s" . affe-grep)))

(use-package consult-ls-git
  :after (consult)
  :bind
  (("C-c F" . #'consult-ls-git-other-window)))

(use-package marginalia
  :after (vertico consult)
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package tempbuf
  :hook ((dired-mode-hook magit-mode-hook). turn-on-tempbuf-mode))

(use-package migemo
  :when (executable-find "cmigemo")
  :config
  (setq migemo-command "cmigemo")
  (setq migemo-dictionary "/usr/share/migemo/utf-8/migemo-dict")
  (setq migemo-options '("-q" "--emacs" "-i" "\a"))
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix))

(use-package ace-window
  :bind (("C-t" . ace-window)
         :map dired-mode-map
         ("C-t" . ace-window))
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-background nil)
  (set-face-attribute  'aw-leading-char-face nil :height 10.0)
  (add-to-list 'aw-ignored-buffers 'lsp-ui-imenu-mode))

(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
	 ("M-g j" . dumb-jump-go)
	 ("M-g i" . dumb-jump-go-prompt)
	 ("M-g x" . dumb-jump-go-prefer-external)
	 ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config
  (setq dumb-jump-selector 'ivy))

(use-package skk
  :straight ddskk
  :bind (("C-x C-j" . skk-mode))
  :hook ((fundamental-mode prog-mode text-mode) .
	 (lambda () (skk-mode 1) (skk-latin-mode 1)))
  :init
  (setq skk-egg-like-newline t)
  (setq skk-use-color-cursor t)
  (setq skk-use-azik t)
  (setq skk-azik-keyboard-type 'jp106)
  (setq-default skk-kutouten-type 'en)
  :config
  (setq skk-large-jisyo (locate-user-emacs-file (convert-standard-filename "locals/dict/SKK-JISYO.L")))
  (unless (file-exists-p skk-large-jisyo)
    (skk-get (locate-user-emacs-file (convert-standard-filename "locals/dict/")))))

(use-package treemacs
  :ensure t
  :defer t
  :bind (("C-c t" . treemacs))
  :config
  (treemacs-follow-mode t))

(use-package treemacs-magit
  :disabled t
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-tab-bar
  :after (treemacs)
  :ensure t
  :config
  (treemacs-set-scope-type 'Tabs))

(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :custom ((flycheck-display-errors-delay  0.3 "表示のdelayを入れる")))

(use-package yasnippet
  :hook (after-init . yas-global-mode)
  :bind (:map yas-minor-mode-map
	      ("C-x i i" . yas-insert-snippet)
	      ("C-c y" . yas-insert-snippet)
	      ("C-x i n" . yas-new-snippet)
	      ("C-x i v" . yas-visit-snippet-file)
	      ("C-x i l" . yas-describe-tables)
	      ("C-x i g" . yas-reload-all))
  :init
  (setq yas-snippet-dirs
	`(,(locate-user-emacs-file (convert-standard-filename "conf/snippets")))))

(use-package yasnippet-snippets
  :after (yasnippet))

(use-package yatemplate
  :hook (after-init . yatemplate-fill-alist)
  :config
  (setq yatemplate-dir (locate-user-emacs-file (convert-standard-filename "conf/templates")))
  (setq auto-insert-query nil)
  (auto-insert-mode))

(use-package org
  :after (all-the-icons)
  :bind (("C-c a" . org-agenda)
	 ("C-c c" . org-capture))
  :config
  (require 'org-tempo)
  (setq org-directory "~/org")
  (setq org-default-notes-file "notes.org")
  (setq org-agenda-files (list org-directory))
  (setq org-refile-targets '((org-agenda-files :maxlevel . 1)))
  (setq org-enforce-todo-dependencies t)
  (setq org-log-done 'time)
  (setq org-todo-keywords
	'((sequence "TODO(t)" "DOING(i)" "WAITING(w)" "|" "DONE(d)" "SOMEDAY(s)" "CANCELED(c)")))

  (setq org-todo-keyword-faces
        '(("TODO" . org-todo)
          ("WAITING" . "dim gray")
          ("DOING" . "red")))

  (setq org-capture-templates
	`(("t" ,(concat (all-the-icons-octicon "checklist" :face 'all-the-icons-blue) " Task")
	   entry (file+headline ,(expand-file-name (concat org-directory "/inbox.org")) "inbox")
	   "* TODO %?\nCREATED: %T")
	  ("n" ,(concat (all-the-icons-octicon "book" :face 'all-the-icons-blue) " Note")
	   entry (file ,(expand-file-name (concat org-directory "/notes.org")))
	   "* %?\n   %a\n    %T")))
  (setq org-confirm-babel-evaluate nil))

(use-package org-bullets
  :custom (org-bullets-bullet-list '("" "" "" "" ""))
  :hook (org-mode . org-bullets-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred lsp-rename)
  :hook ((go-mode web-mode). lsp-deferred)
  :bind (("C-c r" . #'lsp-rename))
  :custom
  ((lsp-keymap-prefix "C-c k")
   (lsp-print-performance t)
   (lsp-prefer-capf t "capf(completion-at-point) companyを使用する")
   (lsp-diagnostics-provider :flycheck)
   (lsp-response-timeout 5)
   (lsp-idle-delay 0.5)
   (lsp-enable-file-watchers nil)
   (lsp-enable-completion-at-point t)
   (lsp-eslint-run "onSave")
   (lsp-eslint-options '((cache . t)))
   (lsp-modeline-diagnostics-enable t)

   (lsp-log-io t)

   (lsp-javascript-format-enable nil)
   (lsp-javascript-preferences-import-module-specifier "non-relative")
   (lsp-typescript-format-enable t)
   (lsp-typescript-validate-enable nil)
   (lsp-typescript-preferences-import-module-specifier "non-relative"))

  :init
  (setq lsp-session-file
        (locate-user-emacs-file (convert-standard-filename "locals/.lsp-session-v1")))
  :config
  (defun my-lsp-mode-hook ()
    (lsp-enable-which-key-integration))

  (add-hook 'lsp-mode-hook #'my-lsp-mode-hook))

(use-package lsp-ui
  :after (lsp-mode)
  :hook   (lsp-mode . lsp-ui-mode)
  :bind (([remap xref-find-definitions] . #'lsp-ui-peek-find-definitions) ; M-.
	 ([remap xref-find-references] . #'lsp-ui-peek-find-references) ; M-?
         ("C-c m" . #'lsp-ui-imenu))
  :custom
  ((lsp-ui-flycheck t)

   (lsp-ui-imenu-auto-refresh t)
   (lsp-ui-imenu-kind-position 'top)
   (lsp-ui-imenu-refresh-delay 0.5)

   (lsp-ui-peek-enable t)
   (lsp-ui-peek-always-show t)
   (lsp-ui-peek-peek-height 30)
   (lsp-ui-peek-list-width 30)

   (lsp-ui-sideline-enable t)
   (lsp-ui-sideline-show-diagnostics t)
   (lsp-ui-sideline-show-code-actions t)

   (lsp-ui-doc-enable t)
   (lsp-ui-doc-show-with-cursor t)
   (lsp-ui-doc-show-with-mouse nil)
   (lsp-ui-doc-position 'at-point)))

(use-package lsp-treemacs
  :after (lsp-mode treemacs)
  :config
  (lsp-treemacs-sync-mode 1))

(use-package yaml-mode)

(use-package scheme
  :mode (("\\.scm\\'" . scheme-mode))
  :interpreter "gosh"
  :bind (:map scheme-mode-map
	      ("C-c C-c" . scheme-other-window))
  :config
  (defun my-scheme-mode-hook ()
    (add-hook 'before-save-hook 'delete-trailing-whitespace nil 'local)
    (add-hook 'before-save-hook #'(lambda () (indent-region (point-min) (point-max))) nil 'local))
  (add-hook 'scheme-mode-hook #'my-racket-mode-hook)
  (setq scheme-program-name "gosh -i")
  (use-package cmuscheme)
  (defun scheme-other-window ()
    "Run Gauche on other window"
    (interactive)
    (split-window-horizontally (/ (frame-width) 2))
    (let ((buf-name (buffer-name (current-buffer))))
      (scheme-mode)
      (switch-to-buffer-other-window
       (get-buffer-create "*scheme*"))
      (run-scheme scheme-program-name)
      (switch-to-buffer-other-window
       (get-buffer-create buf-name)))))

(use-package go-mode
  :after (company)
  :config
  (setq gofmt-command "goimports")

  (setq lsp-go-language-server-flags
	'("-gocodecompletion"
	  "-diagnostics"
	  "-lint-tool=golint"))

  (defun my-go-mode-hook ()
    (setq c-basic-offset 4)
    (setq tab-width 4)

    (add-hook 'before-save-hook #'lsp-organize-imports t t)
    (add-hook 'before-save-hook #'gofmt-before-save t t))

  (add-hook 'go-mode-hook #'my-go-mode-hook))

(use-package adoc-mode
  :mode "\\.adoc\\'"
  :config
  (set-face-attribute 'markup-attribute-face nil :foreground "gray60" :weight 'bold)
  (set-face-attribute 'markup-command-face nil :foreground "gray60" :weight 'bold)
  (set-face-attribute 'markup-value-face nil :foreground "gray60" :weight 'bold)
  (set-face-attribute 'markup-meta-face nil :foreground "gray60" :weight 'bold)
  (set-face-attribute 'markup-title-0-face nil :height 2.0)
  (set-face-attribute 'markup-title-1-face nil :height 1.5)
  (set-face-attribute 'markup-title-2-face nil :height 1.2)
  (set-face-attribute 'markup-title-3-face nil :height 1.0)
  (set-face-attribute 'markup-title-4-face nil :height 1.0)

  (set-face-attribute 'markup-meta-hide-face nil :foreground "gray90" :weight 'bold)
  (set-face-attribute 'markup-list-face nil :background 'unspecified :weight 'bold))

(use-package modus-themes
  :straight (modus-themes :type git :host github :repo "protesilaos/modus-themes" :branch "main")
  :init
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        modus-themes-region '(bg-only no-extend))
  :config
  (load-theme 'modus-vivendi :no-confirm))

(show-paren-mode 1)
(setq show-paren-delay 0.1)
(setq show-paren-style 'expression)
(setq show-paren-style 'expression)
(set-face-attribute 'show-paren-match-expression nil
                    :background nil :foreground nil
                    :underline "#ff0000")

(use-package json-mode
  :mode "\\.json\\'")

(use-package tuareg)

(use-package merlin
  :after tuareg
  :hook (tuareg-mode . merlin-mode))

(use-package gist
  :init
  (setq pcache-directory
	(locate-user-emacs-file (convert-standard-filename "locals/pcache/")))
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

(use-package php-mode
  :mode "\\.php\\'")

(use-package docker)

(use-package dockerfile-mode)

(use-package web-mode
  :mode "\\.tsx?\\'"
  :init
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-auto-indentation nil)
  (setq web-mode-enable-auto-quoting nil)

  (defun my-web-mode-hook ()
    (setq c-basic-offset 2)
    (add-hook 'XXX-mode-hook #'lsp-deferred))

  (add-hook 'web-mode-hook #'my-web-mode-hook))

(use-package request)

(use-package graphql-mode
  :after (request)
  :mode "\\.graphqls?\\'"
  :init
  (defun my-graphql-mode-hook ()
    (setq comment-start "\"\"\"")
    (setq comment-end "\"\"\""))

  (add-hook 'graphql-mode-hook #'my-graphql-mode-hook))

(use-package ox-reveal
  :ensure t)

(use-package prettier
  :hook (after-init . global-prettier-mode))
