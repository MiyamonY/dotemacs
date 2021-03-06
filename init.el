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

(use-package util
  :load-path "lisp")

(setq straight-use-package-by-default t)

(use-package dashboard
  :bind (:map dashboard-mode-map
	      ("f" . counsel-recentf)
	      ("b" . counsel-bookmark)
	      ("a" . org-agenda)
	      ("g" . counsel-ghq))
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
  (setq magit-diff-refine-ignore-whitespace t))

(use-package magit-gitflow
  :after (magit)
  :hook (magit-mode . turn-on-magit-gitflow))

(use-package git-gutter+
  :hook (after-init . global-git-gutter+-mode)
  :config
  (use-package git-gutter-fringe+))

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
  (toggle-hl-line-when-idle)
  (set-face-background 'hl-line "gray30"))

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

(show-paren-mode 1)
(setq show-paren-delay 0.1)
(setq show-paren-style 'expression)
(set-face-attribute 'show-paren-match nil
		    :background 'unspecified :foreground 'unspecified
		    :underline "white")

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
  (setq company-idle-delay 0.4)
  (setq company-minimum-prefx-length 2)
  (setq company-selection-wrap-around t))

(use-package company-box
  :hook (company-mode . company-box-mode)
  :custom ((company-box-icons-alist 'company-box-icons-all-the-icons)))

(use-package counsel
  :hook ((after-init . ivy-mode)
	 (ivy-mode . counsel-mode))
  :bind (("C-s" . swiper)
	 ("M-x" . counsel-M-x)
	 ("C-c C-r" . ivy-resume)
	 ("C-x C-b" . ivy-switch-buffer)
	 ("C-x C-f" . counsel-find-file)
	 ("<f1> f" . counsel-describe-function)
	 ("<f1> l" . counsel-find-library)
	 ("<f1> v" . counsel-describe-variable)
	 ("C-c s" . counsel-rg)
	 ("M-y" . counsel-yank-pop)
	 ("C-x l" . counsel-locate)
	 (:map ivy-minibuffer-map
	       ("C-m" . ivy-alt-done)))
  :init
  (setq ivy-height 30)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  :config
  (dolist (pair `((counsel-evil-registers . ,ivy-height)
		  (counsel-yank-pop . ,ivy-height)
		  (counsel-git-log . ,ivy-height)
		  (counsel--generic . ,ivy-height)
		  (counsel-el . ,ivy-height)))
    (add-to-list 'ivy-height-alist pair))
  (use-package counsel-ghq
    :straight (consel-ghq :type git :host github :repo "windymelt/counsel-ghq") ; not found in melpa
    :bind (("C-c g" . counsel-ghq))
    :config
    (defalias 'counsel-symbol-at-point 'ivy-thing-at-point)))

(use-package counsel-projectile
  :after (counsel)
  :bind (("C-c f" . counsel-projectile-find-file)))

(use-package ivy-rich
  :after (ivy all-the-icons)
  :config
  (setq ivy-format-function #'ivy-format-function-line)
  (defun ivy-rich-switch-buffer-icon (candidate)
    (with-current-buffer
	(get-buffer candidate)
      (let ((icon (all-the-icons-icon-for-mode major-mode)))
	(if (symbolp icon)
	    (all-the-icons-icon-for-mode 'fundamental-mode)
	  icon))))

  (if (display-graphic-p)
      (progn
	(setq my-icon-hash (make-hash-table))
	(defun ivy-rich-file-icon (candidate)
	  (when (equal (gethash candidate my-icon-hash nil) nil)
	    (let ((icon
		   (if (file-directory-p candidate)
		       (cond
			((and (fboundp 'tramp-tramp-file-p)
			      (tramp-tramp-file-p default-directory))
			 (all-the-icons-octicon "file-directory"))
			((file-symlink-p candidate)
			 (all-the-icons-octicon "file-symlink-directory"))
			((all-the-icons-dir-is-submodule candidate)
			 (all-the-icons-octicon "file-submodule"))
			((file-exists-p (format "%s/.git" candidate))
			 (all-the-icons-octicon "repo"))
			(t
			 (let ((matcher
				(all-the-icons-match-to-alist candidate all-the-icons-dir-icon-alist)))
			   (apply (car matcher) (list (cadr matcher))))))

		     (all-the-icons-icon-for-file candidate))))
	      (puthash candidate icon my-icon-hash)))
	  (gethash candidate my-icon-hash))
	)
    (defun ivy-rich-file-icon (candidate) ""))

  (setq ivy-rich-display-transformers-list
	(plist-put ivy-rich-display-transformers-list
		   'ivy-switch-buffer
		   '(:columns
		     ((ivy-rich-switch-buffer-icon (:width 2))
		      (ivy-rich-candidate (:width 30))
		      (ivy-rich-switch-buffer-size (:width 7))
		      (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
		      (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
		      (ivy-rich-switch-buffer-project (:width 15 :face success))
		      (ivy-rich-switch-buffer-path
		       (:width
			(lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
		     :predicate
		     (lambda (cand) (get-buffer cand)))))
  (setq ivy-rich-display-transformers-list
	(plist-put ivy-rich-display-transformers-list
		   'counsel-find-file
		   '(:columns
		     ((ivy-rich-file-icon (:width 2))
		      (ivy-rich-candidate)
		      (ivy-rich-counsel-find-file-truename (:face font-lock-doc-face))))))
  (ivy-rich-mode 1))

(use-package ivy-posframe
  :after ivy
  :hook (ivy-mode . ivy-posframe-mode)
  :config
  (setq ivy-posframe-display-functions-alist
	'((t . ivy-posframe-display-at-frame-center))))

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

(use-package avy
  :config
  (use-package avy-migemo
    :init
    (avy-migemo-mode 1)
    :config
    (use-package avy-migemo-e.g.swiper
      :disabled)
    (setq avy-timeout-seconds 0.1)))

(use-package ace-window
  :bind (("C-t" . ace-window))
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-background nil)
  (set-face-attribute  'aw-leading-char-face nil :height 10.0))

(use-package format-all
  :hook (prog-mode . format-all-mode))

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

(use-package neotree
  :bind (("C-c t" . neotree-toggle)
	 (:map neotree-mode-map
	       ("d" . neotree-delete-node)))
  :init
  (setq neo-hidden-regexp-list '("\\.pyc$" "~$" "^#.*#$" "\\.elc$"))
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

(use-package flycheck
  :hook (prog-mode . global-flycheck-mode)
  :init
  (setq flycheck-check-syntax-automatically '(save mode-enabled)))

(use-package flycheck-posframe
  :after (flycheck)
  :hook (flycheck-mode . flycheck-posframe-mode))

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
	`(,(locate-user-emacs-file (convert-standard-filename "conf/snippets"))))
  :config
  (use-package ivy-yasnippet
    :after (ivy)))

(use-package yasnippet-snippets
  :after (yasnippet))

(use-package yatemplate
  :hook (after-init . yatemplate-fill-alist)
  :config
  (setq yatemplate-dir (locate-user-emacs-file (convert-standard-filename "conf/templates")))
  (setq auto-insert-query nil)
  (auto-insert-mode))

(use-package git-auto-commit-mode
  :config
  (setq-default gac-automatically-push-p t))

(use-package org
  :after (all-the-icons)
  :bind (("C-c a" . org-agenda)
	 ("C-c c" . org-capture))
  :config
  (require 'org-tempo)
  (setq org-directory "~/src/github.com/MiyamonY/memo/")
  (setq org-default-notes-file "notes.org")
  (setq org-agenda-files `(,(expand-file-name (concat org-directory "/task.org"))))
  (setq org-enforce-todo-dependencies t)
  (setq org-log-done 'time)
  (setq org-todo-keywords
	'((sequence "TODO(t)" "WAITING(w)" "PENDING(p)" "|" "DONE(d)" "CANCELED(c)")))

  (setq org-capture-templates
	`(("t" ,(concat (all-the-icons-octicon "checklist" :face 'all-the-icons-blue) " Task")
	   entry (file ,(expand-file-name (concat org-directory "/task.org")))
	   "* TODO %?\n    %i\n    %T")
	  ("n" ,(concat (all-the-icons-octicon "book" :face 'all-the-icons-blue) " Note")
	   entry (file ,(expand-file-name (concat org-directory "/notes.org")))
	   "* %?\n   %a\n    %T")))
  (setq org-confirm-babel-evaluate nil))

(use-package org-pomodoro
  :after (org org-agenda all-the-icons)
  :bind (:map org-agenda-mode-map
	      ("p" . org-pomodoro))
  :hook
  (org-pomodoro-started .
			(lambda () (notifications-notify
				    :title "org-pomodoro"
				    :body "Let's focus for 25 minutes!")))
  (org-pomodoro-finished .
			 (lambda () (notifications-notify
				     :title "org-pomodoro"
				     :body "Well done! Take a break.")))
  (org-pomodoro-short-break-finished .
				     (lambda (notifications-notify
					      :title "org-pomdoro"
					      :body "Short break end. Start new pomodoro.")))
  :config
  (setq org-pomodoro-format (concat (all-the-icons-octicon "flame" :face 'all-the-icons-red) "%s"))
  (setq org-pomodoro-short-break-format (concat (all-the-icons-material "free_breakfast") "%s"))
  (setq org-pomodoro-long-break-format (concat (all-the-icons-material "free_breakfast") "%s"))
  (setq org-pomodoro-play-sounds nil))

(use-package org-bullets
  :custom (org-bullets-bullet-list '("" "" "" "" ""))
  :hook (org-mode . org-bullets-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (go-mode . lsp-deferred)
  :init
  (setq lsp-session-file
	(locate-user-emacs-file (convert-standard-filename "locals/.lsp-session-v1")))
  (setq lsp-keymap-prefix "s-l"))

(use-package lsp-ui
  :after (lsp-mode)
  :hook   (lsp-mode . lsp-ui-mode))

(use-package racket-mode
  :config
  (defun my-racket-mode-hook ()
    (add-hook 'before-save-hook 'delete-trailing-whitespace nil 'local)
    (add-hook 'before-save-hook #'(lambda () (indent-region (point-min) (point-max))) nil 'local))
  (add-hook 'racket-mode-hook #'my-racket-mode-hook))

(straight-use-package
 '(ob-racket :type git :host github :repo "wallyqs/ob-racket"))

(use-package ob-racket)

(use-package yaml-mode)

(use-package rustic)

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
    (set (make-local-variable 'company-backends) '(company-go))
    (setq indent-tabs-mode nil)
    (setq c-basic-offset 4)
    (setq tab-width 4)
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t)
    (add-hook 'before-save-hook #'gofmt-before-save))
  (add-hook 'go-mode-hook #'my-go-mode-hook))

(use-package company-lsp
  :commands company-lsp)

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

(use-package dracula-theme
  :config
  (set-face-attribute 'trailing-whitespace nil
		      :foreground 'unspecified
		      :background 'unspecified
		      :underline "red")
  (set-face-attribute 'show-paren-match nil
		      :background 'unspecified :foreground 'unspecified
		      :underline "#bd93f9"))

(use-package vue-mode)

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

(use-package emr
  :bind (("C-c ;" . iedit-mode)
	 :map prog-mode-map
	 ("M-RET" . emr-show-refactor-menu)))

(use-package elpy
  :after flycheck
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  :config
  (setq elpy-rpc-virtualenv-path 'current)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

(use-package scala-mode
  :interpreter
  ("scala" . scala-mode))

(use-package php-mode
  :mode "\\.php\\'")

(use-package docker)

(use-package dockerfile-mode)
