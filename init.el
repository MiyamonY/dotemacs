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
  :load-path "./lisp")

(setq straight-use-package-by-default t)

(use-package bind-key
  :bind (("C-h" . delete-backward-char)
	 ("C-x j" . split-window-horizontally)
	 ("C-x u" . split-window-vertically)
	 ("C-c d f" . describe-function)
	 ("C-c d v" . describe-variable)
	 ("C-c d m" . describe-mode)
	 ("C-c i" .
	  (lambda ()
	    (interactive)
	    (find-file (locate-user-emacs-file "init.el"))))))

(use-package which-key
  :hook (after-init . which-key-mode))

(use-package dracula-theme)

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-height 20)
  (setq doom-modeline-icon t)
  (setq doom-modeline-major-mode-icon t)
  (setq inhibit-compacting-font-caches t)
  (setq find-file-visit-truename t)
  (use-package nyan-mode
    :after (doom-modeline)
    :config
    (nyan-mode)))

(use-package magit
  :commands (magit-status)
  :bind (("C-c g" . magit-status))
  :init
  (setq transient-history-file
	(locate-user-emacs-file (convert-standard-filename "locals/transient/history.el"))))

(use-package git-gutter+
  :hook (prog-mode . git-gutter+-mode)
  :init
  (setq git-gutter+-hide-gutter nil)
  :config
  (setq git-gutter+-separator-sign "|")
  (set-face-foreground 'git-gutter+-separator "white"))

(use-package rainbow-delimiters
  :commands (rainbow-delimiters-mode)
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package hl-line+
  :init
  (setq hl-line-idle-interval 0.5)
  :config
  (toggle-hl-line-when-idle)
  (set-face-background 'hl-line "gray30"))

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
  (setq undo-tree-mode-lighter ""))

(use-package sequential-command-config
  :straight sequential-command
  :hook (after-init . sequential-command-setup-keys))

(use-package elscreen
  :bind (("C-; n" . elscreen-next)
	 ("C-; p" . elscreen-previous)
	 ("C-; c" . elscreen-create)
	 ("C-; k" . elscreen-kill))
  :config
  (setq elscreen-display-tab t)
  (setq elscreen-tab-display-kill-screen nil)
  (setq elscreen-tab-display-control nil)
  (let ((dracula-background "#282a36") (dracula-purple "#bd93f9")
	(dracula-foreground "#f8f8f2"))
    (set-face-attribute
     'elscreen-tab-current-screen-face nil :weight 'bold :foreground dracula-purple :background dracula-background)
    (set-face-attribute
     'elscreen-tab-other-screen-face nil :weight 'bold :foreground dracula-purple :background dracula-foreground))
  (elscreen-start))

(use-package smartparens-config
  :straight smartparens
  :hook (after-init . smartparens-global-mode))

(show-paren-mode 1)
(setq show-paren-delay 0.1)
(setq show-paren-style 'expression)
(set-face-attribute 'show-paren-match nil
		    :background 'unspecified :foreground 'unspecified
                    :underline "white")

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
	 ("C-x C-f" . counsel-find-file)
	 ("<f1> f" . counsel-describe-function)
	 ("<f1> l" . counsel-find-library)
	 ("<f1> v" . counsel-describe-variable)
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
    :straight (el-patch :type git :host github :repo "windymelt/counsel-ghq") ; not found in melpa
    :bind (("C-x g" . counsel-ghq))))

(use-package ivy-rich
  :after (ivy)
  :config
  (setq ivy-format-function #'ivy-format-function-line)
  (defun ivy-rich-switch-buffer-icon (candidate)
    (with-current-buffer
  	(get-buffer candidate)
      (let ((icon (all-the-icons-icon-for-mode major-mode)))
  	(if (symbolp icon)
  	    (all-the-icons-icon-for-mode 'fundamental-mode)
  	  icon))))
  (defun ivy-rich-file-icon (candidate)
    (when (display-graphic-p)
      (let ((icon (if (file-directory-p candidate)
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
		       (t (let ((matcher (all-the-icons-match-to-alist candidate all-the-icons-dir-icon-alist)))
			    (apply (car matcher) (list (cadr matcher))))))
		    (all-the-icons-icon-for-file candidate))))
	icon)))
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
		      (ivy-read-file-transformer (:width 10))
		      (ivy-rich-counsel-find-file-truename (:face font-lock-doc-face))))))
  (use-package all-the-icons)
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
  :bind (("C-j" . skk-mode))
  :init
  (setq skk-egg-like-newline t)
  (setq skk-use-color-cursor t)
  (setq skk-use-azik t)
  (setq skk-azik-keyboard-type 'jp106))

(use-package neotree
  :bind (("C-c t" . neotree-toggle))
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
  (setq yas-snippet-dirs '("~/.emacs.d/snippets")))

(use-package yasnippet-snippets
  :after (yasnippet))

(use-package racket-mode)

(use-package rustic)
