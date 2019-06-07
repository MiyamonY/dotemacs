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

(straight-use-package 'use-package)

(use-package util
  :load-path "./lisp")

(use-package bind-key
  :bind (("C-h" . delete-backward-char)
	 ("C-x j" . split-window-horizontally)
	 ("C-x u" . split-window-vertically)
	 ("C-c i" .
	  (lambda ()
	    (interactive)
	    (find-file (locate-user-emacs-file "init.el"))))))

(use-package dracula-theme
  :straight t)

(use-package magit
  :straight t
  :commands (magit-status)
  :bind (("C-c g" . magit-status)
	 ("C-c C-g" . magit-status))
  :init
  (setq transient-history-file
	(locate-user-emacs-file (convert-standard-filename "locals/transient/history.el"))))

(use-package git-gutter+
  :straight t
  :hook (prog-mode . git-gutter+-mode)
  :config
  (setq git-gutter+-separator-sign "|")
  (set-face-foreground 'git-gutter+-separator "white"))

(use-package rainbow-delimiters
  :straight t
  :commands (rainbow-delimiters-mode)
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package hl-line+
  :straight t
  :init
  (setq hl-line-idle-interval 0.5)
  :config
  (toggle-hl-line-when-idle)
  (set-face-background 'hl-line "gray30"))

(use-package recentf
  :init
  (setq recentf-save-file
	(locate-user-emacs-file (convert-standard-filename "locals/recentf")))
  (setq recentf-max-saved-items 500)
  (setq recentf-max-menu-items 15)
  (setq recentf-auto-cleanup 'never))

(use-package company
  :straight t
  :hook (prog-mode . company-mode)
  :bind (:map company-active-map
	      ("M-n" . nil)
	      ("M-p" . nil)
	      ("C-n" . company-select-next)
	      ("C-p" . company-select-previous)
	      ("C-h" . nil))
  :config
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefx-length 2)
  (setq company-selection-wrap-around t))

(use-package counsel
  :straight t
  :bind (("C-s" . swiper)
	 ("M-x" . counsel-M-x)
	 ("C-c C-r" . ivy-resume)
	 ("C-x C-f" . counsel-find-file)
	 ("<f1> f" . counsel-describe-function)
	 ("<f1> l" . counsel-find-library)
	 ("<f1> v" . counsel-describe-variable)
	 ("C-x l" . counsel-locate))
  :init
  (setq ivy-height 30)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  :config
  (ivy-mode 1)
  (use-package all-the-icons-ivy
    :straight t
    :config
    (setq inhibit-compacting-font-caches t)
    (all-the-icons-ivy-setup)))

(use-package migemo
  :straight t
  :when (executable-find "cmigemo")
  :config
  (setq migemo-command "cmigemo")
  (setq migemo-dictionary "/usr/share/migemo/utf-8/migemo-dict")
  (setq migemo-options '("-q" "--emacs" "-i" "\a"))
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix))

(use-package avy
  :straight t
  :config
  (use-package avy-migemo
    :straight t
    :init
    (avy-migemo-mode 1)
    :config
    (use-package avy-migemo-e.g.swiper
      :disabled)
    (setq avy-timeout-seconds 0.1)))

(use-package ace-window
  :straight t
  :bind (("C-t" . ace-window))
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-background nil)
  (set-face-attribute  'aw-leading-char-face nil :height 10.0))

(use-package format-all
  :straight t
  :config
  (format-all-mode))
