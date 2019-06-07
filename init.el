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
  :hook (prog-mode))

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

(use-package sequential-command
  :straight t
  :config
  (use-package sequential-command-config
    :config
    (sequential-command-setup-keys)))

(use-package elscreen 
 :straight t
  :bind (("C-c n" . elscreen-next)
	 ("C-c p" . elscreen-previous)
	 ("C-c c" . elscreen-create))
  :config
  (setq elscreen-display-tab t)
  (setq elscreen-tab-display-kill-screen nil)
  (setq elscreen-tab-display-control nil)
  (let ((dracula-background "#282a36") (dracula-purple "#bd93f9")
	(dracula-foreground "#f8f8f2"))
    (set-face-attribute
     'elscreen-tab-current-screen-face nil :weight 'bold :foreground dracula-purple :background dracula-foreground)
    (set-face-attribute
     'elscreen-tab-other-screen-face nil :weight 'bold :foreground dracula-purple :background dracula-background))
  (elscreen-start))

(use-package smartparens
  :straight t
  :config
  (use-package smartparens-config)
  (smartparens-global-mode t))

(show-paren-mode 1)
(setq show-paren-delay 0.1)
(setq show-paren-style 'expression)
(set-face-attribute 'show-paren-match nil
		    :background 'unspecified :foreground 'unspecified
                    :underline "white")

(use-package racket-mode
  :straight t)
