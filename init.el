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

