(setq jit-lock-defer-time 0.05)
(setq-default bidi-display-reordering nil)

(setq default-frame-alist
      '((width . 120)
	(height . 40)
	(vertical-scroll-bars . nil)
	(top . 0)
	(left . 0)
	(font . "-PfEd-Ricty Diminished Discord-normal-normal-normal-*-*-*-*-*-*-0-iso10646-1")))

(setq auto-save-list-file-prefix nil)
(defalias 'yes-or-no-p 'y-or-n-p)

(tool-bar-mode -1)

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

(provide 'util)
