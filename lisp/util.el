(setq auto-save-list-file-prefix nil)
(defalias 'yes-or-no-p 'y-or-n-p)

(tool-bar-mode -1)
(scroll-bar-mode -1)

(show-paren-mode 1)
(setq show-paren-dilay 0.2)
(setq show-paren-style 'expression)
(set-face-attribute 'show-paren-match nil
		    :background 'unspecified :foreground 'unspecified
		    :underline "#bd93f9")

(setq default-frame-alist
      '((width . 120)
	(height . 40)
	(top . 0)
	(left . 0)
	(font . "-PfEd-Ricty Diminished Discord-normal-normal-normal-*-*-*-*-*-*-0-iso10646-1")))

(setq-default show-trailing-whitespace t)
(set-face-attribute 'trailing-whitespace nil
		    :foreground 'unspecified
		    :background 'unspecified
		    :underline "red")
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(provide 'util)
