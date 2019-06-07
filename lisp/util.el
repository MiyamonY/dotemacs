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

(provide 'util)
