(setq jit-lock-defer-time 0.05)
(setq-default bidi-display-reordering nil)

(let* ((font-name "Ricty Diminished Discord")
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

;; ediff
(set-face-attribute 'ediff-even-diff-A nil :background "gray20")
(set-face-attribute 'ediff-odd-diff-A nil :background "gray30")
(set-face-attribute 'ediff-even-diff-B nil :background "gray20")
(set-face-attribute 'ediff-odd-diff-B nil :background "gray30")
(setq ediff-split-window-function 'split-window-horizontally)

;;  tab
(tab-bar-mode)
(tab-bar-history-mode +1)

(bind-key "C-; p" #'tab-previous)
(bind-key "C-; n" #'tab-next)
(bind-key "C-; c" #'tab-new)

(provide 'util)
