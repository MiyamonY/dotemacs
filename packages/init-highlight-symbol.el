;;; init-hilight-symbol.el ---                       -*- lexical-binding: t; -*-

;; Copyright (C) 2018

;; Author:  <ymiyamoto@ymiyamoto-pc>
;; Keywords:
(use-package highlight-symbol
  :hook
  ((prog-mode . highlight-symbol-mode))
  :config
  (progn
    (set-face-attribute 'highlight-symbol-face nil :background "dark orange")
    (setq highlight-symbol-idle-delay 0.2)))
