;;; init-hilight-symbol.el ---                       -*- lexical-binding: t; -*-

;; Copyright (C) 2018

;; Author:  <ymiyamoto@ymiyamoto-pc>
;; Keywords:
(use-package highlight-symbol
  :hook
  ((prog-mode-hook . highlight-symbol-mode))
  :config
  (progn
    (setq highlight-symbol-idle-delay 0.2)))
