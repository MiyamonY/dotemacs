;;; init-smart-mode-line.el ---                      -*- lexical-binding: t; -*-

;; Copyright (C) 2018

;; Author:  <ymiyamoto@ymiyamoto-desktop>
;;;; Keywords:
(use-package smart-mode-line
  :init
  (progn
    (setq sml/no-confirm-load-theme t)
    (setq sml/theme 'dark)))

(sml/setup)
