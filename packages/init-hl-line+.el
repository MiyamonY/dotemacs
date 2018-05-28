;;; init-hl-line.el --- init-hl-line                 -*- lexical-binding: t; -*-

;; Copyright (C) 2018

;; Author:  <ymiyamoto@ymiyamoto-desktop>
;; Keywords:
(use-package hl-line+
  :init
  (progn
    (global-hl-line-mode -1))
  :config
  (hl-line-when-idle-interval 0.1))

(toggle-hl-line-when-idle)
