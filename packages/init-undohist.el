;;; init-undohist.el --- initalize undohist          -*- lexical-binding: t; -*-

;; Copyright (C) 2018

;; Author:  <ymiyamoto@ymiyamoto-desktop>
;; Keywords:
(use-package undohist
  :config
  (progn
    (setq undohist-ignored-files '("COMMIT_EDITMSG")))
  :init
  (progn
    (setq undohist-directory (concat init-emacs-local-files "/undohist/undohist"))))

(undohist-initialize)
