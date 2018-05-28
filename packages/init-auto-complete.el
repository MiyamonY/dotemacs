;;; init-auto-complete.el --- initialize auto-complete  -*- lexical-binding: t; -*-

;; Copyright (C) 2018

;; Author:  <ymiyamoto@ymiyamoto-pc>
;; Keywords:
(use-package auto-complete
  :bind
  (:map ac-menu-map
        ("C-n" . ac-next)
        ("C-p" . ac-previous))
  :init
  (progn
    (setq ac-auto-show-menu 0.5)
    (setq ac-use-menu-map t))
  :config
  (progn
    (ac-set-trigger-key "TAB")))

(ac-config-default)
