;;; init-yasnippet.el ---                            -*- lexical-binding: t; -*-

;; Copyright (C) 2018

;; Author:  <ymiyamoto@ymiyamoto-pc>
;; Keywords:
(use-package yasnippet
  :bind
  (:map yas-minor-mode-map
        ("C-x i n" . yas-new-snippet)
        ("C-x i i" . yas-insert-snippet)
        ("C-x i v" . yas-visit-snippet-file)
        ("TAB" . yas-next-field)
        ("<tab>" . nil)
        ("C-i" . nil))
  :init
  (setq yas-snippet-dirs (locate-user-emacs-file "conf/insert/")))

(yas-global-mode 1)
