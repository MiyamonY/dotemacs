;;; init-ac-helm.el --- initialize ac-helm           -*- lexical-binding: t; -*-

;; Copyright (C) 2018

;; Author:  <ymiyamoto@ymiyamoto-pc>
;;;; Keywords: lisp
(use-package ac-helm
  :bind
  (("C-:" . ac-complete-with-helm)
   :map ac-menu-map
        ("C-n" . ac-next)
        ("C-p" . ac-previous)
        ("C-:" . ac-complete-with-helm)))
