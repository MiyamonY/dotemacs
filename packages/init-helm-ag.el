;;; init-helm-ag.el --- initialize helm-ag           -*- lexical-binding: t; -*-

;; Copyright (C) 2018

;; Author:  <ymiyamoto@ymiyamoto-pc>
;; Keywords: lisp,
(use-package helm-ag
  :bind
  (("C-c s" . helm-ag-project-root)))
