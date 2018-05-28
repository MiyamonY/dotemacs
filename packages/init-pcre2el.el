;;; init-pcre2el.el --- initialize pcre2el           -*- lexical-binding: t; -*-

;; Copyright (C) 2018

;; Author:  <ymiyamoto@ymiyamoto-pc>
;; Keywords: lisp,
(use-package pcre2el
  :hook
  ((prog-mode-hook . rxt-mode))
  :init
  (setq reb-re-syntax 'pcre))
