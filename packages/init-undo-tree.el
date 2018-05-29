;;; init-undo-tree.el --- initalize undo-tree        -*- lexical-binding: t; -*-

;; Copyright (C) 2018

;; Author:  <ymiyamoto@ymiyamoto-desktop>
;; Keywords: lisp,
(use-package undo-tree
  :bind
  (("C-." . undo-tree-redo)
   :map undo-tree-visualizer-mode-map
   ("C-m" . undo-tree-visualizer-quit)
   ("C-g" . undo-tree-visualizer-quit)))

(global-undo-tree-mode)
