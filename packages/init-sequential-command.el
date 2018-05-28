;;; init-sequential-command.el --- initalize sequential-command  -*- lexical-binding: t; -*-

;; Copyright (C) 2018

;; Author:  <ymiyamoto@ymiyamoto-desktop>
;; Keywords:
(use-package sequential-command-config
  :bind
  (("C-/" . seq-undo)
   :map undo-tree-map
   ("C-/" . seq-undo))
  :config
  (progn
    (define-sequential-command seq-undo
      undo-tree-undo undo-tree-visualize)))

(sequential-command-setup-keys)
