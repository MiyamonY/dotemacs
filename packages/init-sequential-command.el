;;; init-sequential-command.el --- initalize sequential-command  -*- lexical-binding: t; -*-

;; Copyright (C) 2018

;; Author:  <ymiyamoto@ymiyamoto-desktop>
;; Keywords:
(use-package sequential-command-config
  :commands
  (sequential-command-setup-keys)
  :bind
  (("C-/" . seq-undo)
   :map undo-tree-map
   ("C-/" . seq-undo))
  :config
  (progn
    (defun my-undo-tree-undo ()
      (interactive)
      (condition-case err
          (undo-tree-undo)
        (undo-tree-mode 1)))
    (defun my-undo-tree-visualize ()
      (interactive)
      (condition-case err
          (undo-tree-visualize)
        (undo-tree-mode 1)))
    (define-sequential-command seq-undo
      my-undo-tree-undo my-undo-tree-visualize)))

(sequential-command-setup-keys)
