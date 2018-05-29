;;; init-sequential-command.el --- initalize sequential-command  -*- lexical-binding: t; -*-

;; Copyright (C) 2018

;; Author:  <ymiyamoto@ymiyamoto-desktop>
;; Keywords:
(use-package sequential-command-config
  :config
  (progn
    (define-sequential-command seq-undo undo-tree-undo undo-tree-visualize)
    (define-sequential-command seq-home
      beginning-of-line beginning-of-buffer seq-return)
    (define-sequential-command seq-end
      end-of-line end-of-buffer seq-return)))

(sequential-command-setup-keys)
