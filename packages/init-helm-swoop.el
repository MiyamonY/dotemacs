(use-package helm-swoop
  :bind
  (("C-s" . helm-swoop)
   ("C-r" . helm-swoop-back-to-last-point)
   :map helm-swoop-map
   (("C-r" . helm-previous-line)
    ("C-s" . helm-next-line)))
  :config
  (progn
    (setq helm-swoop-use-line-number-face t)
    (setq helm-swoop-split-window-function 'helm-default-display-buffer)))
