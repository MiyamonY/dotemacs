(use-package helm-swoop
  :bind
  (:map helm-swoop-map
        (("C-r" . helm-previous-line)
         ("C-s" . helm-next-line)))
  :init
  (progn
    (setq helm-swoop-use-line-number-face t)
    (setq helm-swoop-split-window-function 'helm-default-display-buffer)))
