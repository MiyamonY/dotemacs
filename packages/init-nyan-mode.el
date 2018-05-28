(use-package nyan-mode
  :init
  (progn
    (setq nyan-bar-length 10
          nyan-wavy-trail nil))
  :config
  (progn
    (nyan-stop-animation)))

(nyan-mode)
