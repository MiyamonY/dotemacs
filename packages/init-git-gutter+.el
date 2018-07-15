(use-package git-gutter+
  :config
  (progn
    (global-git-gutter+-mode t)
    (run-with-idle-timer 1 t 'git-gutter+-refresh)))
