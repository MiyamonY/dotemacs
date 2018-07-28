(use-package git-gutter+
  :config
  (progn
    (global-git-gutter+-mode t)
    (defun my-git-gutter+-refresh ()
      (if (git-gutter+-in-git-repository-p (buffer-file-name))
          (git-gutter+-refresh)))
    (run-with-idle-timer 1 t 'my-git-gutter+-refresh)
    ))
