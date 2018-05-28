(use-package ace-jump-mode
  :init
  (progn
    (setq ace-jump-mode-move-keys
          (append "asdfghjkl;:]qwertyuiop@zxcvbnm,." nil))
    (setq ace-jump-word-mode-use-query-char nil)))
