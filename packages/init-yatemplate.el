(use-package autoinsert
  :init
  (progn
    (setq auto-insert-directory (locate-user-emacs-file "conf/insert/") ; テンプレートファイルの場所
          auto-insert-query nil) ; テンプレートを挿入するか聞かない
    ))
(auto-insert-mode 1)

(use-package yatemplate
  :init
  (progn
    (setq yatemplate-dir (locate-user-emacs-file "conf/insert"))
    (yatemplate-fill-alist)))
