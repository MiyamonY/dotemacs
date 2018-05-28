(use-package skk
  :init
  (progn
    (setq skk-user-directory
          (file-name-as-directory
           (concat (file-name-as-directory init-emacs-local-files) ".ddskk")))
    (setq default-input-method "japanese-skk")
    (setq skk-show-icon t))
  :config
  (progn
    (defun init-check-jisyo-file ()
      (unless (file-exists-p
	       (concat skk-user-directory "SKK-JISYO.L"))
        (unless (file-exists-p skk-user-directory)
          (make-directory skk-user-directory))
        (url-copy-file "http://openlab.jp/skk/dic/SKK-JISYO.L.gz" "SKK-JISYO.L.gz")
        (call-process-shell-command "gzip -d SKK-JISYO.L.gz")
        (call-process-shell-command (format "mv SKK-JISYO.L %s" skk-user-directory))))

    (init-check-jisyo-file)
    (setq-default skk-kutouten-type 'en)
    (setq default-input-method "japanese-skk"
          ;; skk-kuten-touten-alist '((jp . ("." . "," ))
          ;;                          (en . ("." . ","))); 句読点を「，．」にする
          skk-large-jisyo (concat (file-name-as-directory skk-user-directory) "SKK-JISYO.L")
          skk-use-azik t
          skk-azik-keyboard-type 'jp106)))
