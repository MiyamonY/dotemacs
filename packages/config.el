;; -*- coding: utf-8 -*-
(setq gc-cons-threshold (* gc-cons-threshold 10))
;; make curor move faster
;; https://emacs.stackexchange.com/questions/28736/emacs-pointcursor-movement-lag
(setq auto-window-vscroll nil)

(defun init-open-dotemacs ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(bind-key* "C-h" 'delete-backward-char)
(bind-key "C-h" 'isearch-delete-char isearch-mode-map)
(bind-key "C-m" 'newline-and-indent)
(bind-key "C-c l" 'toggle-truncate-lines)
(bind-key "M-l" 'goto-line)
(bind-key "C-c i" 'init-open-dotemacs)
(bind-key "C-c j" 'split-window-horizontally)
(bind-key "C-c u" 'split-window-vertically)
(bind-key "C-c r" 'query-replace)
(bind-key "C-x w" 'delete-frame)

(tool-bar-mode -1)
(setq inhibit-startup-message t
      frame-title-format "%f"
      line-move-visual t
      kill-whole-line t
      completion-ignore-case t)
(fset 'yes-or-no-p 'y-or-n-p)
(auto-image-file-mode t)
(scroll-bar-mode -1)
(delete-selection-mode 1)
(global-display-line-numbers-mode 1)
(setq x-wait-for-event-timeout nil)     ; bug for emacs26
(setq-default show-trailing-whitespace t)
(setq-default bidi-display-reordering nil)
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq-default fill-column 120)

(use-package saveplace
  :init
  (progn
    (setq save-place-file (concat init-emacs-local-files "/.places"))))
(save-place-mode 1)

;; backup fileの設定
(setq backup-directory-alist '((".*" . "/tmp")))

(use-package recentf-ext
  :init
  (progn
    (setq recentf-save-file (concat init-emacs-local-files "/recentf/.recentf"))
    (setq recentf-max-saved-items 200)
    (setq recentf-auto-cleanup 60)
    (setq recentf-auto-save-timer
          (run-with-idle-timer 30 t 'recentf-save-list)))
  :config
  (progn
    (setq recentf-exclude (-concat '(".recentf") recentf-exclude))))

(recentf-mode 1)
(setq make-backup-files nil)
(setq create-lockfiles nil)
(setq auto-save-list-file-prefix (concat init-emacs-local-files "/auto-save-list/.saves-"))

;; 括弧の設定
(use-package paren
  :config
  (progn
    (set-face-background 'show-paren-match nil) ;バックグランドフェイスを消す
    (set-face-underline 'show-paren-match "red")) ;下線の色をつける
  :init
  (progn
    (setq show-paren-delay 0.1   ;括弧のハイライトを表示するまでの時間
          show-paren-style 'expression) ;括弧の中もハイライト
    ))
(show-paren-mode t)

;; prettify mode
(setq prettify-symbols-unprettify-at-point 'right-edge)
(global-prettify-symbols-mode)

;; hook
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; window
(defadvice quit-window (before quit-window-always-kill)
  "When running `quit-window', always kill the buffer."
  (ad-set-arg 0 t))
(ad-activate 'quit-window)

;; org-agenda
(use-package org-agenda
  :bind
  (("C-c a" . 'org-agenda))
  :init
  (progn
    (setq org-directory "~/src/github.com/MiyamonY/memo")
    (setq org-agenda-files (list org-directory))))

(defun capture-report-data-file ()
  (format "%s.org" (format-time-string "%Y-%m-%d")))

(use-package org-capture
  :bind
  (("C-c c" . 'org-capture))
  :init
  (progn
    (setq org-capture-templates
          `(("t" "Task" entry (file "todos.org")
             "* TODO %?\n    %i %T")
            ("m" "Memo" entry (file "memo.org")
             "* %?\n %a\n %T")
            ("n" "Note" entry (file ,(capture-report-data-file)) "* %?" :empty-lines 1 :jump-to-captured 1)))))

;; フォントの設定
(cond ((display-graphic-p)
       (create-fontset-from-ascii-font
        "Ricty Diminished Discord-12:weight=normal:slant=normal"
        nil
        "Ricty_Diminished_Discord")
       (set-fontset-font
        "fontset-ricty_diminished_discord"
        'unicode
        (font-spec :family "Ricty Diminished Discord" :weight 'normal :slant 'normal)
        nil
        'append)
       (add-to-list 'default-frame-alist
                    '(font . "fontset-ricty_diminished_discord"))
       (add-to-list 'initial-frame-alist
                    '(font . "fontset-Ricty_Diminished_Discord"))))

;; エンコーディングの設定(上書きされてしまうので後ろの方で設定する)
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)
