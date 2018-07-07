;;; init-powerline.el --- initialize powerline       -*- lexical-binding: t; -*-

;; Copyright (C) 2018

;; Author:  <ymiyamoto@ymiyamoto-desktop>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(defun shorten-directory (dir max-length)
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat ".../" output)))
    output))

(use-package powerline
  :init
  (progn
    (defun powerline-my-theme ()
      (setq-default mode-line-format
                    '("%e"
                      (:eval
                       (let* ((active (powerline-selected-window-active))
                              (mode-line-buffer-id (if active 'mode-line-buffer-id 'mode-line-buffer-id-inactive))
                              (mode-line (if active 'mode-line 'mode-line-inactive))
                              (face0 (if active 'powerline-active0 'powerline-inactive0))
                              (face1 (if active 'powerline-active1 'powerline-inactive1))
                              (face2 (if active 'powerline-active2 'powerline-inactive2))
                              (separator-left (intern (format "powerline-%s-%s"
							                                                (powerline-current-separator)
                                                              (car powerline-default-separator-dir))))
                              (separator-right (intern (format "powerline-%s-%s"
                                                               (powerline-current-separator)
                                                               (cdr powerline-default-separator-dir))))
                              (lhs (list (powerline-raw "%*" face0 'l)
                                         (when powerline-display-buffer-size
                                           (powerline-buffer-size face0 'l))
                                         (when powerline-display-mule-info
                                           (powerline-raw mode-line-mule-info face0 'l))
                                         (funcall separator-left face0 face1)
                                         (powerline-raw (shorten-directory default-directory 15) face1 'l)
                                         (funcall separator-left face1 face0)
                                         (powerline-buffer-id nil 'r)
                                         (when (and (boundp 'which-func-mode) which-func-mode)
                                           (powerline-raw which-func-format face0 'l))
                                         (powerline-raw " " face0)
                                         (funcall separator-left face0 face1)
                                         (when (and (boundp 'erc-track-minor-mode) erc-track-minor-mode)
                                           (powerline-raw erc-modified-channels-object face1 'l))
                                         (powerline-major-mode face1 'l)
                                         (powerline-process face1)
                                         (powerline-narrow face1 'l)
                                         (powerline-raw " " face1)
                                         (funcall separator-left face1 face0)
                                         (powerline-vc face0 'r)
                                         (when (bound-and-true-p nyan-mode)
                                           (powerline-raw (list (nyan-create)) face2 'l))))
                              (rhs (list (powerline-raw global-mode-string face2 'r)
                                         (funcall separator-right face2 face1)
				                                 (unless window-system
				                                   (powerline-raw (char-to-string #xe0a1) face1 'l))
				                                 (powerline-raw "%4l : %3c " face1 'l)
				                                 (funcall separator-right face1 face0)
				                                 (powerline-raw " " face0)
				                                 (powerline-raw "%6p" face0 'r)
				                                 (powerline-fill face0 0))))
		                     (concat (powerline-render lhs)
			                           (powerline-fill face2 (powerline-width rhs))
			                           (powerline-render rhs)))))))
    (powerline-my-theme)))

(provide 'init-powerline)
;;; init-powerline.el ends here
