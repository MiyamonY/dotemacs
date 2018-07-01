;;; init-helm-gtags.el --- initialize helm gtags     -*- lexical-binding: t; -*-

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
(use-package helm-gtags
  :init
  (progn
    (setq helm-gtags-auto-update t))
  :bind
  (("M-t" . helm-gtags-dwim)
   ("M-r" . helm-gtags-find-rtag)
   ("M-p" . helm-gtags-previous-history)
   ("M-n" . helm-gtags-next-history)))

(provide 'init-helm-gtags)
;;; init-helm-gtags.el ends here
