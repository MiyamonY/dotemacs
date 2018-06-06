;;; init-flycheck-pos-tip.el --- initalize flycheck-pos-tip  -*- lexical-binding: t; -*-

;; Copyright (C) 2018

;; Author:  <ymiyamoto@ymiyamoto-desktop>
;; Keywords: abbrev

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(use-package flycheck-pos-tip
  :init
  (progn
    (setq flycheck-pos-tip-timeout 10))
  :hook
  ((flycheck-mode . flycheck-pos-tip-mode)))


(provide 'init-flycheck-pos-tip)
;;; init-flycheck-pos-tip.el ends here
