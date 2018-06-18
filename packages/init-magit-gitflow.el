;;; init-magit-gitflow.el --- initialize magit-gitflow  -*- lexical-binding: t; -*-

;; Copyright (C) 2018

;; Author:  <ymiyamoto@ymiyamoto-pc>
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(use-package magit-gitflow
  :commands
  (turn-on-magit-gitflow)
  :init
  (progn
    (add-hook 'magit-mode-hook 'turn-on-magit-gitflow)))


(provide 'init-magit-gitflow)
;;; init-magit-gitflow.el ends here
