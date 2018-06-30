;;; init-flycheck-color-mode-line.el --- initialize flycheck mode line  -*- lexical-binding: t; -*-

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
(use-package flycheck-color-mode-line
  :config
  (progn
    (set-face-foreground 'flycheck-color-mode-line-error-face "#ff5555") ; red
    (set-face-foreground 'flycheck-color-mode-line-warning-face "#ffb86c") ; orange
    (set-face-foreground 'flycheck-color-mode-line-success-face "#50fa7b") ; green
    ))


(provide 'init-flycheck-color-mode-line)
;;; init-flycheck-color-mode-line.el ends here
