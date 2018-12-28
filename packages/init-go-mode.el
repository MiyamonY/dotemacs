;;; init-go-mode.el --- initialize go-mode           -*- lexical-binding: t; -*-

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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(use-package go-mode
  :config
  (progn
    (setq gofmt-command "goimports")
    (add-hook 'go-mode-hook
              (lambda ()
                (set (make-local-variable 'company-backends)
                     '((company-dabbrev-code company-yasnippet)))))
    (add-hook 'before-save-hook 'gofmt-before-save)))

(provide 'init-go-mode)
;;; init-go-mode.el ends here
