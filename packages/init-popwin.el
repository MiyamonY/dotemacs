;;; init-popwin.el --- initalize popwin              -*- lexical-binding: t; -*-

;; Copyright (C) 2018

;; Author:  <ymiyamoto@ymiyamoto-desktop>
;; Keywords: lisp

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

;;; Code:
(use-package popwin
  :init
  (progn
    (setq popwin:close-popup-window-timer-interval 0.05))
  :config
  (progn
    (setq popwin:special-display-config
          (-concat
           '(("*Help*" :height 20 :position bottom)
             ("*Backtrace*":height 12 :position bottom :noselect t)
             ("*ack*":height 12 :position bottom)
             ("^\*Org.+\*$" :height 20 :position bottom :regexp t)
             ("^\*magit.+\*$" :height 25 :position bottom :regexp t)
             (" *undo-tree*" :width 0.3 :position right)) popwin:special-display-config))))

(popwin-mode 1)

(provide 'init-popwin)
;;; init-popwin.el ends here
