;;; File:  `(file-name-nondirectory (buffer-file-name))`
;; Author: ymiyamoto
;;
;; Created on `(current-time-string)`
;;
(define (read-number)
  (string->number (read-line)))

(define (read-numbers)
  (map string->number (string-split (read-line) #\space)))
