#!/usr/bin/guile \
-e main -s
!#
;this is a caesar cipher implemented as a guile script

(define (usage)
  (display "usage: ")
  (display (car (command-line)))
  (display "<string> [number to rotate by]")
  (newline)
  (exit))

(define (main args)
  (let* ((args-len (length args))
         (str (if (and (>= args-len 2) (string? (list-ref args 1))) (list-ref args 1) (usage)))
         (rot (if (= args-len 3) 
                (let ((num (string->number (list-ref args 2))))
                  (if (equal? num #f) (usage) num))
                13))
         (Z (char->integer #\Z))
         (A (char->integer #\A))
         (encode (lambda (char)
                   (let ((char-in-ascii (char->integer char)))
                     (if (or (> char-in-ascii Z) (< char-in-ascii A))
                          char
                          (let ((out (+ rot char-in-ascii)))
                            (integer->char (if (> out Z)
                                             (- out 26)
                                             out))))))))
              (display  (list->string (map (lambda (x) (encode (char-upcase x))) (string->list str))))
              (newline)))

