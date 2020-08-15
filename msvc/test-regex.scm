(display (string-match "[0-9][0-9][0-9][0-9]" "blah2002"))
;;; ⇒ #("blah2002" (4 . 8))
(display (string-match "[A-Za-z]" "123456"))
;;; ⇒ #f
;; Regexp to match uppercase letters
(define r (make-regexp "[A-Z]*"))

;; Regexp to match letters, ignoring case
(define ri (make-regexp "[A-Z]*" regexp/icase))

;; Search for bob using regexp r
(display (match:substring (regexp-exec r "bob")))
;;; ⇒ ""                  ; no match

;; Search for bob using regexp ri
(display (match:substring (regexp-exec ri "Bob")))
;;; ⇒ "Bob"               ; matched case insensitive

(display (map match:substring (list-matches "[a-z]+" "abc 42 def 78")))
;;; ⇒ ("abc" "def")

(display (fold-matches "[a-z][0-9]" "abc x1 def y2" 0
                       (lambda (match count)
                         (1+ count))))
;;; ⇒ 2
(display (regexp-substitute #f (string-match "[0-9]+" "number 25 is good")
                            'pre "37" 'post))
;;; ⇒ "number 37 is good"

(define date-regex
   "([0-9][0-9][0-9][0-9])([0-9][0-9])([0-9][0-9])")
(define s "Date 20020429 12am.")
(display (regexp-substitute #f (string-match date-regex s)
                            'pre 2 "-" 3 "-" 1 'post " (" 0 ")"))
;;; ⇒ "Date 04-29-2002 12am. (20020429)"

(display (regexp-substitute/global #f "[ \t]+"  "this   is   the text"
                                   'pre "-" 'post))
;;; ⇒ "this-is-the-text"

(display (regexp-substitute/global #f "[a-z]+"  "to do and not-do"
           'pre (lambda (m) (string-reverse (match:substring m))) 'post))
;;; ⇒ "ot od dna ton-od"

(define date-regex
   "([0-9][0-9][0-9][0-9])([0-9][0-9])([0-9][0-9])")
(define s "Date 20020429 12am.")
(display (regexp-substitute/global #f date-regex s
                                   'pre 2 "-" 3 "-" 1 'post " (" 0 ")"))

;;; ⇒ "Date 04-29-2002 12am. (20020429)"
