;;;; grammar.lisp


(in-package #:grammar)


(defrule ws (+ (or #\space #\tab))
  (:constant nil))

(defrule nl (+ (and (? #\return) #\linefeed))
  (:constant nil))

(defrule wsnl (+ (or ws nl))
  (:constant nil))

(defrule integer (+ (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
  (:lambda (list)
    (parse-integer (text list) :radix 10)))

(defrule name (+ (upper-case-p character))
  (:text t))

(defrule line-label (and (? ws) integer)
  (:function cadr))

(defrule value (or name integer))

(defrule operator (or #\+ #\- #\* #\/ #\%)
  (:lambda (x)
    (if (equal x "%")
      "MOD"
      x)))

(defrule expression (or (and value (? ws) operator (? ws) value)
                        value)
  (:lambda (x)
    (if (atom x)
      x
      (destructuring-bind (v1 w1 op w2 v2) x
        (list op v1 v2)))))

(defrule comparison (and value ws (or #\= #\> #\<) ws value)
  (:destructure (v1 w1 op w2 v2)
    (list op v1 v2)))

(defrule nullary-command (or "END")
  (:function list))

(defrule unary-command (and (or "GOTO" "PRINT") ws value)
  (:destructure (c w v)
    (list c v)))

(defrule comment (and "REM" (* (graphic-char-p character)))
  (:destructure (rem rest)
    (list rem (text rest))))

(defrule let-command (and "LET" ws name ws #\= ws expression)
  (:destructure (let w1 var w2 eq w3 e)
    (list let var e)))

(defrule if-command (and "IF" ws comparison ws "THEN" ws integer)
  (:destructure (if w1 c w2 then w3 i)
    (list if c i)))

(defrule command (or comment
                     nullary-command
                     unary-command
                     let-command
                     if-command))

(defrule line (and line-label (? (and ws command)))
  (:destructure (l wc)
    (if wc
      (cons l (cdr wc))
      (list l))))

(defrule line-terminated (and (? wsnl) line (? ws) (+ nl))
  (:function second))

(defrule program (and (* line-terminated)
                      (? line))
  (:destructure (lines maybe-last-line)
    (if maybe-last-line
      (reverse (cons maybe-last-line (reverse lines)))
      lines)))
