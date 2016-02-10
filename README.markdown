This was done just for fun!

**CLEMENY** is a (very basic) BASIC to Common Lisp compiler. It supports `REM`, `LET`, `IF .. THEN`, `GOTO`, `PRINT`, and `END`, as well as simple arithmetics. All lines must be numbered, and may appear out of order.

The name CLEMENY is named after John G. Kemeny, the pioneer who co-invented BASIC with Thomas E. Kurtz in 1964.

## How it works

CLEMENY uses the [esrap](http://scymtym.github.io/esrap/) library to build a grammar for BASIC.

Let's have a look at a sample program to sum all multiples of 3 and 5 below 1000:

```
0000 REM Count multiples of 3 and 5 below 1000

0010 LET I = 3
0011 LET SUM = 0

9000 PRINT SUM
9001 END

0100 LET TEMP = I % 3
0110 IF TEMP = 0 THEN 400

0200 LET TEMP = I % 5
0210 IF TEMP = 0 THEN 400

0300 LET I = I + 1
0310 IF I = 1000 THEN 9000
0320 GOTO 100

0400 LET SUM = SUM + I
0410 GOTO 300
```

When parsed this produces an abstract syntax tree in Lisp looking like this:

```
((0 ("REM" " Count multiples of 3 and 5 below 1000")) 
 (10 ("LET" "I" 3))
 (11 ("LET" "SUM" 0)) 
 (100 ("LET" "TEMP" ("MOD" "I" 3)))
 (110 ("IF" ("=" "TEMP" 0) 400)) 
 (200 ("LET" "TEMP" ("MOD" "I" 5)))
 (210 ("IF" ("=" "TEMP" 0) 400)) 
 (300 ("LET" "I" ("+" "I" 1)))
 (310 ("IF" ("=" "I" 1000) 9000)) 
 (320 ("GOTO" 100))
 (400 ("LET" "SUM" ("+" "SUM" "I"))) 
 (410 ("GOTO" 300)) 
 (9000 ("PRINT" "SUM"))
 (9001 ("END"))) 
```

Compiling this tree into executable Common Lisp code is actually not very hard, once you know about the [`TAGBODY`](http://clhs.lisp.se/Body/s_tagbod.htm#tagbody) operator. The compiled program will eventually look like this:

```
(BLOCK NIL
  (LET (TEMP I SUM)
    (TAGBODY
     0
     10
      (SETF I 3)
     11
      (SETF SUM 0)
     100
      (SETF TEMP (MOD I 3))
     110
      (WHEN (= TEMP 0) (GO 400))
     200
      (SETF TEMP (MOD I 5))
     210
      (WHEN (= TEMP 0) (GO 400))
     300
      (SETF I (+ I 1))
     310
      (WHEN (= I 1000) (GO 9000))
     320
      (GO 100)
     400
      (SETF SUM (+ SUM I))
     410
      (GO 300)
     9000
      (FORMAT T "~A~%" SUM)
     9001
      (RETURN)))) 
``` 

By sending that code to `EVAL` the sum of 233168 is printed to the terminal.