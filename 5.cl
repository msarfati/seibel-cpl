;;;; Functions
;;;
;;; Three most basic components of all Lisp programs:
;;; functions, variables, macros.
;;; Lisp isn't a pure functional language -- Common Lisp was designed to support a variety of styles. Scheme is the closest to pure functional, but still falls short. True pure functional languages include Haskell and ML.
;;; 
;;; :: Defining New Functions:: 
;;; Skel:

;;; (defun name (parameter*)
;;;  "Optional documentation string."
;;;  body-form*)

;;; Any symbol is usable as function name. Usually contain letters and hyphens, but other chars are allowed by convention.
;;; -> means convert, eg, string->widget
;;; 
;;; use hyphens, not underscores or inner caps
;;;
;;; Parameter lists are also called *lambda lists*
;;;
;;; DOCUMENTATION function returns the doc string.:
;;; 	(documentation 'foo 'function)
(defun hola-muendo ()
  "hola-muendo returns a mystery formatted string!"
  (format t "hola muendo!"))
(documentation 'hola-muendo 'function)

(defun verbose-sum (x y)
  "Sums two numbers after printing."
  (format t "Summing ~d and ~d.~%" x y)
  (+ x y))

(defun verbose-sum2 (x y)
  "Displays the addends and their sum in a single format string."
  (format t "Summing ~d and ~d: ~d.~%" x y (+ y y))
  )

(format t "Documentation for verbose-sum2:~%")
(format t "~a~%" (documentation 'verbose-sum2 'function))

;;; :: Function Parameter Lists ::
;;; Parameter list, also known as the arguments
;;; Parameters may be optional or keyword passed
;;;
;;; : Optional Parameters: 
;;; &OPTIONAL followed by name of paramters:

(defun foo (a b &optional c d)
  (list a b c d))

;;; But this will return NIL for unspecified values C and D. If you want to ahve it return a default value:

(defun foo-two (a &optional (b 99) (c 'apple))
  (list a b c))

(print (foo-two 4 'yes 99))	; no keyword arguments, reads in sequentially.

;;; More flexibility:

(defun my-rectangle (width &optional (height width)) ; Height param would be the same as the width param
  (* height width))

;;; Review of Keyword arguments
(defun my-month (&key month (days 30))
  "Returns pretty format of month. Requires month and day-count"
  (format t "Month: ~a~%Number of Days: ~d~%" month days))

(my-month :month "Hexembruary" :days 45)

;;; You can find out if the value to the optional argument was provided by caller or is the default value:

(defun my-foo-three (a &optional (b 99 b-supplied-p))
  (list a b b-supplied-p)) ; b-supplied-p will return a boolean value

;;; :: Rest Parameters ::
