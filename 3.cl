#!/usr/bin/env clisp
;; LIST returns list of arguments
(list 5 3 9 'the)

;; property list or plist is a kind of dictionary/associative array
;; Keywords begin with a colon :
;;

(list :a 85 :b 1337 :c "my string")

;; Use getf to access a member of the plist:
(getf (list :a 85 :b 1337 :c "my string") :c)

(defun make-book (title author year ebook)
  (list :title title :author author :year year :ebook ebook))

(make-book "All's Quiet on the Western Front" "Remarque" 1929 nil)

(defvar *db* nil)
(defun add-record (book)
  (push book *db*))

;; Push adds a record into *db* 
(add-record (make-book "All Quiet on the Western Front" "Remarque" 1929 nil))
(add-record (make-book "The Trial" "Kafka" 1925 nil))
(add-record (make-book "Faust" "Goethe" 1808 t))
(print *db*)

;; Or we can sort its display
(defun dump-db()
  (dolist (book *db*)
  	(format t "~{~a:~10t~a~%~}~%" book)))
;;
;; Function loops over all elements of *db* with the DOLIST macro
;; Formatting covered further in Chapter 18
;; ~a is "aesthetic" directive; renders "" and : to text. Takes symbol/string as arg
;; ~t tabulates. ~10t formats ten spaces wide. no args
;; ~% is newline
;; ~{ and ~} loops over a list

;; PROMPTS
(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt) 
  (force-output *query-io*) ;; forces a wait for output before continuing the rest of the function
  (read-line *query-io*))	;; reads what you type into *query-io*

(defun prompt-for-book ()
  (make-book
  	(prompt-read "Title")
  	(prompt-read "Author")
  	(or (parse-integer (prompt-read "Year") :junk-allowed t) 0)
  	(y-or-n-p "Is this an Ebook? [y/n]"))) ;; Yes, this will totally handle the prompting and parsing of y/n answer!

;; PROMPT-READ will let you take anything from user input.
;; But using PARSE-INTEGER will coerce it into an integer, as will be useful for the "year" field.
;; :junk-allowed argument loosens PARSE-INTEGER's constraint, and if that's still not enough, you can or it and defaul it to zero
;;

(defun add-books ()
  (loop (add-record (prompt-for-book))
  		(if (not (y-or-n-p "Add another?"))
  		  (return))))
;(format t "~%Add the name of your book:~%")
;(add-books)

;; === Saving and Loading Database ===

(defun save-db (filename)
  (with-open-file (out filename
  					   :direction :output
  					   :if-exists :supersede)
  	(with-standard-io-syntax
  	  (print *db* out))))
;; WITH-OPEN-FILE is a macro. It opens a file, binds stream to variable, executes expressions then closes file
;; You run (print *db* out) to save the database.
;; **PRINT prints Lisp objects in a form that can be read by Lisp. WITH-STANDARD-IO-SYNTAX ensures variables that affect behavior of PRINT are set to standard values
;; Same macro used to read datab back in 

(save-db "./my-books.db")

;; Loading the database

(defun load-db (filename)
  (with-open-file (in filename)
  	(with-standard-io-syntax
  	  (setf *db* (read in))))) ;; setf updates value of a var already created

;; SELECTing from database
(loop for i from 1 to 20 collecting i)

(remove-if-not #'evenp '(1 2 3 4 5 6)) ;; Returns a new list with the odd numbers stripped.

;; May also pass remove-if-not an anonymous function
(remove-if-not #'(lambda (x)
				   (= 0 (mod x 2)))
			   '(1 2 3 4 5 6 7 8 9 10))

(defun select-by-author (author)
  (remove-if-not
  	#'(lambda (book)
  		(equal (getf book :author) author))
  	*db*))

;; We coulse add more select statements per author, but its better to make it more general. Here we pass SELECT a function, much like a decoratorin Python

(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

(defun author-selector (author)
  #'(lambda (book) (equal (getf book :author) author)))

;; We will make select even more generizable by sending it keyword arguments.
;; First we will review how kwargs work in Lisp

(defun foo (&key a b c) ;; Note &key at the beginning of the argument list
  (list a b c))
	;; call this like so:
(foo :a 3 :c 'Yes :b "My string")
	;; If you call the function without specifying arguments, it will return nil
(foo :a 1)
	;; Default args work like this:
(defun foo-bar (&key a (b 20) (c 30 c-p))
  (list a b c c-p))
	;; c-p is a `supplied-p` parameter. Will be set to true or false whether an argumnet was actually passed for that keyword or not
	;; If yes, it returns the value and True, if no, it returns 30 and Nil

; (defun where (&key title author year (ebook nil ebook-p))
;   #'(lambda (book)
;   	  (and	;; uses the logical AND of the result of our select statement
;   	  	(if title	(equal (getf book :title)	title)	t)
;   	  	(if author	(equal (getf book :author)	author)	t)
;   	  	(if year	(equal (getf book :year)	year)	t)
;   	  	(if ebook	(equal (getf book :ebook)	ebook)	t))))

; (print (select (where :author "Kafka")))

;; Updating records
(defun update (selector-fn &key title author year (ebook nil ebook-p))
  (setf *db*
  		(mapcar
  		  #'(lambda (row)
  		  	  (when (funcall selector-fn row)
  		  	  	(if title	(setf (getf row :title) title))
  		  	  	(if author	(setf (getf row :author) author))
  		  	  	(if year	(setf (getf row :year) year))
  		  	  	(if ebook-p	(setf (getf row :ebook) ebook)))
  		  	  row) *db)))

;; Deleting
(defun delete-rows (selector-fn)
    (setf *db* (remove-if selector-fn *db*)))

;;;;; Macros ;;;;;;;;
;; Once called, the Lisp compiler passes the arguments unevaluated to the macro,
;; which inturn returns new a Lisp evaluated expression in place of the original
;; macro.

(defmacro backwards (expr)
  (reverse expr))

(backwards "my string")

;; (defun make-comparison-expr (field value)
;;   (list 'equal (list 'getf 'book field) value))

;; Even better, making use of the backtick
;; Backtick passes everything as data, except objects prefixed with ,
;; as in teh case bellow with ,field and ,value
(defun make-comparison-expr (field value)
  `(equal (getf book ,field) ,value))

;; Towards a new and improved where

(defun make-comparisons-list (fields)
  (loop while fields
  		collecting (make-comparison-expr (pop fields) (pop fields))))

(defmacro where (&rest clauses)
  `#'(lambda (book) (and ,@(make-comparisons-list clauses))))

;; in the context of ` ,@ "splices" the value, eg, returns everything within a collection
;; as its own unique entry to the as-data
;;
;; &rest modifies the way arguments are passed, like &key
;; &rest takes an arbitrary number of arguments
