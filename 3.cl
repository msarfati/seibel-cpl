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
(defun add-record (book) (push book *db*))
;; Push adds a record into *db* 
(add-record (make-book "All's Quiet on the Western Front" "Remarque" 1929 nil))
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
  	(prompt-read "Year")
  	(prompt-read "Ebook? [y/n]")))
