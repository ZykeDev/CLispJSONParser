;;;; -*- Mode: Lisp -*-

;;;; Author: Marco Vincenzi
;;;; Matricola: 795694

;;;; json-parsing.lisp



;; Structure of a json-obj.
(defun make-json-obj (m)
  (cond ((null m) (list 'json-obj))
        (t (append (list 'json-obj) m))))

;; Structure of a json-array.
(defun make-json-array (e)
  (cond ((null e) (list 'json-array))
        (t (append (list 'json-array) e))))

;; Structure of a pair (k v).
(defun make-pair (k v)
  (list (cons k (cons v '()))))



;; json-parse
;; From an input string, returns a JSON object.
(defun json-parse (input)
  ;; Convert the string into a list of characters and parse it.
  (parse-main-obj (remove-trailing (coerce input 'list))))
  
  
;; Parse function to be called with the initial input.
(defun parse-main-obj (in)
  (cond ((opened-obj-p in)
              
         (cond ((closed-obj-p (rest (remove-ws in)))
                (make-json-obj nil))
               (t (multiple-value-bind (p r) 
                                       (parse-members (rest (remove-ws in)))
                    ;; Check if the entire input has been parsed.
                    ;; If ther's a rest that couldn't be parsed, throw an error
                    (cond ((null (remove-ws r)) (make-json-obj p))
                          (t (error "Invalid JSON Syntax.")))))))

        ((opened-array-p in)
         (multiple-value-bind (a r) (parse-array-members (rest (remove-ws in)))
           (make-json-array a)))

        (t (error "Invalid JSON Object."))))


;; Parses a json-obj and its members.
(defun parse-obj (in)
  (if (opened-obj-p in)
      (multiple-value-bind (p r) (parse-members (rest in))
           (values (make-json-obj p) r))))
    


;; Parses key and value, then the other memebers.
(defun parse-members (in)
  ;; Parse the key
  (multiple-value-bind (k kr) (parse-key in)

    ;; Parse the corresponding value from the rest from parse-key.
    (multiple-value-bind (v vr) (parse-value kr)

        ;; If this is the last member, return the pair and close the object.
        (cond ((closed-obj-p (remove-ws vr))  
               (values (make-pair k v) (remove-closed-obj (remove-ws vr))))
              
              ;; If there are other members, parse them and append them after.
              ((comma-p (remove-ws vr))    
              (multiple-value-bind (p r) (parse-members (rest (remove-ws vr)))
                (values (append (make-pair k v) p) r)))))))




;; Returns (key rest). Key is always a string. Rest doesn't contain ":"
(defun parse-key (in)
  (multiple-value-bind (k r) (parse-string (remove-ws in))
    (values k (remove-colon r))))



;; Parses the value according to its type.
(defun parse-value (in)
  (cond ((undefined-p in)
         (error "Undefined value."))

        ((string-start-p in)                                  
         (parse-string in))
        
        ((number-p in)
         (parse-number in))
        
        ;; If the array has no items, create an empty array.
        ;; Otherwise, parse the array normally.
        ((opened-array-p in)
         (cond ((closed-array-p (rest (remove-ws in)))
                (values (make-json-array nil) (rest (rest (remove-ws in)))))
               (t (parse-array in))))
         
        ;; If the object has no members, create an empty object.
        ;; Otherwise parse the object normally.
        ((opened-obj-p in)
         (cond ((closed-obj-p (rest (remove-ws in)))
                (values (make-json-obj nil) (rest (rest (remove-ws in)))))
               (t (parse-obj in))))
        
        (t (error "Syntax error: Invalid value."))))



;; Parses the string and returns (string rest).
(defun parse-string (in)
  (cond ((DQ-p in)
         (multiple-value-bind (s r) (parse-string-DQ (rest in))
           ;; Coerces the list of chars into a string
           (values (coerce s 'string) r)))

        ((SQ-p in)
         (multiple-value-bind (s r) (parse-string-SQ (rest in))
           (values (coerce s 'string) r)))))
  
  
  
;; Returns (string rest). The input list does not contain the initial DQ.
(defun parse-string-DQ (in)
  (cond ((not (DQ-c-p (first in)))
         (multiple-value-bind (s r) (parse-string-DQ (rest in))
           (values (append (list (first in)) s) r)))
        (t (values nil (rest in)))))

;; Returns (string rest). The input list does not contain the initial SQ.
(defun parse-string-SQ (in)
  (cond ((not (SQ-c-p (first in)))
         (multiple-value-bind (s r) (parse-string-SQ (rest in))
           (values (append (list (first in)) s) r)))
        (t (values nil (rest in)))))



;; Parses the number and returns (number rest).
(defun parse-number (in)
  (cond ((dash-p (first in))
         (multiple-value-bind (n r) (parse-number-abs (rest in)) 
           (values (- 0 (read-from-string (coerce n 'string))) r)))

        ((plus-p (first in))
         (multiple-value-bind (n r) (parse-number-abs (rest in)) 
           (values (read-from-string (coerce n 'string)) r)))

        (t (multiple-value-bind (n r) (parse-number-abs in)
             (values (read-from-string (coerce n 'string)) r)))))
         

;; Parses the absolute value of a number, returns (number rest).
(defun parse-number-abs (in)
  (cond ((or (dash-p (first in)) (plus-p (first in)))
         ;; Throws an error if it find another "-" or "+"
         (error "Invalid numerical value."))
                 
        ((digit-char-p (first in))
         (multiple-value-bind (d r) (parse-number-abs (rest in))
           (values (cons (first in) d) r)))
        ((dot-p (first in))
         (multiple-value-bind (d r) (parse-number-dec (rest in))
           (values (cons (first in) d) r)))
        (t (values nil in))))


;; Parses the decimal part of a float value, returns (decimal rest).
(defun parse-number-dec (in)
  (cond ((digit-char-p (first in))
         (multiple-value-bind (d r) (parse-number-dec (rest in))
           (values (cons (first in) d) r)))
        ((dot-p (first in))
         (error "Invalid numerical value."))
        (t (values nil in))))



;; Parses the input and returns (array rest).
(defun parse-array (in)
  (multiple-value-bind (a r) (parse-array-members (rest (remove-ws in)))
    (values (make-json-array a) r)))

;; Parses the memebers of the input array
(defun parse-array-members (in)
  (multiple-value-bind (v r) (parse-value (remove-ws in))

    ;; If this is the last item in the array
    (cond ((closed-array-p (remove-ws r))
           (values (append (list v) nil) (remove-closed-array (remove-ws r))))
          
          ;; If there are other items in the array
          ((comma-p (remove-ws r))
           (multiple-value-bind (nv nr) 
                                (parse-array-members (rest (remove-ws r)))
             (values (append (list v) nv) nr)))))) 


(defun undefined-p (in)
  (and 
   (char= (char-upcase (first in))   #\U)
   (char= (char-upcase (second in))  #\N)
   (char= (char-upcase (third in))   #\D)
   (char= (char-upcase (fourth in))  #\E)
   (char= (char-upcase (fifth in))   #\F)
   (char= (char-upcase (sixth in))   #\I)
   (char= (char-upcase (seventh in)) #\N)
   (char= (char-upcase (eighth in))  #\E)
   (char= (char-upcase (ninth in))   #\D)))


;; Custom version of numberp, as the input is a list of chars.
(defun number-p (in)
  (or (and (dash-p (first in)) (digit-char-p (second in)))
      (and (plus-p (first in)) (digit-char-p (second in)))
      (digit-char-p (first in))))

(defun float-p (in)
  (cond ((digit-char-p (first in)) (float-p (rest in)))
        ((dot-p (first in)) (dot-p (first in)))
        (t nil)))

(defun string-start-p (in)
  (or (DQ-p in) (SQ-p in)))

(defun DQ-c-p (c)
  (char= c #\"))

(defun SQ-c-p (c)
  (char= c #\'))

(defun DQ-p (in)
  (DQ-c-p (first (remove-ws in))))

(defun SQ-p (in)
  (SQ-c-p (first (remove-ws in))))

(defun comma-p (c)
  (char= (first (remove-ws c)) #\,))

(defun dash-p (c)
  (char= c #\-))

(defun plus-p (c)
  (char= c #\+))

(defun dot-p (c)
  (char= c #\.))

(defun colon-p (c)
  (if (not (null c))
       (char= c #\:)))

(defun opened-obj-p (in)
  (char= (first (remove-ws in)) #\{))
        
(defun closed-obj-p (in)
  (char= (first (remove-ws in)) #\}))
        
(defun opened-array-p (in)
  (char= (first (remove-ws in)) #\[))
        
(defun closed-array-p (in)
  (char= (first (remove-ws in)) #\]))

;; Checks for Whitespaces/Newlines/etc.
(defun ws-p (in)
  (if (not (null in))
      (cond ((listp in)
             (cond ((= (length in) 1) (ws-c-p (list-to-single-char in)))
                   (t (ws-c-p (first in)))))
            (t (ws-c-p in)))))

;; Checks if the single char (c) is a Whitespace/Newline/etc.
(defun ws-c-p (c)
  (cond ((or (or (char= c #\Space)
                 (char= c #\Tab))
             (char= c #\Newline)))))

;; Determines if q is an object
(defun obj-q-p (q)
  (cond ((and (not (listp q)) (not (numberp q)))
         (string= 'json-obj q))   
        (t nil)))


;; Determines if q is an array
(defun array-q-p (q)
  (cond ((and (not (listp q)) (not (numberp q)))
         (string= 'json-array q))
        (t nil)))



;; Returns the list without trailing spaces or newlines.
(defun remove-ws (in)
  (cond ((not (ws-p in)) in)  
        (t (remove-ws (rest in)))))


;; Removes the first colon it finds and returns the rest of the input.
;; If no colon is found, the parser throws an error.
(defun remove-colon (in)
  (cond ((colon-p (first in)) (remove-ws (rest in)))
        ((ws-p in) (remove-colon (rest in)))
        (t (error "Invalid Key - Value syntax: missing colon."))))

;; Removes } when no more members need to be parsed in an object.
(defun remove-closed-obj (in)
  (cond ((closed-obj-p in) (rest in))
        (t (error "Invalid JSON syntax: Missing }."))))

;; Removes ] when no more elements need to be parsed in an array.
(defun remove-closed-array (in)
  (cond ((closed-array-p in) (rest in))
        (t (error "Invalid JSON syntax: Missing ]."))))

;; Removes the last element of a list.
(defun remove-last (lst)
  (reverse (rest (reverse lst))))


;; Converts a 1-element list of chars into a character.
(defun list-to-single-char (lst)
  (if (= (length lst) 1)
      (coerce (coerce lst 'string) 'character)
    (first lst)))



 

;; json-get
;; From a JSON object and a series of fields, returns the corresponding value.
(defun json-get (obj &rest fields)
  (get-element obj fields))


(defun get-element (obj fields)
  (cond 
   ((null obj) (error "Element not found."))

   ;; If the fields arg nil, return the entire gotten object.
   ((null fields) obj)

   ((and (numberp (first fields)) (< (first fields) 0))
    (error "Index numbers should be 0 or greater."))

   
   ;; If the value is a JSON object, recall without 'json-obj.
   ((obj-q-p (first obj))
    (get-element (rest obj) fields))


   ;; If fields' a number and the value is an array, recall without 'json-array.
   ((and (numberp (first fields)) (array-q-p (first obj)))
    (get-element (rest obj) fields))


   ;; If fields' a number and a possible index of the array, get the according item.
   ((and (numberp (first fields)) (< (first fields) (length obj)))
    (get-element (nth (first fields) obj) (rest fields)))

     
   ;; If the key = field, recall with the next field and the rest of the obj.
   ((not (numberp (first fields)))
    (if (string= (first fields) (key (first obj)))
        (get-element (val (first obj)) (rest fields))
      (get-element (rest obj) fields)))

   (t (error "Element not found."))))



;; Returns the key of a json object member.
(defun key (obj)
  (car obj))

;; Returns the value of a json object member.
(defun val (obj)
  (second obj))




;; I/O
;; json-load
;; Loads a text file from filename, returning its parsed representation.
(defun json-load (filename)
  (with-open-file (in filename
                      :direction :input
                      :if-does-not-exist :error)

    ;; Setting the initial-element to #\Null so that, if EOF is reached
    ;; prematurely, remove-trailing is able to remove unwanted characters.
    (multiple-value-bind (d) (make-string (file-length in) 
                                          :initial-element #\Null)
      (read-sequence d in)
      (json-parse d))))


;; Removes trailing characters at the end of a list 
;; like Newlines, spaces, ETX, etc...
(defun remove-trailing (lst)
  (cond ((char= (car (last lst)) #\ETX)
         (remove-trailing (remove-last lst)))

        ((char= (car (last lst)) #\Newline)
         (remove-trailing (remove-last lst)))

        ((char= (car (last lst)) #\Space)
         (remove-trailing (remove-last lst)))

        ((char= (car (last lst)) #\Null)
         (remove-trailing (remove-last lst)))

        (t lst)))



;; json-write
;; Converts the json object into a string and saves it as filename.
(defun json-write (obj filename)
  (with-open-file (out filename
                       :direction :output
                       :if-does-not-exist :create
                       :if-exists :supersede)
    (format out (coerce (stringify-json obj) 'string))))


;; Parses a json-obj into a string.
(defun stringify-json (obj)
  (cond 
   ((not (listp obj)) obj)

   ((array-q-p (first obj)) 
    (enclose-array (stringify-elements (rest obj))))

   ((obj-q-p (first obj))
    (enclose-obj (stringify-members (rest obj))))

   (t (error "Invalid JSON."))))


;; Parses an array and its elements into a string.
(defun stringify-elements (lst)
  (cond ((array-q-p (first lst))
         (stringify-elements (rest lst)))
        
        ;; If this is the last member
        ((null (rest lst))
         (cond ((not (listp (first lst)))
                (write-to-string (stringify-json (first lst))))
               (t (stringify-json (first lst)))))

        (t (stringify-element lst))))


;; Parses a single element into a string.
(defun stringify-element (e)
  (cond ((listp (first e))
         (concatenate 'string
                      (stringify-json (first e))
                      ", "
                      (stringify-elements (rest e))))
              
        (t (cond ((numberp (first e))
                  (concatenate 'string
                               (write-to-string (stringify-json (first e)))
                               ", "
                               (stringify-elements (rest e))))
                 (t (concatenate 'string
                                 (enclose-DQ (write-to-string 
                                               (stringify-json (first e))))
                                 ", "
                                 (stringify-elements (rest e))))))))


;; Parses an object memeber into a string.
(defun stringify-members (obj)
  (cond ((obj-q-p (first obj))
         (stringify-members (rest obj)))
        
        ;; If this is the last member, no need to concatenate.
        ((null (rest obj)) 
         (stringify-pair (first obj)))

        (t (concatenate 'string
                        (stringify-pair (first obj))
                        ", "
                        (stringify-members (rest obj))))))


;; Parses a pair, returning a string containing ("key" : value).
(defun stringify-pair (p)
  (cond ((numberp (stringify-json (val p)))
         (concatenate 'string 
                      (enclose-DQ (key p))
                      " : "
                      (write-to-string (stringify-json (val p)))))

        ((not (listp (val p)))
         (concatenate 'string 
                      (enclose-DQ (key p))
                      " : "
                      (write-to-string (stringify-json (val p)))))
         
        (t (concatenate 'string
                        (enclose-DQ (key p))
                        " : "
                        (stringify-json (val p))))))



;; Surroinds the element o with [ and ].
(defun enclose-array (a)
  (concatenate 'string "[" a "]"))


;; Surroinds the element o with { and }.
(defun enclose-obj (o)
  (concatenate 'string "{" o "}"))


;; Surrounds the element s with 2 double quotes.
(defun enclose-DQ (s)
  (concatenate 'string (string #\") s (string #\")))


                                                                                            


;;;; end of file - json-parsing.lisp