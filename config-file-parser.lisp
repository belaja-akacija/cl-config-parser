;;;; A little parser for a config file

;;; should read in commands from the CLI

;;; preliminary file opening to read into the parser, and some utils
(defparameter *file* (uiop:read-file-lines ".testconfigrc"))

(defun get-file-line (f ln)
  (assert (<= ln (length f))
          (ln)
          "Invalid index.")
  (if (= 0 ln)
      (car f)
      (get-file-line (cdr f) (1- ln))))
(length *file*)

(defun get-char-code-l (l)
  (if (null l)
      '()
      (cons (char-code (coerce (car l) 'character)) (get-char-code-l (cdr l)))))


(defun combine-list (l1 l2)
  (if (null l1)
      '()
      (cons (car l1) (cons (car l2) (combine-list (cdr l1) (cdr l2))))))

(defun plist-member (token plist)
  (member token plist))

;;; tree stuff

;;setters

(defun make-bin-tree-leaf (e)
  (list e))

(defun make-bin-tree-node (e b1 b2)
  (list e b1 b2))

;; getters

(defun get-bin-tree-leaf-element (l)
  (car l))

(defun get-bin-tree-node-element (n)
  (first n))

(defun get-bin-tree-node-left (n)
  (second n))

(defun get-bin-tree-node-right (n)
  (third n))

;; recognizers

(defun bin-tree-leaf-p (b)
  (and (listp b)
       (= (list-length b) 1)))

(defun bin-tree-node-p (b)
  (and (listp b)
       (= (list-length b) 3)))

;;; tokenization phase

(defvar letter-list
  '("A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z" "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y"))

(defvar symbol-list
  '("." ":" ";" "(" ")" "\'" "\"" "#" "@" "+" "|" "`" "[" "{" "&" "=" "}" "]" "*" "!" "%" "$" "~" "\\" "-" "_" "/" "?" "^"))

(defvar digit-list '(1 2 3 4 5 6 7 8 9 0))

(defvar letter-plist (combine-list (get-char-code-l letter-list) letter-list))

(defvar symbols-plist (combine-list (get-char-code-l symbol-list) symbol-list))

;;; Grammar classes

(defclass identifier ()
  ((id :accessor get-identifier-id
       :initarg :id)))

(defclass _add ()
  ((left :accessor get-add-left
         :initarg :left)
   (right :accessor get-add-right
          :initarg :right)))

(defclass _mult ()
  ((left :accessor get-mult-left
         :initarg :left)
   (right :accessor get-mult-right
          :initarg :right)))

(defclass _keyword ()
  ((id :accessor get-keyword-id
       :initarg :id)
   (pointer :accessor get-keyword-pointer
            :initarg :pointer)))

;; constructors

(defun make-identifier (value)
   (make-instance 'identifier :id value))


;;; Print methods to visualize the AST. Prints it in evaluable format :)
(defgeneric print-class (instance type)
  (:documentation "Pretty prints the class' tree structure in S-expression format"))

(defmethod print-class ((instance identifier) (type symbol))
  (declare (ignorable type))
  (get-identifier-id instance))

(defmethod print-class ((instance _add) (type symbol))
  (list '+
        (print-class (get-add-left instance) (type-of (get-add-left instance)))
        (print-class (get-add-right instance) (type-of (get-add-right instance)))))

(defmethod print-class ((instance _mult) (type symbol))
  (list '*
        (print-class (get-mult-left instance) (type-of (get-mult-left instance)))
        (print-class (get-mult-right instance) (type-of (get-mult-right instance)))))

(defmethod print-class ((instance _keyword) (type symbol))
  (list
    (get-keyword-id instance)
    (print-class (get-keyword-pointer instance) (type-of (get-keyword-pointer instance)))))


;;; Some test parsing functions as a proof of concept
(defun parse-letter (obj)
  (member (char-code obj) letter-plist))

(defun parse-word (stream)
  (let ((cursor (read-char stream)))
   (if (parse-letter cursor)
       (cons cursor (parse-word stream))
       '())))
