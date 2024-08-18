;;;; A little parser for a config file

;;; should read in commands from the CLI

;; preliminary file opening to read into the parser and utils
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

(defparameter x (make-string-input-stream "123 foo bar"))

;; figure out how to create tokens (x) and create trees (✓) of pass and fail branches
(defun %test-token (stream)
  (let* ((x (gensym))
        (c (read-char stream nil x)))
    (cond
      ((equal c x)
       (make-bin-tree-node (make-bin-tree-leaf 'end)
                           '()
                           '()))
      ((plist-member (char-code c) symbols-plist)
       (make-bin-tree-node (make-bin-tree-leaf c)
                           (%test-token stream)
                           '()))
      ((plist-member (char-code c) letter-plist)
       (make-bin-tree-node (make-bin-tree-leaf c)
                           (%test-token stream)
                           '()))
      (t (make-bin-tree-node (make-bin-tree-leaf c)
                             '()
                             (%test-token stream))))))

(defun plist-member (token plist)
  (member token plist))

(defparameter y (make-string-input-stream "abc 123 +[{ +1a{4"))
(%test-token y)


(member (char-code #\c) letter-plist)


(defun %integer (stream)
  )

(read-char x)
