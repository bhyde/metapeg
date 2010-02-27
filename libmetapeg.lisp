(in-package :metapeg)
(declaim (optimize (speed 3) (safety 0) (debug 0)))

; Global variables used during the parse
; --------------------

(defvar *actions*)

(defvar *rules* nil)
(defvar *input* nil)

(defvar *context* nil)
(defclass context ()
  ((parent :accessor parent :initform nil)
   (rule :accessor rule :initform nil)
   (children :accessor children :initform nil)
   (value :accessor value :initform nil)
   (start-index :initarg :start-index :accessor start-index :initform nil)
   (end-index :accessor end-index :initform nil)))

(defmethod print-object ((obj context) stream)
  (format stream "context ~A ~S val ~S ~A ~A" (rule obj) (children obj) (value obj) (start-index obj) (end-index obj)))

(defun clone-ctx (ctx rule)
   (let ((new-ctx (make-instance 'context)))
    (setf (parent new-ctx) ctx)
    (setf (rule new-ctx) rule)
    (setf (start-index new-ctx) (end-index ctx))
    new-ctx))

(defun ctx-failed-p (ctx) (null (end-index ctx)))
(defun succeed (ctx value start-index end-index)
  (setf (value ctx) value)
  (setf (start-index ctx) start-index)
  (setf (end-index ctx) end-index)
;  (format t "succeed ~A ~A ~A~%" (rule ctx) (start-index ctx) (end-index ctx))
  ctx)

(defun fail ()
  (let ((ctx (make-instance 'context)))
    (setf (rule ctx) 'fail)
    (setf (value ctx) (rule *context*))
;    (format t "fail ~A ~A ~A~%" (rule *context*) (start-index *context*) (end-index *context*))
    ctx))


; Utility functions
; --------------------

(defun make-name (string)
  (intern (concatenate 'string "parse_" string)
          (symbol-package 'this-package)))


(defun fix-escapes2 (char-list)
  (do ((out nil)
       (remaining char-list))
      ((null remaining) (reverse out))
    (let ((c (first remaining)))
;      (printf "rem ~s~n" remaining)
      (if (char= c #\\)
	  (let ((nextc (second remaining)))
	    (setf out (cons (case nextc
			      ((#\n) #\newline)
			      ((#\t) #\tab)
			      (otherwise nextc))
			    out))
	    (setf remaining (cdr (cdr remaining))))
	  (progn 
	    (setf out (cons c out))
	    (setf remaining (cdr remaining)))))))

(defun fix-escapes (list) (fix-escapes2 list))
; filter out the first part of pair, useful for patterns where we specify a negative match (eg (!"x" .)*)
(defun zip-second (pair-list)
  (loop for x in pair-list collect (second x)))

(defvar *build-with-tracing* nil)

(defmacro build-parser-function (name parser)
  (if *build-with-tracing*
      `(let* ((*context* (clone-ctx *context* ,name))
              (result (funcall ,parser offset)))
         (format t "~&~vT> ~A at ~D" offset ',name offset)
         (prog1
             (if (ctx-failed-p result)
                 (fail)
                 (succeed *context* (value result) (start-index result) (end-index result)))
           (format t "~&~vT<~A ~A" offset ',name (if (ctx-failed-p result) ":<" ":)"))))
      `(let* ((*context* (clone-ctx *context* ,name))
              (result (funcall ,parser offset)))
         (if (ctx-failed-p result)
             (fail)
             (succeed *context* (value result) (start-index result) (end-index result))))))

(defun make-call-rule-closure (rule)
  `#'(lambda (offset)
       (let ((pair (assoc ',rule *rules*)))
	 (if pair
	     (funcall (cadr pair) offset)
	     (error "missing rule ~A" ',rule))) ))

(defun make-call-rule-closure2 (rule)
  `#'(lambda (offset)
       (let ((pair (assoc ',rule *rules*)))
	 (if pair
	     (funcall (cadr pair) offset)
	     (error "missing rule ~A" ',rule))) ))

(defun call-rule (rule)
  (make-call-rule-closure rule))

(defun char-list-to-string (char-list)
  (reduce #'(lambda (a b) (concatenate 'string a (string b))) char-list :initial-value ""))

(defun emit-actions (stream actions)
  (loop for (sym string) in actions
	  do (format stream "~%(defun ~S (data) ~A )" sym string)))

; takes the name of the parser file to be created, the grammar and the existing bootstrap parser
(defun create-parser (new-parser-file-name grammar parser)
  (multiple-value-bind (form actions) (parse grammar parser)
    (with-open-file (stream new-parser-file-name :direction :output :if-exists :supersede)
      (let ((*print-readably* t)
	    (*print-pretty* t)
	    (*print-circle* nil))
	(loop for aform in form do
	      (prin1 aform stream)
	      do (format stream "~%"))
	(format stream "~% ")
	(emit-actions stream actions)))
    t))

; parsing combinator functions
; --------------------

; I have found remarkably elegant recursive versions of these combinators
; but this comment block is too small to note them

(defun either (&rest parsers)
  #'(lambda (offset)
      (block b1
	(let ((*context* (clone-ctx *context* 'mp_either)))
	  (loop for p in parsers
		do (let ((result (funcall p offset)))
		     (if (not (ctx-failed-p result)) 
			 (progn
			   (return-from b1 (succeed *context* (value result) offset (end-index result))))))))
	(fail))))


(defun optional (parser) #'(lambda (offset)
			     (let ((*context* (clone-ctx *context* 'mp_optional)))
			       (let ((result (funcall parser offset)))
				 (if (ctx-failed-p result)
				     (succeed *context* 'optional offset offset)
				     (succeed *context* (value result) offset (end-index result)))))))

(defun follow (parser) #'(lambda (offset)
			   (let ((*context* (clone-ctx *context* 'mp_follow)))
			     (let ((result (funcall parser offset)))
			       (if (ctx-failed-p result)
				   (fail)
				   (succeed *context* (value result) offset offset)))))) ;don't consume input

(defun many (parser) #'(lambda (offset)
			 (block b1
			   (let ((*context* (clone-ctx *context* 'mp_many))
				 (start-offset offset)
				 children)
			     (loop do
				   (let ((result (funcall parser offset)))
				     (if (end-index result)
					 (progn
					   (push (value result) children)
					   (setf offset (end-index result)))
					 (return-from b1 (succeed *context* (reverse children) start-offset offset)))))))))


(defun many1 (parser) #'(lambda (offset)
			  (let ((*context* (clone-ctx *context* 'mp_many1)))
			    (let ((result (funcall parser offset)))
			      (if (end-index result)
				(let ((result2 (funcall (many parser) (end-index result))))
				  (if (end-index result2)
				      (succeed *context* (cons (value result) (value result2)) offset (end-index result2))
				      (succeed *context* (value result) offset (end-index result))))
				(fail))))))


(defun seq (&rest parsers)
  #'(lambda (offset)
      (block b1
	(assert (> (length parsers) 0))
	(let ((*context* (clone-ctx *context* 'mp_seq))
	      (start-offset offset)
	      child-values
	      child-nodes)
	  ; run the parsers
	  (loop for p in parsers do
		(if (not (listp p))
		    (let ((result (funcall p offset)))
		      (if (end-index result)
			  (progn
			    (push result child-nodes)
			    (push (value result) child-values)
			    (setf offset (end-index result))
			    (setf (children *context*) (reverse child-nodes)))
			  (return-from b1 (fail))))
		    (progn
		      (push (succeed (clone-ctx *context* 'action) nil offset offset)  child-nodes)
		      (push p child-values)
		      (setf (children *context*) (reverse child-nodes))))
		finally (return (succeed *context* (reverse child-values) start-offset offset)))))))

; non-portable use of bounding exception, should check input length instead

(defun match-string (string)
  #'(lambda (offset)
      (handler-case
	  (if (string= string (subseq *input* offset (+ offset ( length string))))
	      (let ((*context* (clone-ctx *context* 'mp_string)))
		(succeed *context* string offset (+ offset (length string))))
	      (fail))
	(#+sbcl SB-KERNEL:BOUNDING-INDICES-BAD-ERROR #+ccl simple-error () (fail)))))

(defun match-char (char-list)
  #'(lambda (offset)
      (handler-case
	  (block b1
	    (loop for char in char-list do
		  (progn
		    (setf char (if (stringp char)
				 (elt char 0)
				 char))
		    (if (char= char (elt *input* offset))
			(return-from b1 (succeed (clone-ctx *context* 'mp_char) char offset (+ offset 1))))))
		  
;	    (format t "match char dropped through ~S~%" char-list)
	    (fail))
	(#+sbcl SB-KERNEL:BOUNDING-INDICES-BAD-ERROR #+ccl simple-error ()
                (fail))
	(#+sbcl SB-KERNEL::INDEX-TOO-LARGE-ERROR #+ccl simple-error ()
                (fail))
        #+ccl(CCL::SEQUENCE-INDEX-TYPE-ERROR ()
               (fail)))))


(defun match-any-char (ignored)
  (declare (ignore ignored))
  #'(lambda (offset)
      (handler-case
	  (succeed (clone-ctx *context* 'mp_anychar) (elt *input* offset) offset (+ offset 1))
	(#+sbcl SB-KERNEL:BOUNDING-INDICES-BAD-ERROR #+ccl simple-error ()
                (fail))
        #+ccl(CCL::SEQUENCE-INDEX-TYPE-ERROR ()
               (fail)))))

(defun match-any-char2 (ignored)
  (declare (ignore ignored))
  #'(lambda (offset)
      (handler-case
	  (succeed (clone-ctx *context* 'mp_anychar) (elt *input* offset) offset (+ offset 1))
	(#+sbcl SB-KERNEL:BOUNDING-INDICES-BAD-ERROR #+ccl simple-error ()
                (fail))
        #+ccl(CCL::SEQUENCE-INDEX-TYPE-ERROR ()
               (fail)))))

(defun negate (parser)
  #'(lambda (offset)
      (let ((*context* (clone-ctx *context* 'mp_negate)))
	(let ((result (funcall parser offset)))
	  (if (ctx-failed-p result)
	    (succeed *context* 'negate offset offset) ;note we return a parse result but don't advance input
	    (fail))))))

(defun find-match ( original-ctx examine-ctx rule-name offset)
    (if (null examine-ctx)
	(succeed (clone-ctx original-ctx rule-name) "" offset offset)
	(let ((siblings (children examine-ctx)))
;	  (format t "siblings are ~S~%" siblings)
	  (loop for sibling in siblings
		do (if (and (typep sibling 'context) 
			    (stringp (rule sibling))
			    (string= (rule sibling) rule-name))
		       (progn
;			 (format t "comparing ~A ~A at ~A~%" rule-name sibling offset)
			 (let* ((ms (subseq *input* (start-index sibling) (end-index sibling)))
				(failed (null (end-index (funcall (match-string ms) offset)))))
;			   (format t "match string ~S failed ~A~%" ms failed)
			   (return-from find-match (if failed
						       (fail)
						       (succeed (clone-ctx original-ctx rule-name) ms offset (+ offset (length ms)))))))
		       
		     ))
	  (find-match original-ctx (parent examine-ctx) rule-name offset))))

(defun match (rule-name)
  #'(lambda (offset)
      (find-match *context* (parent *context*) rule-name offset)))

(defun read-file (filename)
  (with-open-file (file filename :direction :input)
    (let ((s (make-string (file-length file))))
      (read-sequence s file)
      s)))

(defvar *action-name-counter* 0)

(defun gen-action-name ()
  (intern (format nil "METAPEG-ACTION~A" (incf *action-name-counter*))
          (symbol-package 'this-package)))

(defvar *cached-parser-file-name* nil)
(defvar *cached-parser-file-write-date* nil)
; returns the parse tree and the content of the *actions* variable
(defun parse (input-file parser-file)
  (let ((input (read-file input-file)))
    (parse-string input parser-file)))

(defun parse-string (input parser-file)
  (let ((*input* input)
	(*actions* nil))
    (if (and (equal *cached-parser-file-name* parser-file) ; this breaks if the user changes directories
	     (equal *cached-parser-file-write-date* (file-write-date parser-file)))
	(progn
	  'dont-load-it-again
	  )
	(progn
          (setf *cached-parser-file-name* nil) ; in moment the cache breaks.
	  (load parser-file)
	  (setf *cached-parser-file-name* parser-file)
	  (setf *cached-parser-file-write-date* (file-write-date parser-file))
	  ))
    (let ((result (generated-parser)))
      (if (not (ctx-failed-p result))
	  (if (= (length *input*)  (end-index result))
	      (values (transform (value result)) *actions*)
	      (progn  (cerror "Continue" "Parse only parsed up to index ~D, \"~A\"" (end-index result) (subseq *input* (end-index result)))
		      (values *actions*)))
	  (values result *actions*)))))

(defun transform (tree)
  (if tree
      (if (listp tree)
	  (if (equal (first tree) 'action)
	      tree
	      (let ((data (mapcar #'transform tree)))
		(progn (loop for el in data
			  when (and (listp el)
				    (equal (first el) 'action)
				    (symbolp (third el)))
			  do (progn
;			       (format *error-output* "data is ~s~%" data)
			       (handler-case
				   (return-from transform (funcall (third el) data))
				 (undefined-function (e)
				   (progn  (format *error-output* "missing definition for ~S ~A~%" (third el) e)
					   tree)))))
		       data)))
	  tree)))

#|

;;; Example of how to rebootstrap the metapeg parser.

(let ((*package* (find-package "METAPEG")) (metapeg::*action-name-counter* 319))
  (metapeg:create-parser "/tmp/metapeg.lisp" "metapeg.peg" "metapeg.lisp"))

|#
