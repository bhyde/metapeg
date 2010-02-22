(IN-PACKAGE :METAPEG)
(DECLAIM (OPTIMIZE (SPEED 3) (SAFETY 0) (DEBUG 0)))
(DEFUN METAPEG::GENERATED-PARSER ()
  (LET ((METAPEG::*CONTEXT* (MAKE-INSTANCE 'METAPEG::CONTEXT :START-INDEX 0)))
    (FUNCALL (METAPEG::|parse_program|) 0)))
(DEFUN |parse_program| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "program"
                                    (METAPEG::SEQ (METAPEG::MANY (|parse_ws|))
                                                  (METAPEG::MANY1
                                                   (|parse_rule|))
                                                  (LIST 'METAPEG::ACTION NIL
                                                        '|metapeg_action320|)))))
(DEFUN |parse_rule| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "rule"
                                    (METAPEG::SEQ (|parse_id|)
                                                  (METAPEG::MANY (|parse_ws|))
                                                  (METAPEG::MATCH-STRING "<-")
                                                  (METAPEG::MANY (|parse_ws|))
                                                  (|parse_ordered-expr-list|)
                                                  (METAPEG::MANY
                                                   (|parse_ws_or_nl|))
                                                  (LIST 'METAPEG::ACTION NIL
                                                        '|metapeg_action321|)))))
(DEFUN |parse_ordered-expr-list| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "ordered-expr-list"
                                    (METAPEG::EITHER
                                     (METAPEG::SEQ (|parse_expr-list|)
                                                   (METAPEG::MANY (|parse_ws|))
                                                   (METAPEG::MATCH-STRING "/")
                                                   (METAPEG::MANY (|parse_ws|))
                                                   (|parse_ordered-expr-list|)
                                                   (LIST 'METAPEG::ACTION NIL
                                                         '|metapeg_action322|))
                                     (METAPEG::SEQ (|parse_expr-list|)
                                                   (LIST 'METAPEG::ACTION NIL
                                                         '|metapeg_action323|))))))
(DEFUN |parse_expr-list| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "expr-list"
                                    (METAPEG::SEQ (|parse_expr|)
                                                  (METAPEG::MANY
                                                   (METAPEG::SEQ
                                                    (METAPEG::MANY1
                                                     (|parse_ws|))
                                                    (|parse_expr-list|)))
                                                  (LIST 'METAPEG::ACTION NIL
                                                        '|metapeg_action324|)))))
(DEFUN |parse_expr| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "expr"
                                    (METAPEG::EITHER
                                     (METAPEG::SEQ (|parse_simple-expr|)
                                                   (METAPEG::MATCH-STRING "*")
                                                   (LIST 'METAPEG::ACTION NIL
                                                         '|metapeg_action325|))
                                     (METAPEG::SEQ (|parse_simple-expr|)
                                                   (METAPEG::MATCH-STRING "+")
                                                   (LIST 'METAPEG::ACTION NIL
                                                         '|metapeg_action326|))
                                     (METAPEG::SEQ (|parse_simple-expr|)
                                                   (METAPEG::MATCH-STRING "?")
                                                   (LIST 'METAPEG::ACTION NIL
                                                         '|metapeg_action327|))
                                     (METAPEG::SEQ (|parse_simple-expr|)
                                                   (LIST 'METAPEG::ACTION NIL
                                                         '|metapeg_action328|))))))
(DEFUN |parse_simple-expr| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "simple-expr"
                                    (METAPEG::EITHER
                                     (METAPEG::SEQ (|parse_string|)
                                                   (LIST 'METAPEG::ACTION NIL
                                                         '|metapeg_action329|))
                                     (|parse_action|)
                                     (METAPEG::SEQ (METAPEG::MATCH-STRING "&")
                                                   (|parse_simple-expr|)
                                                   (LIST 'METAPEG::ACTION NIL
                                                         '|metapeg_action330|))
                                     (METAPEG::SEQ (METAPEG::MATCH-STRING "@")
                                                   (|parse_id|)
                                                   (LIST 'METAPEG::ACTION NIL
                                                         '|metapeg_action331|))
                                     (METAPEG::SEQ (|parse_id|)
                                                   (LIST 'METAPEG::ACTION NIL
                                                         '|metapeg_action332|))
                                     (METAPEG::SEQ (|parse_bracketed-rule|)
                                                   (LIST 'METAPEG::ACTION NIL
                                                         '|metapeg_action333|))
                                     (METAPEG::MATCH-STRING "!.")
                                     (METAPEG::SEQ (METAPEG::MATCH-STRING "!")
                                                   (|parse_expr|)
                                                   (LIST 'METAPEG::ACTION NIL
                                                         '|metapeg_action334|))
                                     (METAPEG::SEQ (|parse_character-class|)
                                                   (LIST 'METAPEG::ACTION NIL
                                                         '|metapeg_action335|))
                                     (METAPEG::SEQ (METAPEG::MATCH-STRING ".")
                                                   (LIST 'METAPEG::ACTION NIL
                                                         '|metapeg_action336|))))))
(DEFUN |parse_bracketed-rule| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "bracketed-rule"
                                    (METAPEG::EITHER
                                     (METAPEG::MATCH-STRING "()")
                                     (METAPEG::SEQ (METAPEG::MATCH-STRING "(")
                                                   (METAPEG::MANY (|parse_ws|))
                                                   (|parse_ordered-expr-list|)
                                                   (METAPEG::MANY (|parse_ws|))
                                                   (METAPEG::MATCH-STRING ")")
                                                   (LIST 'METAPEG::ACTION NIL
                                                         '|metapeg_action337|))))))
(DEFUN |parse_id| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "id"
                                    (METAPEG::SEQ
                                     (METAPEG::MANY1
                                      (METAPEG::MATCH-CHAR
                                       '(#\A #\B #\C #\D #\E #\F #\G #\H #\I
                                         #\J #\K #\L #\M #\N #\O #\P #\Q #\R
                                         #\S #\T #\U #\V #\W #\X #\Y #\Z #\a
                                         #\b #\c #\d #\e #\f #\g #\h #\i #\j
                                         #\k #\l #\m #\n #\o #\p #\q #\r #\s
                                         #\t #\u #\v #\w #\x #\y #\z #\- #\_)))
                                     (LIST 'METAPEG::ACTION NIL
                                           '|metapeg_action338|)))))
(DEFUN |parse_character-class| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "character-class"
                                    (METAPEG::SEQ (METAPEG::MATCH-STRING "[")
                                                  (METAPEG::MANY1
                                                   (METAPEG::SEQ
                                                    (|parse_not_right_bracket|)
                                                    (METAPEG::MATCH-ANY-CHAR
                                                     'METAPEG::DUMMY)))
                                                  (METAPEG::MATCH-STRING "]")
                                                  (LIST 'METAPEG::ACTION NIL
                                                        '|metapeg_action339|)))))
(DEFUN |parse_string| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "string"
                                    (METAPEG::SEQ (METAPEG::MATCH-CHAR '(#\"))
                                                  (METAPEG::MANY
                                                   (METAPEG::SEQ
                                                    (METAPEG::NEGATE
                                                     (METAPEG::MATCH-CHAR
                                                      '(#\")))
                                                    (METAPEG::MATCH-ANY-CHAR
                                                     'METAPEG::DUMMY)))
                                                  (METAPEG::MATCH-CHAR '(#\"))
                                                  (LIST 'METAPEG::ACTION NIL
                                                        '|metapeg_action340|)))))
(DEFUN |parse_action| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "action"
                                    (METAPEG::SEQ (METAPEG::MATCH-CHAR '(#\{))
                                                  (METAPEG::MANY
                                                   (METAPEG::SEQ
                                                    (METAPEG::NEGATE
                                                     (METAPEG::MATCH-CHAR
                                                      '(#\})))
                                                    (METAPEG::MATCH-ANY-CHAR
                                                     'METAPEG::DUMMY)))
                                                  (METAPEG::MATCH-CHAR '(#\}))
                                                  (LIST 'METAPEG::ACTION NIL
                                                        '|metapeg_action341|)))))
(DEFUN |parse_not_right_bracket| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "not_right_bracket"
                                    (METAPEG::NEGATE
                                     (METAPEG::MATCH-STRING "]")))))
(DEFUN |parse_ws| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "ws" (METAPEG::MATCH-CHAR '(#\  #\Tab)))))
(DEFUN |parse_nl| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "nl" (METAPEG::MATCH-CHAR '(#\Newline)))))
(DEFUN |parse_ws_or_nl| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "ws_or_nl"
                                    (METAPEG::EITHER (|parse_ws|)
                                                     (|parse_nl|)))))

 
(defun |metapeg_action341| (data)  
	(let ((action-name (gen-action-name)))
	 (push (list action-name (char-list-to-string (fix-escapes (zip-second (second data))))) *actions*)
	 `(list 'action nil ',action-name))
 )
(defun |metapeg_action340| (data)  `(match-string ,(char-list-to-string (zip-second (second data))))  )
(defun |metapeg_action339| (data)  `(match-char ',(fix-escapes2 (zip-second (second data))))  )
(defun |metapeg_action338| (data)  (char-list-to-string (first data))  )
(defun |metapeg_action337| (data)  (third data)  )
(defun |metapeg_action336| (data)  `(match-any-char 'dummy)  )
(defun |metapeg_action335| (data)  (first data)  )
(defun |metapeg_action334| (data)  `(negate ,(second data))  )
(defun |metapeg_action333| (data)  (first data)  )
(defun |metapeg_action332| (data)  
	`(,(make-name (first data)))
 )
(defun |metapeg_action331| (data)  `(match ,(second data))  )
(defun |metapeg_action330| (data)  `(follow ,(second data))  )
(defun |metapeg_action329| (data)  (first data)  )
(defun |metapeg_action328| (data)  (first data)  )
(defun |metapeg_action327| (data)  `(optional ,(first data))  )
(defun |metapeg_action326| (data)  `(many1 ,(first data))  )
(defun |metapeg_action325| (data)  `(many ,(first data))  )
(defun |metapeg_action324| (data)  (if (or (equal (second data) "") (null (second data)))
					     (first data)
					     (let ((tail (second (first (second data)))))
						  (if (equal (first tail) 'seq)
		   			              `(seq ,(first data) ,@(rest tail))
		   			              `(seq ,(first data) ,tail))))  )
(defun |metapeg_action323| (data)  (first data)  )
(defun |metapeg_action322| (data)  
(let ((tail (fifth data)))
	(if (equal (first tail) 'either)
	    `(either ,(first data) ,@(rest tail))
	    `(either ,(first data) ,(fifth data))))
 )
(defun |metapeg_action321| (data)  `(defun ,(make-name (first data)) ()
	 (lambda (offset)
	  (build-parser-function ,(first data) ,(fifth data))))  )
(defun |metapeg_action320| (data) 
`((in-package :metapeg)	
 (declaim (optimize (speed 3) (safety 0) (debug 0)))
  (defun generated-parser ()
	(let ((*context* (make-instance 'context :start-index 0)))
	   (funcall (|parse_program|) 0)))
	,@(second data))
 )