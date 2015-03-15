D,#TD1PsT[Begin using 006 escapes](0 0 (NIL 0) (NIL NIL NIL) "CPTFONT")(1 0 (NIL 0) (NIL :BOLD-EXTENDED NIL) "CPTFONTB")0;;; -*- Package: ZWEI; Syntax: Common-Lisp; Fonts: (CPTFONT CPTFONTB); -*-

(defvar si:*force-defvar-init* nil)


(DEFMACRO BP-CH-CHAR (BP)
  "Return the character after BP, ignoring bits and font attributes."
  `(MAKE-CHAR (BP-CHAR ,BP)))

(DEFUN BACKWARD-OVER-PACKAGE-PREFIX (BP)
  "Return a bp to the start of the package prefix before BP, or NIL if none."
  (LET (BP3 BP4)
    (SETQ BP3 (BACKWARD-OVER *WHITESPACE-CHARS* BP)
	  BP4 (FORWARD-WORD BP3 -1 T))
    (AND (CHAR-EQUAL (BP-CHAR-BEFORE BP3) #\:)
	 (BEG-LINE-P BP4)
	 BP4))) 


(DEFUN DEFUN-INTERVAL (BP &OPTIONAL (TIMES 1) FIXUP-P (COMMENTS-P T) (TOP-BLANK-P NIL))
  "Return an interval surrounding the defun that BP is within, or NIL.
If TIMES is > 1, includes additional defuns after that one.
COMMENTS-P non-NIL means include comments before the defun.
TOP-BLANK-P non-NIL along with COMMENTS-P means
 include one blank line (if any) before anything else.
The second value is the line which opens the list structure of
 the defun that the interval contains."
  (DECLARE (VALUES INTERVAL DEFINITION-LINE))
  (PROG (BP1 BP2 BP3 BP4 SBP)
	(COND ((NULL (SETQ BP1 (FORWARD-DEFUN BP -1)))
	       (SETQ BP1 (BEG-LINE BP 0))
	       (COND ((= (LIST-SYNTAX (BP-CHAR BP1)) LIST-OPEN)
		      (GO BUFBEG1))
		     (T
		      (GO BUFBEG)))))
	(OR (SETQ BP2 (FORWARD-SEXP BP1))
	    (IF (NOT FIXUP-P)
		(RETURN NIL)
		(SETQ BP2 (BEG-LINE (BACKWARD-OVER-COMMENT-LINES (FORWARD-DEFUN BP1 1 T) TOP-BLANK-P)
				    -1))))
	(OR (BP-< (END-LINE BP2) BP)
	    ;; We were in the middle of the defun.
	    (GO FOUND))
	(SETQ BP BP1)
     BUFBEG
	(COND ((NULL (SETQ BP1 (FORWARD-DEFUN BP)))
	       (AND BP2
		    (SETQ BP1 (FORWARD-DEFUN BP2 -1))
		    (GO FOUND))			;At end of buffer, take previous
	       (RETURN NIL)))
     BUFBEG1
	(OR (SETQ BP2 (FORWARD-SEXP BP1))
	    (RETURN NIL))
     FOUND
	;; At this point, BP1 and BP2 surround a "defun".  Now we should grab any
	;; comment lines and intervening blank lines before the beginning, and the
	;; rest of the last line.
	(AND (> TIMES 1)
	     (SETQ BP2 (FORWARD-SEXP BP2 (1- TIMES) T)))
	(SETQ SBP BP1)				;Save real starting line
     CONTIN
	(AND COMMENTS-P
	     (SETQ BP1 (BACKWARD-OVER-COMMENT-LINES BP1 TOP-BLANK-P NIL)))
	(SETQ BP3 (FORWARD-OVER *BLANKS* BP2))
	(AND BP3 (OR (= (LIST-SYNTAX (BP-CHAR BP3)) LIST-COMMENT)
		     (CHAR= (BP-CH-CHAR BP3) #\NEWLINE))
	     (SETQ BP2 (BEG-LINE BP2 1 T)))
	;; Now try to find any extra close-parens because of a LOCAL-DECLARE
	(SETQ BP3 (FORWARD-OVER '(#\)) BP2))
	(AND (NOT (BP-= BP2 BP3))
	     (SETQ BP4 (FORWARD-SEXP BP3 (- TIMES)))
	     (BP-< BP4 BP1)
	     (SETQ BP1 BP4
		   BP2 BP3)
	     (GO CONTIN))
	;; Now try to find a package prefix
	(SETQ BP3 (BACKWARD-OVER-PACKAGE-PREFIX BP1))
	(WHEN BP3
	  (SETQ BP1 BP3)
	  (GO CONTIN))
	(RETURN (VALUES (CREATE-INTERVAL BP1 BP2) SBP)))) 


(defun read-until-eof (stream)
"Given a stream this function returns a list of all of the forms that it reads
from the stream until it gets to the end of file.
"
  (let ((sexpr (read stream nil :Eof)))
       (if (equal sexpr :Eof) nil (Cons sexpr (read-until-eof stream)))))

(defun read-form-or-forms-from-buffer (&optional (section-p nil))
"This function reads either a form or a collection of forms from the current
buffer.  If a section has been marked out then forms are read from this,
otherwise a form is read from after the cursor.  If only one form is found
then this is returned.  If more than one form is found then a list containing
these forms is returned.
"
  (if (or (Window-Mark-P *Window*) section-p)
      (let ((bp1 (mark))
	    (bp2 (point))
	    (defun-name nil))
	   (or (bp-< bp1 bp2) (psetq bp1 bp2 bp2 bp1))
	   (if (bp-= (forward-over *whitespace-chars* (mark))
		     (forward-over *whitespace-chars* (point)) )
	       (setq *mark-stays* nil)
	       (setq defun-name "Region"))
	   (cond (defun-name)
		 ((setq bp1 (defun-interval (beg-line (point)) 1 () ()))
		  (setq bp2 (interval-last-bp bp1) bp1 (interval-first-bp bp1))
		  (setq si:*force-defvar-init* t))
		 (t (barf "Unbalanced parentheses")))
	   (let ((stream (Interval-Stream bp1 bp2 t)))
	        (unwind-protect
		  (let ((all-sexprs (read-until-eof stream)))
		       (values (if (equal (length all-sexprs) 1)
				   (first all-sexprs)
				   all-sexprs)
			       (if defun-name
				   defun-name
				   (send (array-leader (bp-line bp1) 5) :Name))))
		  (close stream))))
      (let ((stream (rest-of-interval-stream (point))))
	   (unwind-protect
	     (let ((sexpr (read stream nil :Eof)))
	          (values (if (equal :Eof sexpr) :Nothing-found sexpr)
			  "Expression"))
	     (close stream)))))

(defun padding-with (start padchar form stream)
  (let ((xpr (cl:Coerce 
	       (Format nil "~% ~A ~S" start form)
	       'List)))
    (Loop :for (p c) :on xpr
	  #:if (Eql p #\Newline)
	  :collect padchar :into ans
	  #:else
	  :collect (or c #\Newline) into ans
	  :finally (format stream 
			   "~&~A"
			   (cl:Coerce ans 'String)))))

(defun split-with (char string)
  (loop :for start := 0 :then (and end (1+ end))
	:for end := (cl:position char string) :then (and start (cl:position char string :start (1+ start)))
	:while start
	:collect (cl:subseq string start end)))

(defun padding-with-simple (start string stream)
  (format stream
	  (format nil "~~{~A ~~A~~%~~}" start)
	  (split-with #\Newline string)))

(defcom com-evaluate-and-insert-into-buffer
	" Evaluate the next or selected s-expression and insert the result into the
 buffer before the original expression."
	()
  (com-backward-sexp)
  (let ((form (read-form-or-forms-from-buffer))
	(out nil))
    (if (equal form :Nothing-Found)
	(barf "Unbalanced parentheses or no Form."))
    ;; (setq form (si:eval-abort-trivial-errors form)) ; si:eval1 internally.
    (setq out
	  (with-output-to-string (cl:*standard-output*)
	    (setq form (eval form))))
    (let ((stream (rest-of-interval-stream 
		    (progn
		      (com-forward-sexp)
		      (point)))))
      ;; print output
      (unless (string= "" out)
	(cl:fresh-line stream)
	(padding-with-simple ";>>" out stream))
      ;; evaluate result
      (padding-with "=>" #\; form stream)
      (move-bp (point) (send stream :read-bp))))
  dis-text)

'(set-comtab *standard-comtab* 
	      '(#\control-shift-j com-evaluate-and-insert-into-buffer)
	      (make-command-alist '(com-evaluate-and-insert-into-buffer)))

