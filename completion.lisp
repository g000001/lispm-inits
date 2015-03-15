;;; -*- Mode: lisp; Syntax: Ansi-common-lisp; Lowercase: Yes; Package: CL-USER -*-

;;;; completion.lisp

;;; Copyright Rainer Joswig, joswig@lisp.de, 1995-2003, 2007, 2008, All rights reserved

;;; This file provides a completion facility for Symbolics Zmacs and MCL Fred.
;;; It is not very sophisticated, but it does useful work!

;;; To Do
;;;  Set the CCL package for the MCL symbols.

;;;; User interface for MCL Fred

;;; Just type c-i for completion of (external) symbols.

;;; Type c-i for
;;;     foo  looks in the window package    for "FOO"
;;; bar:foo  looks in the package "BAR"     for "FOO" if bar is a known package
;;; bar:foo  looks in all packages          for "FOO" if bar is a unknown package
;;;    :foo  looks in the package "KEYWORD" for "FOO"

;;; Type m-i for
;;;     foo  looks in all packages for "FOO"
;;; bar:foo  looks in all packages for "FOO"
;;;    :foo  looks in all packages for "FOO"


;;;; User interface for Symbolics Zmacs

;;; Just press COMPLETE (F11) for completion of symbols.
;;; The original functionality of completion in Zmacs ZWEI:COM-COMPLETE-DEFINITION-NAME
;;; (from file #p"sys:zwei;functions-buffers.lisp") is overwridden.

;;; Type COMPLETE for
;;;     foo  looks in the buffer package    for "FOO"
;;; bar:foo  looks in the package "BAR"     for "FOO" if bar is a known package
;;; bar:foo  looks in all packages          for "FOO" if bar is a unknown package
;;;    :foo  looks in the package "KEYWORD" for "FOO"

;;; Type c-u COMPLETE for
;;;     foo  looks in all packages for "FOO"
;;; bar:foo  looks in all packages for "FOO"
;;;    :foo  looks in all packages for "FOO"

;;; Type c-u c-u COMPLETE asks for a package and then for
;;;     foo  looks in this package for "FOO"
;;; bar:foo  looks in this package for "FOO"
;;;    :foo  looks in this package for "FOO"

;;; Runs in Genera
;;; Modified July 29 1995
;;; Modified August 20 1999     Now completes strings like "in-in" to "initialize-instance".
;;; Modified December 2007      Ported to Symbolics Genera



(cl:defpackage "RJ-COMPLETION"
  (:use :future-common-lisp))

(in-package "RJ-COMPLETION")


(defun split-string (string &key (item #\space) (test #'char=)
                            (one-char-splits-p t))
  "Splits the string into substrings at character item.
Returns a list of strings."
  (let ((len (length string))
	(index 0) result)
    (dotimes (i len (progn (unless (= index len)
			     (push (subseq string index) result))
                           (reverse result)))
      (when (funcall test (char string i) item)
	(unless (and (not one-char-splits-p) (= index i)) ;; two spaces in a row
	  (push (subseq string index i) result))
	(setf index (1+ i))))))

(defun break-up-string (string)
  (split-string string :item #\-))

(defun matching-string-lists (l1 l2)
  (cond ((and (null l1) (null l2)) t)
        ((null l2) nil)
        ((null l1) t)
        (t (and (starting-substring-p (first l1)
                                      (first l2)
                                      (length (first l1)))
                (matching-string-lists (rest l1)
                                       (rest l2))))))

(defun starting-substring-p (substring string length-of-substring)
  "Returns t if substring is a starting substring of string."
  (unless (> length-of-substring (length string))
    (string-equal substring string :end2 length-of-substring)))

#+ignore
'(defun matching-symbol-name-p (fragment symbol-name length-of-fragment)
  "Returns t if substring is a starting substring of string."
  (starting-substring-p fragment symbol-name length-of-fragment))

(defun matching-symbol-name-p (fragment symbol-name)
  (matching-string-lists fragment symbol-name))

; (matching-symbol-name-p (list "in" "i") (list "initialize" "instance"))

(defun find-completing-symbols-in-package (symbol-name-to-complete package)
  "Returns a list of completions for SYMBOL-NAME-TO-COMPLETE in package PACKAGE."
  (declare (type (or string symbol) symbol-name-to-complete))
  (declare (optimize (speed 3) (safety 3)))
  (setf symbol-name-to-complete (string-upcase symbol-name-to-complete))
  (let ((list-of-symbols nil)
        (string-list (break-up-string symbol-name-to-complete)))
    (do-symbols (symbol package list-of-symbols)
      (when (matching-symbol-name-p string-list
                                    (break-up-string (symbol-name symbol)))
        (push symbol list-of-symbols)))))


; (find-completing-symbols-in-package "MAKE-C" (find-package "COLOR"))

(defun find-all-completing-symbols (symbol-name-to-complete)
  "Returns a list of all completions for SYMBOL-NAME-TO-COMPLETE."
  (declare (type (or string symbol) symbol-name-to-complete))
  (declare (optimize (speed 3) (safety 3)))
  (setf symbol-name-to-complete (string-upcase symbol-name-to-complete))
  (let ((list-of-symbols nil)
        (string-list (break-up-string symbol-name-to-complete)))
    (do-all-symbols (symbol list-of-symbols)
      (when (matching-symbol-name-p string-list
                                    (break-up-string (symbol-name symbol)))
        (push symbol list-of-symbols)))))


(defun analyze-string-as-symbol (string default-package)
  "returns symbol and package part of a string analyzed as a symbol"
  (let* ((first-colon-position (position #\: string))
         (second-colon-position (if first-colon-position
                                  (position #\: string
                                            :start (1+ first-colon-position))
                                  nil)))
    (values (if first-colon-position
              (subseq string
                      (1+ (or second-colon-position first-colon-position))
                      (length string))
              string)
            (if first-colon-position
              (if (zerop first-colon-position)
                (find-package "KEYWORD")
                (find-package (subseq string 0 first-colon-position)))
              default-package)
	    (or (not first-colon-position)
		(and first-colon-position
		     (not second-colon-position))))))
  
(defun remove-some-characters-from-string (string)
  "returns: string from-left from-right"
  (let* ((string1 (string-left-trim '(#\# #\') string))
         (deleted-from-left (- (length string) (length string1)))
         (string2 (string-left-trim '(#\|) string1)))
    (incf deleted-from-left (- (length string1) (length string2)))
    (let* ((string3 (string-right-trim '(#\|) string2))
           (deleted-from-right (- (length string2) (length string3))))
      (values string3 deleted-from-left deleted-from-right))))


(defun find-completing-symbols (symbol-name-to-complete
                                &key
                                (default-package *package*)
                                (all-packages nil))
  "Returns a list of completions for SYMBOL-NAME-TO-COMPLETE."
  (declare (type string symbol-name-to-complete))
  (setf symbol-name-to-complete (string-upcase symbol-name-to-complete))
  (multiple-value-bind (symbol package exported-p)
                       (analyze-string-as-symbol symbol-name-to-complete
                                                 default-package)
    (when symbol
      (let ((symbols (if (or all-packages (not package))
			 (find-all-completing-symbols symbol)
			 (find-completing-symbols-in-package symbol package))))
	(if exported-p
	    (remove-if-not #'(lambda (symbol)
				   (or
				     (multiple-value-bind (symbol type)
					 (find-symbol (symbol-name symbol)
						      (symbol-package symbol))
				       (and symbol (eq type :external)))
				     #+genera  ; seems to have some strange packages
				     (let* ((*package* (find-package "COMMON-LISP"))
					    (colons (count #\: (princ-to-string symbol))))
				       (<= 0 colons 1))))
		       symbols)
	    symbols)))))
				    
#+genera
(defun complete-definition-name-internal ()
  (let* ((bp1 (zwei:forward-atom (zwei:forward-char (zwei:point) 1 t) -1 t))
	 (bp2 (zwei:forward-atom bp1)))
    (when bp2
      (multiple-value-bind (package text)
	  (zwei:get-package-to-search)
	(let ((string (zwei:string-interval bp1 bp2)))
	  (values (find-completing-symbols string
					   :default-package package
					   :all-packages (equalp "all packages" text))
		  bp1
		  bp2
		  package
		  string))))))

#+genera
(zwei:defcom zwei::com-complete-definition-name-rj
	"Attempt to complete the definition-name of the symbol under point" ()
  (multiple-value-bind (completions bp1 bp2 package string)
      (complete-definition-name-internal)
    (declare (ignore package))
    (cond ((null completions) (zwei:barf))
	  (t
	   (if (and (> (length completions)
		       dw::*default-display-possibilities-query-cutoff-length*)
		    (not (cl:y-or-n-p "There are ~D possibilities.  Do you want to see them all? "
				      (length completions))))
	       (zwei:barf)
	       (let ((choice (if (= (length completions) 1)
				 (first completions)
				 (dw:menu-choose (sort completions #'string<)
						 :prompt (concatenate 'string
								      "Choose a completion for: "
								      string)
						 :printer #'prin1))))
		 (if choice
		     (progn
		       (zwei:delete-interval bp1 bp2 t)
		       (zwei:move-point (zwei:insert (zwei:point)
						     (string-downcase (prin1-to-string choice)))))
		     (zwei:barf)))))))
  zwei::dis-text)

#+mcl
(defun select-one-item-from-list (item-list &rest keys)
  "Like select-item-from-list, but doesnｫt ask if there is no or only one item."
  (case (length item-list)
      (0 nil)                            ; no item
      (1 (first item-list))              ; just one item
      (otherwise                         ; select one item from many
       (let ((selection (apply #'select-item-from-list
                         item-list
                         :selection-type :single
                         keys)))
         (if (>= (length selection) 1)
           (first selection)             ; take the first
           nil)))))                      ; no selection


#+mcl
(defmethod ed-complete-symbol ((window window-fred-item) &key (all-packages nil))
  "Inserts a completion for the current symbol into the buffer."
  (let ((buffer (fred-buffer window))
        (*package* (or (fred-package window) *package*)))
    (multiple-value-bind (start end)
                         (buffer-current-sexp-bounds buffer)  ; well, it works
      (if (and start end)
        (when (eq :cancel
                  (catch-cancel
                    (multiple-value-bind (string-to-be-completed from-start from-end)
                                         (remove-some-characters-from-string
                                          (buffer-substring buffer start end))
                      (set-mini-buffer window "~&Completing : ~A" string-to-be-completed)
                      (fred-update window)
                      (let ((completing-symbol
                             (select-one-item-from-list
                              (sort (find-completing-symbols string-to-be-completed
                                                             :default-package *package*
                                                             :all-packages all-packages)
                                    #'string<)
                              :table-print-function #'prin1
                              :window-title "Select a completion.")))
                        (if completing-symbol
                          (progn
                            (collapse-selection window t)
                            (buffer-delete buffer (+ start from-start) (- end from-end))
                            (buffer-insert buffer
                                           (string-downcase (prin1-to-string completing-symbol))
                                           (+ start from-start))
                            (set-mini-buffer window "~&Completion : ~A" completing-symbol))
                          (set-mini-buffer window
                                           "~&No completion for ~A."
                                           string-to-be-completed))))))
          (set-mini-buffer window "~&Completion cancelled."))
        (set-mini-buffer window "~&Completion : No valid string.")))))

#+mcl
(defmethod ed-complete-symbol ((view fred-mixin) &key (all-packages nil))
  "Inserts a completion for the current symbol into the buffer."
  (let ((buffer (fred-buffer view))
        (*package* (or (fred-package view) *package*)))
    (multiple-value-bind (start end)
                         (buffer-current-sexp-bounds buffer)  ; well, it works
      (when (and start end)
        (catch-cancel
         (multiple-value-bind (string-to-be-completed from-start from-end)
                              (remove-some-characters-from-string
                               (buffer-substring buffer start end))
           (let ((completing-symbol
                  (select-one-item-from-list
                   (sort (find-completing-symbols string-to-be-completed
                                                  :default-package *package*
                                                  :all-packages all-packages)
                         #'string<)
                   :table-print-function #'prin1
                   :window-title "Select a completion.")))
             (when completing-symbol
               (collapse-selection view t)
               (buffer-delete buffer (+ start from-start) (- end from-end))
               (buffer-insert buffer
                              (string-downcase
                               (prin1-to-string completing-symbol))
                              (+ start from-start))))))))))

#+mcl
(defmethod ed-complete-symbol-in-all-packages ((view fred-mixin))
  "Inserts a completion for the current symbol into the buffer."
  (ed-complete-symbol view :all-packages t))

#+mcl
(def-fred-command (:control #\i) ed-complete-symbol "c-i")
#+mcl
(def-fred-command (:meta #\i) ed-complete-symbol-in-all-packages "m-i")

(provide 'completion)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of file


