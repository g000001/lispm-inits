;;; -*- Mode: LISP; Package: ZWEI -*-

(DEFCOM COM-INSERT-- "Insert a - in the buffer at point." ()
	  (DOTIMES (IGNORE *NUMERIC-ARG*) (INSERT-MOVING-POINT #\-))
	  DIS-TEXT)

(set-comtab *zmacs-comtab*
	      '(#\control-m com-insert-crs
		#\control-h com-rubout
		#\meta-shift-l com-make-\(\)
		#\meta-: com-forward-up-list
		#\control-i com-indent-for-lisp
		#\control-meta-shift-h com-kill-backward-up-list
		;; #\control-/ com-insert--
		#\meta-space com-insert--
		#\control-shift-j com-evaluate-and-insert-into-buffer
		#\control-meta-i zwei::com-complete-definition-name-rj
		;;#\control-meta-i com-complete-definition-name
;;		#\control-meta-i com-list-definition-names
		)
	      (make-command-alist '(com-evaluate-and-insert-into-buffer)))


;; *EOF*