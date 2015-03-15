;;; -*- Mode: LISP; Syntax: Common-Lisp -*-
(princ "Hello.")
(terpri)

'(when (string= (cl:host-namestring (user-homedir-pathname))
	       "DIS-EMB-HOST")
  (setf (future-common-lisp:logical-pathname-translations "SYS")
	'(("**;*.*.*" "dis-emb-host:/var/lib/symbolics/rel-8-5/sys.sct/**/*"))))



(let* ((lispm-inits (merge-pathnames (make-pathname :directory '(:relative "LISPM-INITS"))
				     (user-homedir-pathname))))
  (mapc (lambda (file)
	  (load (make-pathname :name file :defaults lispm-inits)))
	'("COM-EVALUATE-AND-INSERT-INTO-BUFFER"
	  "COMPLETION"
	  "ZCOMS")))

;;(sct:set-system-source-file 'mc "dis-emb-host:/home/mc/lispm-inits/mc.system")
;;(sct:set-system-directory-file 'mc "dis-emb-host:/home/mc/lispm-inits/mc.system")
;;(sct:system-directory )
;;(load-system 'mc)

(setf si:*default-lisp-syntax* ':ansi-common-lisp)
;; (setf si:*default-lisp-syntax* ':common-lisp)
(setf tv:*wholine-clock-24hour-p* t)
(setf si:*kbd-auto-repeat-enabled-p* t)
(setf tv:*wholine-clock-delimiters* nil)



(ed "gazonk.del")


;;; *EOF*
