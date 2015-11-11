#!/usr/bin/clisp

;;;============================================================
;;; Receives module-base-name player problem-base-name run-type
;;;
;;; Runs player against problem, and prints diagnostic followed by
;;; solution (if any) to stardard output.
;;;
;;; Arguments:
;;;
;;; module-base-name: the complete path to your program source file,
;;; without .lisp or any other extension
;;;
;;; player: the name of your package; notice that while Lisp allows a
;;; wide variety of package names, the submission system requires
;;; the package name (player) to start with a letter and contain
;;; letters, digits, and underscores only
;;;
;;; problem-base-name: the complete path to your problem file, without
;;; .lisp or nay other extension
;;;
;;; run-type: either req or rand, to indicate a requested or random run

(defun module-base-name ()
  (first ext:*args*)
  )

(defun player ()
  (second ext:*args*)
  )

(defun problem-base-name ()
  (third ext:*args*)
  )

(defun run-type ()
  (fourth ext:*args*)
  )

(defun load-player (player-base-name)
  "Loads PLAYER-BASE-NAME.lisp"
  (load player-base-name :if-does-not-exist nil)
  )

(defun load-solution (problem-base-name)
  "Returns solution from PROBELM-BASE-NAME-solution.lisp"
  (with-open-file (f (string-concat problem-base-name "-solution.lisp")
		     :if-does-not-exist nil)
		  (when f (read f))
		  )
  )

(defun print-assignment (stream sol)
  "Prints solution (i.e., assignment) in human readable form"
  (if (good-syntax sol)
      (dolist (item sol)
	(format stream "~S = ~S~%" (first item) (second item))
	)
    (format stream "~S~%" sol)
    )
  )

(defun main-body (player-base-name player problem-base-name run-type)
  "Tests PLAYER against PROBLEM.  Return two values: DIAGNOSTIC and SOLUTION"
  (if (load-player player-base-name)
      (let
	  ((good (load-solution problem-base-name))
	   )
	(if good
	    (call-player player problem-base-name good)
	  (values "solution file does not exist" ())
	  )
	)
    (values "player file does not exist" ())
    )
  )

(defun call-player (player problem-base-name good)
  "Calls PLAYER on PROBLEM-BASE-NAME and compares it to the GOOD
solution.  Returns two values: diagnostic and solution"
  (let ((pack (find-package (string-upcase player))))
    (if pack
	(multiple-value-bind
	 (player-symbol visibility)
	 (find-symbol "SOLVER" pack)
	 (if player-symbol
	     (if (eql visibility :external)
		 (let ((player-function (and (fboundp player-symbol)
					     (symbol-function player-symbol))))
		   (if player-function
		       (handler-case
			(let* ((start (get-internal-run-time))
			       (sol (funcall player-function
					     (string-concat problem-base-name
							    ".lisp")))
			       (stop (get-internal-run-time))
			       (diag (verify good
					     sol
					     (elapsed-run-time start stop)))
			       )
			  (values diag sol)
			  )
			(error (condition)
			       (values (print-condition condition) ())
			       )
			)
   		     (values "no function called SOLVER in package" ())
		     )
		   )
	       (values "symbol SOLVER not exported" ())
	       )
	   (values "symbol SOLVER not found in package" ())
	   )
	 )
      (values "package not found" ())
      )
    )
  )

(defun print-condition (condition)
  (remove #\Newline (format nil "error in SOLVER call: ~A" condition))
  )

(defun verify (good sol time)
  (if (and (good-syntax sol)
	   (equal (sorted-solution sol) (sorted-solution good)))
      (format nil "ok ~9F" time)
    "wrong solution"
    )
  )

(defun good-syntax (sol)
  "Good syntax is being a list, and having every element with a symbolic car"
  (cond
   ((null sol) t)
   ((listp (car sol)) (and (symbolp (caar sol)) (good-syntax (cdr sol))))
   (t nil)
   )
  )

(defun sorted-solution (sol)
  (sort sol #'string< :key #'(lambda(x)(symbol-name (car x))))
  )

(defun elapsed-run-time (start stop)
  (/ (float (- stop start)) internal-time-units-per-second)
  )

(if (= (length ext:*args*) 4)
    (let*
	((module-base-name (module-base-name))
	 (player (player))
	 (problem-base-name (problem-base-name))
	 (run-type (run-type))
	 )
      ;; (format *error-output* "args: ~S ~S ~S ~S~%"
      ;; module-base-name player problem-base-name run-type)
      (multiple-value-bind (diag sol)
			   (main-body module-base-name
				      player
				      problem-base-name
				      run-type)
			   (format *standard-output* "~A~%" diag)
			   (print-assignment *standard-output* sol)
			   )
      )
  (format *standard-output* "Usage: ./run-expr.lisp ~A ~A ~A ~A"
	  "module-base-name"
	  "player"
	  "problem-base-name"
	  "run-type")
  )
