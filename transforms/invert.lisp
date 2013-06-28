;; invert.lisp

#-:debug-cl-ledger(declaim (optimize (safety 3) (speed 1) (space 0) (debug 0)))
#+:debug-cl-ledger(declaim (optimize (safety 0) (speed 0) (space 0) (debug 3) (compilation-speed 0)))

(in-package :ledger)

(defun invert-transactions (xact-series)
  (map-fn 'transaction
	  #'(lambda (xact)
	      (setf (xact-value xact :computed-amount)
		    (cambl:negate (xact-amount xact)))
	      xact)
	  xact-series))

(provide 'invert)

;; invert.lisp ends here
