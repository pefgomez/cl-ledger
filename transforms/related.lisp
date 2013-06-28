;; related.lisp

#-:debug-cl-ledger(declaim (optimize (safety 3) (speed 1) (space 0) (debug 0)))
#+:debug-cl-ledger(declaim (optimize (safety 0) (speed 0) (space 0) (debug 3) (compilation-speed 0)))

(in-package :ledger)

;; This must happen before transaction sorting.
(defun related-transactions (xact-series)
  (scan-lists-of-lists-fringe
   (mapcar
    #'(lambda (entry-xacts)
	(remove-if #'(lambda (xact)
		       (member xact entry-xacts))
		   (entry-transactions
		    (xact-entry (car entry-xacts)))))
    (group-transactions-by-entry (collect xact-series)))))

(provide 'related)

;; related.lisp ends here
