;; transaction.lisp

#-:debug-cl-ledger(declaim (optimize (safety 3) (speed 1) (space 0) (debug 0)))
#+:debug-cl-ledger(declaim (optimize (safety 0) (speed 0) (space 0) (debug 3) (compilation-speed 0)))

(in-package :ledger)

(require 'types)

#-:debug-cl-ledger(declaim (inline xact-entry set-xact-entry))
(defun xact-entry (xact)
  (get-xact-entry xact))
(defun set-xact-entry (xact value)
  (setf (get-xact-entry xact) value))
(defsetf xact-entry set-xact-entry)

#-:debug-cl-ledger(declaim (inline xact-actual-date set-xact-actual-date))
(defun xact-actual-date (xact)
  (get-xact-actual-date xact))
(defun set-xact-actual-date (xact value)
  (setf (get-xact-actual-date xact) value))
(defsetf xact-entry set-xact-entry)

#-:debug-cl-ledger(declaim (inline xact-effective-date set-xact-effective-date))
(defun xact-effective-date (xact)
  (get-xact-effective-date xact))
(defun set-xact-effective-date (xact value)
  (setf (get-xact-effective-date xact) value))
(defsetf xact-effective-date set-xact-effective-date)

#-:debug-cl-ledger(declaim (inline xact-status set-xact-status))
(defun xact-status (xact)
  (let ((entry-status (entry-status (xact-entry xact))))
   (if (equal :uncleared entry-status)
       (get-xact-status xact)
       entry-status)))
(defun set-xact-status (xact value)
  (setf (get-xact-status xact) value))
(defsetf xact-status set-xact-status)

#-:debug-cl-ledger(declaim (inline xact-account set-xact-account))
(defun xact-account (xact)
  (get-xact-account xact))
(defun set-xact-account (xact value)
  (setf (get-xact-account xact) value))
(defsetf xact-account set-xact-account)

#-:debug-cl-ledger(declaim (inline xact-cost set-xact-cost))
(defun xact-cost (xact)
  (get-xact-cost xact))
(defun set-xact-cost (xact value)
  (setf (get-xact-cost xact) value))
(defsetf xact-cost set-xact-cost)

#-:debug-cl-ledger(declaim (inline xact-note set-xact-note))
(defun xact-note (xact)
  (get-xact-note xact))
(defun set-xact-note (xact value)
  (setf (get-xact-note xact) value))
(defsetf xact-note set-xact-note)

#-:debug-cl-ledger(declaim (inline xact-virtualp set-xact-virtualp))
(defun xact-virtualp (xact)
  (get-xact-virtualp xact))
(defun set-xact-virtualp (xact value)
  (setf (get-xact-virtualp xact) value))
(defsetf xact-virtualp set-xact-virtualp)

#-:debug-cl-ledger(declaim (inline xact-generatedp set-xact-generatedp))
(defun xact-generatedp (xact)
  (get-xact-generatedp xact))
(defun set-xact-generatedp (xact value)
  (setf (get-xact-generatedp xact) value))
(defsetf xact-generatedp set-xact-generatedp)

#-:debug-cl-ledger(declaim (inline xact-calculatedp set-xact-calculatedp))
(defun xact-calculatedp (xact)
  (get-xact-calculatedp xact))
(defun set-xact-calculatedp (xact value)
  (setf (get-xact-calculatedp xact) value))
(defsetf xact-calculatedp set-xact-calculatedp)

#-:debug-cl-ledger(declaim (inline xact-must-balance-p set-xact-must-balance-p))
(defun xact-must-balance-p (xact)
  (get-xact-must-balance-p xact))
(defun set-xact-must-balance-p (xact value)
  (setf (get-xact-must-balance-p xact) value))
(defsetf xact-must-balance-p set-xact-must-balance-p)

#-:debug-cl-ledger(declaim (inline xact-position set-xact-position))
(defun xact-position (xact)
  (get-xact-position xact))
(defun set-xact-position (xact value)
  (setf (get-xact-position xact) value))
(defsetf xact-position set-xact-position)

#-:debug-cl-ledger(declaim (inline xact-data set-xact-data))
(defun xact-data (xact)
  (get-xact-data xact))
(defun set-xact-data (xact value)
  (setf (get-xact-data xact) value))
(defsetf xact-data set-xact-data)

(defun print-transaction (transaction stream depth)
  (declare (ignore depth))
  (declare (type stream stream))
  (print-unreadable-object (transaction stream :type t)
    (format stream ":DATE ~S :ACCT ~S :AMT ~S :V ~S :M ~S :G ~S :C ~S :POS ~S"
	    (let ((date (xact-date transaction)))
	      (and date (strftime date)))
	    (account-fullname (xact-account transaction))
	    (let ((amt (xact-amount transaction)))
	      (and (typep amt 'value)
		   (format-value amt)))
	    (xact-virtualp transaction)
	    (xact-must-balance-p transaction)
	    (xact-generatedp transaction)
	    (xact-calculatedp transaction)
	    (let ((pos (xact-position transaction)))
	      (and pos (item-position-begin-line pos))))))

#-:debug-cl-ledger(declaim (inline xact-value))
(defun xact-value (xact key)
  (if-let ((data (xact-data xact)))
    (let ((value-cell (assoc key (xact-data xact))))
      (values (cdr value-cell) value-cell))))

(defsetf xact-value (xact key) (value)
  (let ((xact-sym (gensym))
	(key-sym (gensym)))
    `(let* ((,xact-sym ,xact)
	    (,key-sym ,key)
	    (value-cell (assoc ,key-sym (xact-data ,xact-sym))))
       (if value-cell
	   (rplacd value-cell ,value)
	   (push (cons ,key-sym ,value)
		 (xact-data ,xact-sym))))))

(defun xact-date (xact)
  (declare (type transaction xact))
  (if *use-effective-dates*
      (or (xact-effective-date xact)
	  (entry-date (xact-entry xact)))
      (or (xact-actual-date xact)
	  (entry-date (xact-entry xact)))))

(defun xact-amount (xact)
  (declare (type transaction xact))
  #-:debug-cl-ledger(declare (optimize (speed 3) (safety 0)))
  (or (xact-value xact :computed-amount)
      (let ((amount (get-xact-amount xact)))
	(etypecase amount
	  (value amount)
	  (null
	   (error "Resolving transaction amount for unnormalized data"))
	  (value-expr
	   (setf amount (value-expr-call amount xact)
		 (xact-value xact :computed-amount) amount)
	   amount)))))

(defun xact-amount-expr (xact)
  (declare (type transaction xact))
  (let ((amount (get-xact-amount xact)))
    (cond
      ((valuep amount) amount)
      ((null amount) nil)
      ((value-expr-p amount)
       (value-expr-string amount))
      (t
       (error "impossible")))))

#-:debug-cl-ledger(declaim (inline set-xact-amount))
(defun set-xact-amount (xact value)
  (setf (xact-value xact :computed-amount) nil)
  (setf (get-xact-amount xact) value))
(defsetf xact-amount set-xact-amount)

(defun xact-amount* (xact)
  (declare (type transaction xact))
  (the (or value null)
    (or (xact-value xact :computed-amount)
	(let ((amount (get-xact-amount xact)))
	  (etypecase amount
	    (value amount)
	    (null nil)
	    (value-expr
	     (setf amount (value-expr-call amount xact)
		   (xact-value xact :computed-amount) amount)
	     amount))))))

#-:debug-cl-ledger(declaim (inline set-xact-amount*))
(defun set-xact-amount* (xact value)
  (setf (xact-value xact :computed-amount) nil)
  (setf (get-xact-amount xact) value))
(defsetf xact-amount* set-xact-amount*)

#-:debug-cl-ledger(declaim (inline xact-cleared-p))
(defun xact-cleared-p (xact)
  (declare (type transaction xact))
  (eq (xact-status xact) :cleared))

#-:debug-cl-ledger(declaim (inline xact-pending-p))
(defun xact-pending-p (xact)
  (declare (type transaction xact))
  (eq (xact-status xact) :pending))

#-:debug-cl-ledger(declaim (inline xact-uncleared-p))
(defun xact-uncleared-p (xact)
  (declare (type transaction xact))
  (eq (xact-status xact) :uncleared))

#-:debug-cl-ledger(declaim (inline xact-display-amount))
(defun xact-display-amount (xact)
  (or (xact-value xact :display-amount)
      (xact-amount xact)))

#-:debug-cl-ledger(declaim (inline xact-total))
(defun xact-total (xact)
  (or (xact-value xact :running-total) 0))

#-:debug-cl-ledger(declaim (inline xact-total))
(defun xact-cost-total (xact)
  (or (xact-value xact :running-cost-total)
      (xact-total xact)))

#-:debug-cl-ledger(declaim (inline xact-display-total))
(defun xact-display-total (xact)
  (or (xact-value xact :display-total)
      (xact-total xact)))

(defun group-transactions-by-entry (xacts-list)
  "\"Clump\" the incoming stream of transactions into sublists, where adjacent
transactions with the same parent entry become part of the same sublist.

  Consider the following input stream, with the first letter identifying the
entry and the second identifying the member transaction:

  (A-X A-Y A-Z B-X B-Y C-X)

Given this input, the resulting list from `group-transactions-by-entry' will
be:

  ((A-X A-Y A-Z) (B-X B-Y) (C-X))"
  (nreverse
   (reduce #'(lambda (entries xact)
	       (if entries
		   (if (eq (xact-entry (caar entries))
			   (xact-entry xact))
		       (cons (nconc (first entries) (list xact))
			     (rest entries))
		       (cons (list xact) entries))
		   (list (list xact))))
	   xacts-list
	   :initial-value '())))

(provide 'transaction)

;; transaction.lisp ends here
