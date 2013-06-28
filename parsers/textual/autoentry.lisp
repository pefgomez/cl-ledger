#-:debug-cl-ledger(declaim (optimize (safety 3) (speed 1) (space 0) (debug 0)))
#+:debug-cl-ledger(declaim (optimize (safety 0) (speed 0) (space 0) (debug 3) (compilation-speed 0)))

(in-package :ledger-textual)

(defclass automated-entry (entry)
  ((predicate-expr :accessor auto-entry-predicate-expr
		   :initarg :predicate-expr :type string)
   (predicate :accessor auto-entry-predicate
	      :initarg :predicate :type function)))

(defun read-automated-entry (in line journal)
  (declare (type stream in))
  (declare (type journal journal))
  (let* ((predicate-expr (read-line in))
	 (lines 1)
         (value-expr (or (parse-value-expr predicate-expr)
			 (error "Failed to parse predicate value expression: ~S"
				predicate-expr)))
         (entry
          (make-instance 'automated-entry
                         :journal journal
                         :predicate-expr (value-expr-string value-expr)
                         :predicate (value-expr-function value-expr))))
    (loop
       for transaction = (read-transaction in (+ line lines) entry)
       while transaction do
       (add-transaction entry transaction)
       (incf lines))
    (incf lines)

    (let ((automated-entries (assoc :automated-entries
                                    (journal-data journal))))
      (if automated-entries
          (nconc (cdr automated-entries) (list entry))
          (push (cons :automated-entries (list entry))
                (journal-data journal))))

    (values entry lines)))

(pushnew `(#\= . ,#'(lambda (in line journal)
                      (read-char in)
		      (multiple-value-bind (entry lines)
			  (read-automated-entry in line journal)
			(if entry
			    (add-to-contents journal entry)
			    (error "Failed to read entry at line ~D~%" line))
			lines)))
         *directive-handlers*)

(defun apply-automated-entries (entry &optional postp)
  (let ((automated-entries (assoc :automated-entries
                                  (journal-data (entry-journal entry)))))
    (when automated-entries
      (dolist (auto-entry (cdr automated-entries))
	(do-transactions (outer-xact entry)
	  (when (funcall (auto-entry-predicate auto-entry) outer-xact)
	    (do-transactions (inner-xact auto-entry)
	      (let ((amt
		     (block nil
		       (if (amount-commodity (xact-amount inner-xact))
			   (progn
			     (if postp
				 (return))
			     (xact-amount inner-xact))
			   (progn
			     (if (not postp)
				 (return))
			     (multiply (xact-amount outer-xact)
				       (xact-amount inner-xact)))))))
		(when amt
		  (let* ((account (xact-account inner-xact))
			 (fullname (account-fullname account)))
		    (if (or (string= fullname "$account")
			    (string= fullname "@account"))
			(setf account (xact-account outer-xact)))
		    (let ((new-xact (make-transaction
				     :entry entry
				     :status (xact-status inner-xact)
				     :actual-date (xact-actual-date inner-xact)
				     :effective-date (xact-effective-date inner-xact)
				     :note (xact-note inner-xact)
				     :position (copy-item-position
						(xact-position inner-xact))
				     :account account
				     :amount amt
				     :virtualp (xact-virtualp inner-xact)
				     :must-balance-p (xact-must-balance-p inner-xact)
				     :generatedp t)))
		      (push (cons :automatedp t) (xact-data new-xact))
		      (add-transaction entry new-xact))))))))))))

(pushnew #'apply-automated-entries *pre-normalization-functions*)
(pushnew #'apply-automated-entries *post-normalization-functions*)

(provide 'autoentry)

;; autoentry.lisp ends here
