;; transform.lisp

(declaim (optimize (safety 3) (debug 3) (speed 1) (space 0)))

(in-package :ledger)

(defun apply-key-transforms (xacts args)
  ;; (if comm_as_payee
  ;;     SET_COMM_AS_PAYEE
  ;;     (if code_as_payee
  ;;         SET_CODE_AS_PAYEE))

  ;; budget_transactions takes a set of transactions from a data file and uses
  ;; them to generate "budget transactions" which balance against the reported
  ;; transactions.
  ;;
  ;; forecast_transactions is a lot like budget_transactions, except that it
  ;; adds entries only for the future, and does not balance them against
  ;; anything but the future balance.
;;   (if budget_flags
;;       (progn
;; 	;; Apply the primary filter before the budget handler so that only
;; 	;; matching transactions are calculated toward the budget.  The use of
;; 	;; filter_transactions below will further clean the results so tha
;; 	;; automated transactions not matching the filter don't get reported.
;; 	(if predicate FILTER_TRANSACTIONS("limit"))
;; 	BUDGET_TRANSACTIONS)
;;       (when forecast_limit
;; 	(if predicate FILTER_TRANSACTIONS("limit"))
;; 	FORECAST_TRANSACTIONS))

  ;; `apply-filter' only passes through transactions matching the :expr (or
  ;; :limit) predicate.
  (setf xacts (apply #'apply-filter xacts args))

  ;; related_transactions will pass along all transactions related to the
  ;; transaction received.  If `show_all_related' is true, then all the
  ;; entry's transactions are passed; meaning that if one transaction of an
  ;; entry is to be printed, all the transaction for that entry will be
  ;; printed.
  ;; (if show_related RELATED_TRANSACTIONS)

  ;; invert_transactions inverts the value of the transactions it receives.
  ;; (if show_inverted INVERT_TRANSACTIONS)

  (if-let ((period (getf args :period)))
    ;; jww (2007-12-01): This should call group-by-period directly, once
    ;; things are working
    (setf xacts (periodic-transform xacts period)))
  
  (unless (getf args :balance-report)
    ;; dow_transactions is like period_transactions, except that it reports
    ;; all the transactions that fall on each subsequent day of the week.
    ;; (if days_of_the_week DOW_TRANSACTIONS)
    ;; (else if by_payee BY_PAYEE_TRANSACTIONS)

    ;; subtotal_transactions combines all the transactions it receives into
    ;; one subtotal entry, which has one transaction for each commodity in
    ;; each account.
    ;; (if show_subtotal SUBTOTAL_TRANSACTIONS)

    ;; collapse_transactions causes entries with multiple transactions to
    ;; appear as entries with a subtotaled transaction for each commodity
    ;; used.
    ;; (if show_collapsed COLLAPSE_TRANSACTIONS)

    ;; changed_value_transactions adds virtual transactions to the list to
    ;; account for changes in market value of commodities, which otherwise
    ;; would affect the running total unpredictably.
    ;; (if show_revalued CHANGED_VALUE_TRANSACTIONS)

    ;; sort_transactions will sort all the transactions it sees, based
    ;; on the `sort_order' value expression.
    (let ((sort (getf args :sort-entries)))
      (if sort
	  (setf xacts
		(sort-entries xacts
			      :key (etypecase sort
				     (string (value-expr-function
					      (parse-value-expr sort)))
				     (function sort))))

	  (if-let ((sort (getf args :sort)))
	    (setf xacts
		  (sort-transactions xacts
				     :key (etypecase sort
					    (string (value-expr-function
						     (parse-value-expr sort)))
					    (function sort)))))))
    
    ;; filter_transactions will only pass through transactions matching the
    ;; :only predicate.
    (if-let ((only-expr (getf args :only)))
      (setf xacts (choose-if-value-expr xacts only-expr)))

    ;; `calculate-totals' computes the running total.  When this appears will
    ;; determine, for example, whether filtered transactions are included or
    ;; excluded from the running total.
    (setf xacts (calculate-totals xacts
				  :amount (getf args :amount)
				  :total  (getf args :total)))

    ;; Only pass through transactions matching the :display predicate.
    (if-let ((display-expr (getf args :display)))
      (setf xacts (choose-if-value-expr xacts display-expr)))

    (let (arg)
      (cond
	((setf arg (getf args :head))
	 (setf xacts (subseries xacts 0 arg)))

	((setf arg (getf args :tail))
	 ;; Tail is expensive, because we don't know the length of the
	 ;; series until every element has been seen (and hence computed).
	 ;; Expect a large pause for giant data sets.
	 (setf xacts (subseries 0 (- (collect-length xacts) arg)))))))
  xacts)

(provide 'transform)

;; transform.lisp ends here
