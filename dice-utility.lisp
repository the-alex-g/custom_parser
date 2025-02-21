(defun print-dice (&key (threshold 5) (dice (lambda (x) x)) (dcc t) (float nil))
  (let ((dice-table (make-hash-table)))
    (loop for d in (if dcc
		       '(3 4 5 6 7 8 10 12 14 16 20 24 30)
		       '(3 4 6 8 10 12 20))
	  when (funcall dice d)
	    do (loop for r from (min d threshold) to d
		     do (push (format nil "d~d/~d" d r)
			      (gethash (/ d (- r 1)) dice-table))))
    (loop for r in (sort (loop for k being the hash-keys of dice-table
			       collect k)
			 #'>)
	  do (format t "~:[~5@a~;~4f~]: ~{~a~^, ~}~%"
		     float r (gethash r dice-table)))))
      
							   
  
