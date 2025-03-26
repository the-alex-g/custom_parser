(defun roll (die threshold)
  (labels ((internal (acc)
	     (let ((val (1+ (random die))))
	       (if (>= val threshold)
		   (internal (1+ acc))
		   (1+ acc)))))
    (internal 0)))

(defun roll-2 (die threshold)
  (labels ((internal (acc)
	     (let ((val (1+ (random die))))
	       (cond ((= 1 val)
		      acc)
		     ((or (>= val threshold)
			  (= val die))
		      (internal (1+ acc)))
		     (t
		      (1+ acc))))))
    (internal 0)))

(defun print-new (&rest scales)
  (loop for die-size in '(4 5 6 8 10 12)
	do (loop for threshold in scales
		 when (<= threshold die-size)
		   do (format t "d~d/~d: ~4f~%"
			      die-size threshold
			      (/ (loop repeat 1000
				       sum (roll-2 die-size threshold))
				 1000)))))

(defun print-test ()
  (loop for die-size in '(4 6 8 10 12)
	do (loop for threshold from (min 5 die-size) to die-size
		 do (format t "d~d/~d: ~4f~%"
			    die-size threshold
			    (/ (loop repeat 1000
				     sum (roll die-size threshold))
			       1000)))))

(defun print-stat (&key (threshold 5) (dice (lambda (x) x)) (dcc t) (float nil))
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
      
							   
  
