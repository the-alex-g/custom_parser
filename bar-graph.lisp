(defun roll (die threshold)
  (labels ((internal (acc)
	     (let ((val (1+ (random die))))
	       (if (>= val threshold)
		   (internal (+ acc val))
		   (+ acc val)))))
    (internal 0)))

(defun display-results ()
  (loop for die-size in '(4 6 8 10 12)
	do (loop for threshold from (min 5 die-size) to die-size
		 do (format t "d~d/~d~%" die-size threshold)
		 do (loop for val in  (sort (loop for x below 100
						  collect (roll die-size threshold))
					    #'<)
			  with last-val = 1
			  with accum = 0
			  if (= last-val val)
			    do (incf accum)
			  else
			    do (format t "~2t~2d: ~v@{~c~:*~}~%"
				       last-val accum #\|)
			    and do (setf last-val val)
			    and do (setf accum 1)))))

(display-results)
