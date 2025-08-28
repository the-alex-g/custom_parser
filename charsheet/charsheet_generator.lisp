(defparameter *charsheet* nil)

(defun initialize-sheet (w h)
  (setf *charsheet* (loop repeat h collect (loop repeat w collect #\space))))

(defun set-cell (column row val)
  (setf (nth column (nth row *charsheet*)) val))

(defun create-rect (x y w h fill-type)
  (loop for row from y to (1- (+ y h))
	do (loop for column from x to (1- (+ x w))
		 do (cond ((eq fill-type 'lines)
			   (when (oddp (- row y)) ; every other line
			     (set-cell column row (if (or (= x column) (= (1- (+ x w)) column))
						      #\|
						      #\_))))
			  ((characterp fill-type)
			   (set-cell column row fill-type))))))

(defun create-centered-label (x y width text)
  (let ((padding-left (floor (/ (- width (length text)) 2))))
    (loop for char in (coerce text 'list)
	  with col = 0
	  do (set-cell (+ x col padding-left) y (char-upcase char))
	  do (incf col))))

(defun print-sheet (&optional (stream t))
  (format stream "~{~{~c~}~%~}" *charsheet*))
