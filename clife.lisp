;; Author: Guglielmo Fachini a.k.a. GuglielmoS
;; Date:   08 August 2012
;; 
;; Implementation of the Conway's Game of Life
;;
;; If you want to try it you should load the file and call the function
;; 'evolve':
;; > (evolve)
;;
;; If you want to change the size of world or the time interval between
;; a generation and another one, you can modify the parameter below.
;;

(defparameter *default-world-size* '(20 70))
(defparameter *update-interval* 0.1)
(defparameter *alive-cell* #\.)
(defparameter *dead-cell* #\space)

(defun gen-random-world (&optional (world-size *default-world-size*))
  (let ((world (make-array world-size :element-type 'bit)))
    (destructuring-bind (dim-x dim-y) world-size
      (loop for i from 0 below dim-x do
	   (loop for j from 0 below dim-y do
		(setf (aref world i j) (random 2)))))
    world))

(defun alive-neighbors (world pos)
  (destructuring-bind (dim-x dim-y x y) (append (array-dimensions world) pos)
    (let ((to-check (list 
		    (list (mod (1- x) dim-x) (mod (1- y) dim-y))
		     (list (mod (1- x) dim-x) (mod y dim-y))
		     (list (mod (1- x) dim-x) (mod (1+ y) dim-y))
		     (list x (mod (1- y) dim-y))
		     (list x (mod (1+ y) dim-y))
		     (list (mod (1+ x) dim-x) (mod (1- y) dim-y))
		     (list (mod (1+ x) dim-x) y)
		     (list (mod (1+ x) dim-x) (mod (1+ y) dim-y)))))
      (length (remove-if
	       (lambda (pos)
		 (if (eql (aref world (car pos) (cadr pos)) 0)
		     T
		     nil))
	       to-check)))))

(defun next-generation (world)
  (let* ((dim (array-dimensions world))
	(dim-x (car dim))
	(dim-y (cadr dim))
	(nw (make-array dim)))
    (loop for i from 0 below dim-x do 
	 (loop for j from 0 below dim-y do
	      (let ((cur-state (aref world i j))
		    (an (alive-neighbors world (list i j))))
		(if (eq cur-state 0)
		    (setf (aref nw i j) (if (eq an 3) 1 0))
		    (setf (aref nw i j) (if (or (eq an 2) (eq an 3)) 1 0))))))
    nw))

(defun print-world (world)
  (let* ((dim (array-dimensions world))
	(dim-x (car dim))
	(dim-y (cadr dim)))
    (loop for i from 0 below dim-x do
	 (fresh-line)
	 (loop for j from 0 below dim-y do
	      (let ((cur-cell (aref world i j)))
		(if (eql cur-cell 1)
		    (princ *alive-cell*)
		    (princ *dead-cell*)))))))

(let ((generation 0))
  (defun life-loop (world)
    (ext:run-program "clear")
    (format t "Generation: ~a" generation)
    (print-world world)
    (sleep *update-interval*)
    (incf generation)
    (life-loop (next-generation world))))

(defun evolve ()
  (life-loop (gen-random-world)))

