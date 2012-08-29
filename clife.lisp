;; Author: Guglielmo Fachini a.k.a. GuglielmoS
;; Starting date: 08 August 2012
;; 
;; Implementation of the Conway's Game of Life
;;
;; If you want to try it you should load the file and call the function
;; 'run':
;; > (run)
;;
;; For changing the size of world you should modify the *default-world-size*
;; parameter.
;;
;; Copyleft by GuglielmoS

(asdf:operate 'asdf:load-op :cl-glut)

(defparameter *default-world-size* '(200 200))

(defun range (end)
  "Returns all the natural numbers before 'end'."
  (loop for i below end collect i))

(defun gen-random-world (&optional (world-size *default-world-size*))
  "Generates a random world with the preferred size. 
   The data structure returned is a bidimensional array and the default size
   is taken from the global variable *default-world-size*."
  (let ((world (make-array world-size :element-type 'bit)))
    (destructuring-bind (dim-x dim-y) world-size
      (loop for i in (range dim-x) do
	   (loop for j in (range dim-y) do
		(setf (aref world i j) (random 2)))))
    world))

(defun alive-neighbours (world pos)
  "Returns the number of alive neighbours of the cell located at 'pos'.
   Since that the world is represented as a torus all sides are connected."
  (destructuring-bind (dim-x dim-y x y) (append (array-dimensions world) pos)
    (labels ((neighbours (x y)
	       (let ((acc nil))
		 (dolist (dx '(-1 0 +1))
		   (dolist (dy '(-1 0 +1))
		     (when (not (and (zerop dx) (zerop dy)))
		       (push (cons (mod (+ x dx) dim-x) (mod (+ y dy) dim-y)) acc))))
		 acc)))
      (length (remove-if 
	       (lambda (pos)
		 (if (eql (aref world (car pos) (cdr pos)) 0) T nil))
	       (neighbours x y))))))

(defun next-generation (world)
  "Returns the next generation of the world passed as argoument."
  (destructuring-bind (dim-x dim-y) (array-dimensions world)
    (let ((nw (make-array (list dim-x dim-y))))
      (loop for i in (range dim-x) do
	   (loop for j in (range dim-y) do
		  (let ((cur-state (aref world i j))
		      (an (alive-neighbours world (list i j))))
		  (if (eq cur-state 0)
		      (setf (aref nw i j) (if (eq an 3) 1 0))
		      (setf (aref nw i j) (if (or (eq an 2) (eq an 3)) 1 0))))))
      nw)))

;; GUI PART

(defclass gl-window (glut:window)
  ((cells :accessor cells-of :initarg :cells))
  (:default-initargs 
   :width 400 :height 300
   :title "Conway's Game of Life"
   :mode '(:double :rgb)))

(defmethod glut:display-window :before ((w gl-window))
  (gl:clear-color 0 0 0 0)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (let ((cells (cells-of w)))
    (gl:ortho 0 (array-dimension cells 1)  0 (array-dimension cells 0) -1 1)))

(defun render-cell (x y cell)
  "Render a cell onto the window with the proper color."
  (flet ((draw-cell (x y)
	   (gl:with-pushed-matrix
	       (gl:translate x y 0)
	     (gl:with-primitive :points
	       (gl:vertex x y 0)))))
    (case cell
      (1 (gl:color 1 0 0)
	   (draw-cell x y))
      (0 (gl:color 0.0 0.0 0.0)
	      (draw-cell x y)))))

(defmethod glut:display ((w gl-window))
  "Draws the current world on the window."
  (gl:clear :color-buffer)
  (let* ((cells (cells-of w))
	 (w (array-dimension cells 1))
	 (h (array-dimension cells 0)))
    (loop for j below h
       do (loop for i below w
	     do (render-cell i j (aref cells j i)))))
  (glut:swap-buffers))

(defmethod glut:idle ((w gl-window))
  "Steps to the next generation."
  (setf (cells-of w) (next-generation (cells-of w)))
  (glut:post-redisplay))

(defmethod glut:keyboard ((w gl-window) key x y)
  "Handles the keyboard interaction."
  (case key
    (#\Escape (glut:destroy-current-window))))

(defun run ()
  "Opens a window and starts the evolution of a random world." 
  (glut:display-window
   (make-instance 'gl-window
		  :cells (gen-random-world))))
