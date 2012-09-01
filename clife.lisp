;; Author: Guglielmo Fachini a.k.a. GuglielmoS
;; Starting date: 08 August 2012
;; 
;; Implementation of the Conway's Game of Life
;;
;; If you want to try it you should load the file and call the function
;; 'run'.
;; Here are some examples of use:
;;
;; > (run)                  ;; run the evolution of a world of predefined size
;; > (run '(100 100)) ;; run the evolution of a 100x100 world
;;
;; In the case that you want to see the evolution to proceed slowly you can
;; modify the update interval by changing the value of the respective parameter.
;; For changing the size of the world, instead, you should modify the 
;; *default-world-size* parameter or call the 'run' function with a list of two
;; items as argoument, each representing the width and the height respectively.
;; An example of the latest use can be seen in the examples presented over here.
;;
;; Copyleft by GuglielmoS

(asdf:operate 'asdf:load-op :cl-glut)

(defparameter *default-world-size* '(150 150))
(defparameter *window-size* '(400 400))
(defparameter *update-interval* .05)

(defun range (end)
  "Returns all the natural numbers before 'end'."
  (loop for i below end collect i))

(defun gen-random-world (&optional (world-size *default-world-size*))
  "Generates a random world with the preferred size. 
   The data structure returned is a bidimensional array and the default size
   is taken from the global variable *default-world-size*."
  (let ((world (make-array world-size :element-type 'bit)))
    (destructuring-bind (dx dy) world-size
      (dolist (x (range dx))
	(dolist (y (range dy))
	  (setf (aref world x y) (random 2)))))
    world))

(defun alive-neighbours (world pos)
  "Returns the number of alive neighbours of the cell located at 'pos'.
   Since that the world is represented as a torus, all sides are connected."
  (destructuring-bind (dx dy x y) (append (array-dimensions world) pos)
    (flet ((neighbours (x y)
	     (let ((acc nil))
	       (dolist (fx '(-1 0 +1))
		 (dolist (fy '(-1 0 +1))
		   (when (not (and (zerop fx) (zerop fy)))
		     (push (cons (mod (+ x fx) dx) (mod (+ y fy) dy)) acc))))
	       acc)))
      (length (remove-if
	       (lambda (pos)
		 (if (eql (aref world (car pos) (cdr pos)) 0) T nil))
	       (neighbours x y))))))

(defun evolve (world)
  "Returns the next generation of the world passed as argoument."
  (destructuring-bind (dx dy) (array-dimensions world)
    (let ((nw (make-array (list dx dy))))
      (dolist (x (range dx))
	(dolist (y (range dy))
	  (let ((cur-state (aref world x y))
		(an (alive-neighbours world (list x y))))
	    (if (eq cur-state 0)
		(setf (aref nw x y) (if (eq an 3) 1 0))
		(setf (aref nw x y) (if (or (eq an 2) (eq an 3)) 1 0))))))
      nw)))

;; GUI PART

(defclass gl-window (glut:window)
  ((cells :accessor cells-of :initarg :cells))
  (:default-initargs
    :width (car *window-size*) 
    :height (cadr *window-size*)
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
    (if (zerop cell)
	(gl:color 0 0 0)
	(gl:color 1 0 0))
    (gl:with-primitive :points
      (gl:vertex x y 0)))

(defmethod glut:display ((w gl-window))
  "Draws the current world on the window."
  (gl:clear :color-buffer)
  (let ((cells (cells-of w)))
    (destructuring-bind (dx dy) (array-dimensions cells)
      (dolist (x (range dx))
	(dolist (y (range dy))
	  (render-cell x y (aref cells x y))))))
  (glut:swap-buffers))

(defmethod glut:idle ((w gl-window))
  "Steps to the next generation."
  (setf (cells-of w) (evolve (cells-of w)))
  (sleep *update-interval*)
  (glut:post-redisplay))

(defmethod glut:keyboard ((w gl-window) key x y)
  "Handles the keyboard interaction."
  (case key
    (#\Escape (glut:destroy-current-window))))

(defun run (&optional (world-size *default-world-size*)) 
  "Opens a window and starts the evolution of a random world." 
  (glut:display-window
   (make-instance 'gl-window
		  :cells (gen-random-world world-size))))
