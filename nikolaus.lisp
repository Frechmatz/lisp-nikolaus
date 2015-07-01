
#|
 _   _ _ _         _                 _ 
| \ | (_) | _____ | | __ _ _   _ ___(_)
|  \| | | |/ / _ \| |/ _` | | | / __| |
| |\  | |   < (_) | | (_| | |_| \__ \ |
|_| \_|_|_|\_\___/|_|\__,_|\__,_|___/_|
|#

#|
 			A
 	B				C
	D				E
|#

#|
Representation of pathes:
for the start node:
==> ((A A))
for A => B:
==> ((B A) (A A))
for A => B => C:
==> ((C B) (B A) (A A))
|#

(defparameter *edges* '
  ( (A (B C))
    (B (A C E D))
    (C (A B D E))
    (D (B C E))
    (E (C B D))
    ))

#|
check if (A B) has been traversed
|#
(defun is-traversal-p (tupel path)
  (find tupel path :test #'equal)
)

#|
check if (A B) or (B A) have been traversed
|#
(defun is-bidirectional-traversal-p (tupel path)
  (or (is-traversal-p tupel path) (is-traversal-p (reverse tupel) path)) 
)

#|
((E D) (D C)) => E
|#
(defun last-visited-node (path)
  (car (car path))
  )

#|
((E D) (D B)) => (C B D)
|#
(defun get-edges (path)
  (
   car(cdr(assoc (last-visited-node path) *edges*)))
  )

#|
((A B) (C D)) => ((X A) (A B) (C D))
|#
(defun add-step (path node) ()
       (push (list node (last-visited-node path)) path)
       )

#|
any given path => solution
((A A)) => ...
((E D) (D C) (C C)) => ...
|#
(defun iterate-impl (path)
  (let ((solutions ()))
    (labels ((inner (path)
	    (if (equal (length path) 9)
		(push path solutions)
	      (loop for node in (get-edges path) do
		    (if (not (is-bidirectional-traversal-p (list node (last-visited-node path)) path))
			(inner (add-step path node)))))))
	    (inner path))
    solutions))

#|
(A B E) => all solutions of given nodes
alternative implementation:
(mapcar #'(lambda (node) (iterate-impl (list (list node node)))) nodes)
|#
(defun nikolaus (nodes)
  (let ((node-solutions ()))
    (loop for i in nodes do
	(push (iterate-impl (list (list i i))) node-solutions)
	)
    node-solutions))

;;
;; path: solution in reverse order
;; start node X of path is represented by (X X)
;; tupels are (<target-node> <start-node>)
;; example for start node D: ((E D) (D C) (C E) (E B) (B C) (C A) (A B) (B D) (D D))
(defun format-solution-path (path)
  ;; reverse and skip start tupel (X X)
  (let ((first t))
    (dolist (step (cdr (reverse path)))
      (if first
	  (format t "~a -> ~a" (second step) (first step))
	(format t " -> ~a" (first step))
	)
      (setf first nil)
      )))

(defun format-house ()
  (format t "~cA" #\tab)
  (format t "~%B~c~cC" #\tab #\tab)
  (format t "~%D~c~cE~%" #\tab #\tab)
  )
  
;;
;; Entry point
;;
(defun lets-go ()
  (format t "~%Welcome to 'Haus vom Nikolaus'~%")
  (format-house)
  (let ((node-solutions (nikolaus '(A B C D E))))
    ;; solutions: for each node: a list of solutions or nil
    (let ((flattened-solutions ()))
      (dolist (node node-solutions)
	(if node (dolist (s node)
		   (push s flattened-solutions)))
	)
      (format t "Solutions:~%")
      (dolist (solution flattened-solutions)
	(format-solution-path solution)
	(princ #\newline)
	)
      (format t "Number of solutions found: ~a~%" (length flattened-solutions))
      )))


