
#|
 _   _ _ _         _                 _ 
| \ | (_) | _____ | | __ _ _   _ ___(_)
|  \| | | |/ / _ \| |/ _` | | | / __| |
| |\  | |   < (_) | | (_| | |_| \__ \ |
|_| \_|_|_|\_\___/|_|\__,_|\__,_|___/_|
|#

#|
	/*
	 * 			A
	 * 	B				C
	 * 			
	 * 	D				E
	 */
|#

#|
Representation of pathes:
((A B) (B C) (C D) (D E))
for the start node:
((A A))
for A => B:
((B A) (A A))
for A => B => C:
((C B) (B A) (A A))
|#

(defparameter *edges* '
  ( (A (B C))
    (B (A C E D))
    (C (A B D E))
    (D (B C E))
    (E (C B D))
    ))

(defparameter *solutions* '())


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
  (if (equal (length path) 9)
      (push path *solutions*)
    (loop for node in (get-edges path) do
	  (if (not (is-bidirectional-traversal-p (list node (last-visited-node path)) path))
		   (iterate-impl (add-step path node))
	  ))))


#|
(A B E) => all solutions of given nodes
alternative implementation:
(mapcar #'(lambda (node) (iterate-impl (list (list node node)))) nodes)
|#
(defun nikolaus (nodes)
  (defparameter *solutions* ())
  (loop for i in nodes do
	(iterate-impl (list (list i i)))
	)

  )

(defun lets-go ()
  (nikolaus '(A B C D E))
  (print "Solutions:")
  (print (mapcar #'(lambda (path) (reverse (mapcar #'car path))) *solutions*))
  (print (list "Number of solutions found:" (length *solutions*)))
 )

(lets-go)


