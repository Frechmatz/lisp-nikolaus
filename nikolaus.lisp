
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
Representation of pathes
((A B) (B C) (C D) (D E))
((A A))
Pathes are managed backwards.
|#


(defparameter *relations* '
  ( (A (B C))
    (B (A C E D))
    (C (A B D E))
    (D (B C E))
    (E (C B D))
    ))

(defparameter *solutions* '())


#|
check if (A B) is in path
|#
(defun is-segment-p (tupel path)
  (find tupel path :test #'equal)
)

#|
swap tuple values. (A B) => (B A) => (A B)
|#
(defun swap (tupel)
  (list (cadr tupel) (car tupel))
)

#|
check if (A B) or (B A) are in path 
|#
(defun is-traversed-relation (tupel path)
  (or (is-segment-p tupel path) (is-segment-p (swap tupel) path)) 
)

(may-go (node path) ()
	
)

#|
determines the end-node of the path. See above about the structure of pathes
|#
(defun first-node (path)
  (car (car path))
)



#|
in: ((a b) (c d))
Must be called with a list of lists
out: the relations of a. path is inverse
out: a list, for example (B C)
|#
(defun get-relations (path)
  (
   car(cdr(assoc (first-node path) *relations*)))
  )

(defun next-path (path node) ()
	  (push (list node (first-node path)) path)

       )

#|
path: ( (a b) (b c))
|#
(defun iterate-impl (path) 
  (if (equal (length path) 8)
      (push path *solutions*)
    (loop for node in (get-relations path) do
	  (if (is-traversed-relation (list node (first-node path)) path)
	  ()
	  (iterate-impl (next-path path node))
	  ))))



(defun iterate (xs)
  (iterate-impl (list (list xs xs)))
)))


(defun nikolaus ()
  (loop for i in '(A B C D E) do
	(if i
	    (iterate i))))


