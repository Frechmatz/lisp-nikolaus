(defpackage :nikolaus-v2
  (:use :cl))

(in-package :nikolaus-v2)

#|
  _   _ _ _         _                 _  __      _____  
 | \ | (_) |       | |               (_) \ \    / /__ \ 
 |  \| |_| | _____ | | __ _ _   _ ___ _   \ \  / /   ) |
 | . ` | | |/ / _ \| |/ _` | | | / __| |   \ \/ /   / / 
 | |\  | |   < (_) | | (_| | |_| \__ \ |    \  /   / /_ 
 |_| \_|_|_|\_\___/|_|\__,_|\__,_|___/_|     \/   |____|
                                                        
|#

#|
 			A
 	B				C
	D				E
|#

(defparameter *graph*
  '((A B)
    (A C)
    (B C)
    (B E)
    (B D)
    (C E)
    (C D)
    (D E)))

(defun make-edges ()
  (let ((edges nil))
    (dolist (edge *graph*)
      (let ((edge-id (gensym)))
	(push (list edge-id (first edge) (second edge)) edges)
	(push (list edge-id (second edge) (first edge)) edges)))
    edges))
	
(defun get-edges (node edges)
  (let ((result (remove-if-not (lambda(item) (eq node (second item))) edges)))
    result))

(defun crawl (node)
  (let ((edges (make-edges)) (pathes nil))
    (labels ((crawl-impl (node visited-edge-ids path)
	       (if (= 8 (length visited-edge-ids))
		   (push path pathes)
		   (dolist (edge (get-edges node edges))
		     (if (not (find (first edge) visited-edge-ids))
			 (crawl-impl
			  (third edge)
			  (concatenate 'list visited-edge-ids (list (first edge)))
			  (concatenate 'string path (symbol-name (third edge)))))))))
      (crawl-impl node nil (symbol-name node))
      pathes)))

(defun lets-go ()
  (let ((pathes
	 (concatenate
	  'list
	  (crawl 'A)
	  (crawl 'B)
	  (crawl 'C)
	  (crawl 'D)
	  (crawl 'E))))
    (format t "~%Pathes: ~%~a~%Number of pathes: ~a~%" pathes (length pathes)))
  "DONE")

;; (lets-go)

