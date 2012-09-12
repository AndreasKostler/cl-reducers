;;;; cl-reducers.lisp

(in-package #:cl-reducers)
(annot:enable-annot-syntax)

;; Some cruft

(defun curry-right (function &rest args)
  "Curry the ARGS to FUNCTION, placing ARGS last."
  (lambda (&rest more-args)
    (apply function (append more-args args))))

(defmacro defcurried (name args doc &optional body)
  (flet ((do-curried (name args doc body)
	   (let ((cargs (butlast args))
		 (larg (last args))
		 (x (gensym)))
	     `(defun ,name  (,@cargs &optional ,@larg) 
		,doc
		(if ,@larg
		    ,body
		    (lambda (,x) (,name ,@cargs ,x)))))))
    (do-curried name args doc body)))

(defmacro reducer (coll &rest body)
  `(lambda (op init)
     (let ((reduce 
	    (if (or (consp ,coll) (arrayp ,coll))
		(curry-right 'reduce :initial-value init)
		(lambda (op x) (funcall x op init)))))
       (funcall reduce ,@body ,coll))))

(defun negp (x)
  (< x 0))


@export
(defun comp (&rest fns)
  "Takes a set of functions and returns a lambda that is the composition
                  of those functions.  The returned lambda takes a variable number of args,
                  applies the rightmost of functions to the args, the next function (right-to-left) to the result, etc."
  (cond ((not fns) #'identity)
	((= 1 (length fns)) (lambda (&rest args) (apply (first fns) args)))
	((= 2 (length fns)) (lambda (&rest args) (funcall (first fns) (apply (second fns) args))))
	((= 3 (length fns)) (lambda (&rest args) (funcall (first fns) (funcall (second fns) (apply (third fns) args)))))
	(T (let ((fns (reverse fns)))
	     (lambda (&rest args)
	       (labels ((do-fns (ret fns)
			  (if fns
			      (do-fns (funcall (first fns) ret) (rest fns))
			      ret)))
		 (do-fns (apply (first fns) args) (rest fns))))))))

@export
(defun reduce1 (op generator &key (initial-value nil initial-value-p))
  (funcall generator op (if initial-value-p initial-value (funcall op))))

;; Define reducers

@export
(defcurried rfilter (pred coll)
  "Retains values in the reduction of coll for which (pred val)
  returns logical true"
  (reducer coll
	   (lambda (ret x)
	     (if (funcall pred x)
		 (funcall op ret x)
		 ret))))

@export
(defcurried rmap (f coll)
  "Applies f to every value in the reduction of coll."
  (reducer coll
	   (lambda (ret x)
	     (funcall op ret (funcall f x)))))

@export
(defcurried rmapcat (f coll)
  "Applies f to every value in the reduction of coll, concatenating the result
  colls of (f val)."
  (reducer coll
	   (lambda (ret x)
	     (reduce op (funcall f x) :initial-value ret))))

@export
(defcurried rremove (pred coll)
    "Removes values in the reduction of coll for which (pred val)
  returns logical true. Foldable."
    (rfilter (complement pred) coll))

@export
(defcurried rdrop (n coll)
  "Elides the first n values from the reduction of coll."
  (reducer coll
     (let ((cnt n))
       (lambda (ret x)
	 (decf cnt)
	 (if (negp cnt)
              (funcall op ret x)
              ret)))))
 


;;; "cl-reducers" goes here. Hacks and glory await!


