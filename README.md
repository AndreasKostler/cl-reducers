* cl-reducers

Inspired by Rich Hickeys reducers for clojure described [here] and [here]

** Usage

No surprises here:

```common-lisp
  (reduce1 '+ (rfilter 'evenp '(1 2 3 4 5))
```

Operations are composable:

```common-lisp
  (let ((red (comp (rfilter 'evenp) (rmap '1+))))
       (reduce1 '+ (funcall red '(1 2 3 4 5))))
```


And finally some benchmarks (SBCL):

```common-lisp
(defun reducers ()
  (loop with x = (make-array 100000 :element-type '(integer 0 100))
        repeat 1000
        do (reduce1 '+ (rmap '1+ x))))

(defun no-reducers ()
  (loop with x = (make-array 100000 :element-type '(integer 0 100))
        repeat 1000
        do (reduce '+ (map 'list '1+ x))))
```



