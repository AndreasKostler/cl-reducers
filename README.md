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


And finally some benchmarks (Lispworks):

```common-lisp
(defun reducers ()
  (loop with x = (make-array 100000 :element-type '(integer 0 100))
        repeat 1000
        do (reduce1 '+ (rmap '1+ x))))

(defun no-reducers ()
  (loop with x = (make-array 100000 :element-type '(integer 0 100))
        repeat 1000
        do (reduce '+ (map 'list '1+ x))))

(time (reducers))
Timing the evaluation of (TEST)

User time    =        5.890
System time  =        0.015
Elapsed time =        6.557
Allocation   = 193648 bytes
0 Page faults
NIL

(time (no-reducers))
Timing the evaluation of (TEST1)

User time    =        8.140
System time  =        0.125
Elapsed time =        8.444
Allocation   = 1600272144 bytes
0 Page faults
NIL
```



