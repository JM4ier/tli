(def true 1)
(def else true)

(def id (.\ (x) x))

(def if (m\ (expr then else)
    `(cond
        (#expr #then)
        (true  #else)
    )
))

(if (= 0 0) 1 2)

(def fold (.\ (init fun list)
    (if (pair? list) 
        (fold (fun init (hd list)) fun (tl list))
        init
    )
))

(def concat (.\ (xs ys)
    (cond
        ((nil? xs) ys)
        (true (cons (hd xs) (concat (tl xs) ys)))
    )
))

(def rev.aux (.\ (items revd)
    (cond
        ((nil? items) revd)
        (true (rev.aux (tl items) (cons (hd items) revd)))
    )
))
(def rev (.\ (ls)
    (rev.aux ls nil)
))

(def iter.aux (.\ (reps fun acc val)
    (cond
        ((> 1 reps) acc)
        (true (iter.aux (- reps 1) fun (cons val acc) (fun val)))
    )
))
(def iter (.\ (reps fun val)
    (rev (iter.aux reps fun nil val))
))


(fold 0 + '(1 2 3 4 5))
(rev (concat '(1 2 3) '(a b c)))


(def inc (.\ (x) (+ 1 x)))
(rev (iter 30 inc 1))


(def partial-add (.\ (a) (.\ (b) (+ a b))))
(partial-add 4)

(def defmacro (m\ (name args body)
    `(def #name (m\ #args #body))
))

(defmacro defun (name args body)
    `(def #name (.\ #args #body))
)

(defun square(x) (* x x))
(square 9)

(defun fib.aux(f1 f2 n)
    (if (= n 0)
        f1
        (fib.aux f2 (+ f1 f2) (- n 1))
    )
)
(defun fib(n) (fib.aux 0 1 n))
(fib 20)

(defun sort.join(bwd fwd)
    (cond
        ((nil? bwd) fwd)
        (else (sort.join (tl bwd) (cons (hd bwd) fwd)))
    )
)
(defun sort.insert(ys xs x)
    (cond
        ((nil? xs)      (sort.join ys (list x)))
        ((< x (hd xs))  (sort.join ys (cons x xs)))
        (else           (sort.insert (cons (hd xs) ys) (tl xs) x))
    )
)
(defun sort.tr(unsorted sorted)
    (cond
        ((nil? unsorted) sorted)
        (else (sort.tr (tl unsorted) (sort.insert nil sorted (hd unsorted))))
    )
)
(defun sort(xs) (sort.tr xs nil))

(def long-list (rev (iter 500 inc 1)))
long-list
(sort long-list)
