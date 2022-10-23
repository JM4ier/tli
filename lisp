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

(defun find(target init next)
    (cond
        ((target init) init)
        (else (find target (next init) next))
    )
)

(defun all(condition collection)
    (cond
        ((nil? collection)                  true)
        ((nil? (condition (hd collection))) nil)
        (else                               (all condition (tl collection)))
    )
)

(defun range(from to)
    (cond
        ((= from to) nil)
        (else (cons from (range (+ 1 from) to)))
    )
)

(defun divides?(x y)
    (= 0 (% y x))
)

(defun prime?(x)
    (cond
        ((< x 2) nil)
        ((< x 4) true)
        (else
            (all (.\ (divisor) (nil? (divides? divisor x))) (range 2 x))
        )
    )
)

(defun filter(pred? list)
    (cond
        ((nil? list)        list)
        ((pred? (hd list))  (cons (hd list) (filter pred? (tl list))))
        (else               (filter pred? (tl list)))
    )
)

(defun map(fun list)
    (cond
        ((nil? list) list)
        (else (cons (fun (hd list)) (map fun (tl list))))
    )
)

(defmacro lc(expr for x in list where cond?)
    `(map (.\ (#x) #expr) (filter (.\ (#x) #cond?) #list))
)

(range 1 10)
(divides? 5 10)
(% 10 5)
(= 0 (% 10 5))
(filter prime? (range 1 100))
(find prime? 1000 inc)

(lc (* 2 x) for x in (range 1 10) where (prime? x))

'(end of program)