(def true 1)
(def else true)

(def id (.\ (x) x))

(def if (m\ (expr then else)
    `(cond
        (#expr #then)
        (true  #else)
    )
))

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

(defun /= (x y) (nil? (= x y)))

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

(defun mapi.aux(fun list idx)
    (cond
        ((nil? list) list)
        (else
            (cons
                (fun idx (hd list))
                (mapi.aux fun (tl list) (+ idx 1))
            )
        )
    )
)
(defun mapi(fun list)
    (mapi.aux fun list 0)
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

(defmacro assert(condition)
    `(cond
        (#condition nil)
        (else (panic '#condition))
    )
)

(defmacro let (var value ctx)
    `((.\ (#var) #ctx) #value)
)

(defmacro lets (vars ctx)
    (cond
        ((nil? vars) ctx)
        (else
            (let more-vars (tl vars)
            (let var (el 0 (hd vars))
            (let val (el 1 (hd vars))
                `(lets #more-vars (let #var #val #ctx))
            )))
        )
    )
)

(defun init (ls)
    (cond
        ((nil? ls) (assert nil))
        ((nil? (tl ls)) nil)
        (else (cons (hd ls) (init (tl ls))))
    )
)

(defun last (ls)
    (cond
        ((nil? ls) (assert nil))
        ((nil? (tl ls)) (hd ls))
        (else (last (tl ls)))
    )
)


(defmacro seq.aux (lines ctx)
    `(lets #(map (.\ (line) `(_ #line)) lines) #ctx)
)
(defmacro seq (lines)
    `(seq.aux #(init lines) #(last lines))
)

(lets ( 
    (x 10)
    (x 20))
    x
)

(defun fst(ls) (el 0 ls))
(defun snd(ls) (el 1 ls))
(defun trd(ls) (el 2 ls))

(defun tc-arg(arg)
    `((nil? #arg) (panic (list '#arg 'instantiated 'with #(snd arg))))
)

(defmacro typedfun(name args body)
    `(defun #name #(map snd args)
        #(concat
            (list 'cond)
        (concat
            (map tc-arg args)
            (list (list 1 body))
        ))
    )
)

(typedfun plus
    (
        (int? x) 
        (int? y)
    )

    (+ x y)
)

(defun any?(x) 1)

(defmacro defstruct(name fields) 
    (cons 'progn
    (cons `(defun #(symcat name '?) (instance) 
        (cond
            ((nil? (pair? instance)) nil)
            ((= (el 0 instance) '#name) 1)
        )
    )
    (cons (list typedfun name fields `(list '#name #(cons 'list (map snd fields))))
        (mapi
            (.\ (idx field) 
                `(defun #(symcat name '. (snd field)) (instance) 
                    (el #idx (el 1 instance))
                )
            )
        fields)
))))

(defstruct Point(
    (int? x)
    (int? y)
))

Point.x
Point.y

Point

(def p (Point 111 222))
(Point.x p)
(Point? p)

(plus 1 2)


(let x 5 (* 2 x))

(symcat 'he 'he 'ho)

(typedfun max ((list? values))
    (fold 0 (.\ (x y) (cond ((< x y) y) (else x))) values)
)

(typedfun min ((list? values))
    (fold 1000000000 (.\ (x y) (cond ((< x y) x) (else y))) values)
)

(defun split.aux(delimiter array)
    (cond
        ((nil? array)
            (list nil nil))
        (else 
            (let res (split.aux delimiter (tl array))
            (cond
                ((/= delimiter (fst array))
                    (list (cons (fst array) (fst res)) (snd res)))
                (else
                    (list nil (cons (fst res) (snd res))))
            ))
        )
    )
)

(defun split (delimiter array)
    (snd (split.aux delimiter (cons delimiter array)))
)

(typedfun sum((list? x)) (fold 0 + x))
(defun succ(x) (+ 1 x))
(typedfun len((list? x)) (fold 0 succ x))
(defun pow(a b)
    (cond
        ((= 0 b) 1)
        (else (* a (pow a (- b 1))))
    )
)

(typedfun string->int ((list? str))
(let digit (- (hd str) (hd "0"))
(let res (tl str)
    (cond
        ((nil? res) digit)
        (else (+ 
            (string->int res)
            (*  digit 
                (pow 10 (len res))
            )
        ))
    )
)))

(defun take(n xs)
    (cond
        ((= n 0)
            nil)
        ((nil? xs)
            nil)
        (else
            (cons (hd xs) (take (- n 1) (tl xs))))
    )
)

(defun drop(n xs)
    (cond
        ((= n 0)
            xs)
        (else
            (drop (- n 1) (tl xs)))
    )
)

(defun contains(x xs)
    (cond
        ((nil? xs)      nil)
        ((= x (hd xs))  true)
        (else           (contains x (tl xs)))
    )
)

(defun mod(x y)
    (- x (* y (/ x y)))
)

(defun chunk.aux(size xs)
    (cond
        ((nil? xs)
            (list nil nil))
        (else
            (let rest (chunk.aux size (tl xs))
            (cond
                ((= size (len (fst rest)))
                    (list (list (hd xs)) (cons (fst rest) (snd rest))))
                (else
                    (list (cons (hd xs) (fst rest)) (snd rest)))
            ))
        )
    )
)

(typedfun chunk((int? size) (list? xs))
    (let sol (chunk.aux size xs)
    (filter id (cons (fst sol) (snd sol))))
)

(defun flatten(xss)
    (fold (hd xss) concat (tl xss))
)

(defun half(xs)
    (let half_len (/ (len xs) 2)
    (list
        (take half_len xs)
        (drop half_len xs)
    ))
)

(defun find_same(yss)
    (let xs (hd yss)
    (let ys (tl yss)
    (fst
    (filter 
        (.\ (x)
            (all (.\ (y) (contains x y)) ys))
        xs)
    ))))
)

(defun and(a b)
    (cond (a (cond (b true))))
)

(defun range(begin end)
    (iter (- end begin) succ begin)
)

(def in
    (map (.\ (l) (drop 6 l))
    (filter id
    (split 10 
    input)))
)

(defun remove_space(string)
    (filter (.\ (char) (/= (hd " ") char)) string)
)

(defun parse_valve(line)
    (let name (take 2 line)
    (let flow 
        (string->int
        (fst
        (split (hd ";")
        (snd
        (split (hd "=")
        line)))))
    (let conns
        (map remove_space
        (split (hd ",")
        (drop 24
        (snd
        (split (hd ";")
        line)))))
    `(#name #flow #conns))))
)
(def valves
    (map parse_valve in)
)

(def name fst)
(def flow snd)
(def conn trd)

(defun maybe_head(list)
    (cond
        ((nil? list) nil)
        (else (hd list))
    )
)

(defun or (x y)
    (cond
        (x x)
        (1 y)
    )
)

(defun map.new() nil)
(defun map.get_or(m key default)
    (or (map.get m key) default)
)
(defun map.get_all(m key)
    (map snd
    (filter (.\ (entry) (= key (fst entry)))
    m))
)
(defun map.get(map key)
    (maybe_head (map.get_all map key))
)
(defun map.update(m key fun)
    (map.ins m key (maybe_head (map fun (map.get_all m key))))
)
(defun map.update_or(map key default fun)
    (map.ins map key (fun (map.get_or map key default)))
)
(defun map.rem(map key)
    (filter (.\ (entry) (/= key (fst entry))) map)
)
(defun map.ins(map key val)
    (cons
        `(#key #val)
        (map.rem map key)
    )
)
(defun map.keys(m)
    (map fst m)
)
(def infty 10000000)

(def key fst)
(def val snd)

'valves
valves

(def names (map name valves))
'names
names

(defun repeat(amt list)
    (cond
        ((= 0 amt) nil)
        (else (concat list (repeat (- amt 1) list)))
    )
)

(defun make2(x) `(#x #x))

(def edges (flatten (map (.\ (vertex) (map (.\ (neighbor) (list (name vertex) neighbor)) (conn vertex))) valves)))
(def dist0 (fold (map.new) (map.ins .. .. 0) (map make2 names)))

(defun smoler(a b)
    (cond
        ((< a b) a)
        (else    b)
    )
)

dist0

(fold
    dist0
    (.\ (dist edge)
        (fold
            dist
            (.\ (d entry) 
                (let from (fst edge)
                (let to   (snd (key entry))
                (let len  (+ 1 (val entry))
                (let e   `(#from #to)
                (map.update_or d e infty (smoler .. len))
            )))))
            (filter (.\ (entry) (= (snd edge) (fst (key entry)))) dist)
        )
    )
    (repeat 30 edges)
)

(= edges (repeat  1 edges))

'(end of program)
