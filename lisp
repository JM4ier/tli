(def true 1)
(def else true)

(def not nil?)

(def ls list)

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

(def defmacro (m\ (name args body)
    `(def #name (m\ #args #body))
))

(defmacro defun (name args body)
    `(def #name (.\ #args #body))
)

(defmacro assert(condition)
    `(cond
        (#condition #condition)
        (else (panic '#condition))
    )
)

(assert (= '(1 2 3 4 5 6) (concat '(1 2 3) '(4 5 6))))


(defun flatten(xss)
    (cond
        (xss (fold (hd xss) concat (tl xss)))
    )
)

(assert (= 
    '(1 2 3 4)
    (flatten '((1 2) (3) nil (4) nil nil))
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

(assert (= nil (rev nil)))
(assert (= '(1) (rev '(1))))
(assert (= '(2 1) (rev '(1 2))))

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

(defmacro assert_msg(condition msg)
    `(cond
        (#condition nil)
        (else (panic '(failed: #condition because #msg)))
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
    (flatten (list '(progn)

    ;; 'type checking' - checks if it is a struct instance of this type
    `((defun #(symcat name '?) (instance) 
        (cond
            ((nil? (pair? instance)) nil)
            (else (= (el 0 instance) '#name))
        )
    ))

    ;; constructor of the struct
    `((typedfun #name #fields 
        #`(list '#name #(cons 'list (map snd fields)))
    ))

    ;; accessors of the struct
    (mapi
        (.\ (idx field) 
            `(defun #(symcat name '. (snd field)) (instance) 
                (el #idx (el 1 instance))
            )
        )
    fields)
    
    ;; editing fields of the struct
    (mapi
        (.\ (idx field) 
            `(defun #(symcat name '. (snd field) '!) (instance new_val) 
                #(cons name (mapi (.\ (val_idx field2)
                    (cond
                        ((= idx val_idx) 'new_val)
                        (else `(#(symcat name '. (snd field2)) instance))
                    )
                ) fields))
            )
        )
    fields)
)))

(defstruct Point(
    (int? x)
    (int? y)
))

'Point
Point.x
Point.y
Point.x!
Point.y!
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
        ((sym? xs)      (panic '(contains expects a list as 2nd arg)))
        ((int? xs)      (panic '(contains expects a list as 2nd arg)))
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

(defun remove_space(string)
    (filter (.\ (char) (/= (hd " ") char)) string)
)

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

(defun repeat(amt list)
    (cond
        ((= 0 amt) nil)
        (else (concat list (repeat (- amt 1) list)))
    )
)


(defun pipeline(init funs)
    (cond
        (funs (pipeline ((hd funs) init) (tl funs)))
        (else init)
    )
)

(defun unwrap(error value)
    (cond
        (value value)
        (else (panic error))
    )
)

(defmacro sg(instance field)
    `(pipeline #instance (list
        (filter
            (.\ (entry) (= (fst entry) '#field))
            ..
        )
        maybe_head
        (unwrap '(missing field #field on struct) ..)
        snd
    ))
)

(defun si.(instance field value)
    (cons (list field value) instance)
)

(defmacro si(instance field value)
   `(cond
        ((contains '#field (map fst #instance))
            (panic `(the field #field already exists in the struct #instance)))
        (else
            (si. #instance '#field #value))
    )
)

(defmacro su(instance field value)
    `(map
        (.\ (entry)
            (cond
                ((= '#field (fst entry))
                    (list '#field #value))
                (else entry)
            )
        )
        #instance
    )
)

(defun n_tuple?(n tup)
    (cond
        ((< n 0)        nil)
        ((= 0 n)        (nil? tup))
        ((pair? tup)    (n_tuple? (- n 1) (tl tup)))
    )
)

(defun new.(fields)
    (fold
       nil
       (.\ (struct new_field)
           (progn
               (assert_msg 
                   (n_tuple? 2 new_field) 
                  '(struct field should be a pair)) 
               (si. struct (fst new_field) (eval (snd new_field)))
           )
       )
       (rev fields)
    )
)

(defmacro sn(fields)
    (cond
        ((list? fields)
         `'#(fold
                nil
                (.\ (struct new_field)
                    (progn
                        (assert_msg 
                            (n_tuple? 2 new_field) 
                           '(struct field should be a pair)) 
                        (si. struct (fst new_field) (eval (snd new_field)))
                    )
                )
                (rev fields)
            )
        )
        (else
            (panic `(creating a new struct requires a list of key-value pairs (#fields))))
    )
)


(assert (n_tuple? 0 nil))
(assert (not (n_tuple? 1 nil)))
(assert (not (n_tuple? 2 '(1 2 3))))
(assert (n_tuple? 3 '(4 5 6)))

(defun fun?(fun)
    (cond
        ((bfun? fun) true)
        ((not (pair? fun)) nil)
        ((= '.\ (fst fun)) true)
    )
)

(assert (fun? int?))
(assert (fun? sym?))
(assert (fun? nil?))
(assert (fun? pair?))
(assert (fun? fun?))
(assert (not (fun? 1)))
(assert (not (fun? 'hehe)))

(defun gtup?(typ?s els)
    (cond
        ((and (nil? typ?s) (nil? els))
            true
        )
        ((and (pair? typ?s) (pair? els))
            (let ty? (fst typ?s) (progn
                (assert_msg (fun? ty?) (the type should be a function))
                (cond
                    ((not (ty? (fst els)))
                        nil)
                    (else
                        (gtup? (tl typ?s) (tl els)))
                )
            ))
        )
    )
)

(assert (gtup? (ls int? int?) '(1 2)))
(assert (not (gtup? (ls int? int?) '(1 2 3))))

(defun glist?(ty? xs)
    (cond
        ((not (fun? ty?))
            (panic `(ty? << #ty? >> should be a function.)))
        ((nil? xs)
            true)
        ((not (pair? xs))
            nil)
        ((not (ty? (hd xs)))
            nil)
        (else
            (glist? ty? (tl xs)))
    )
)

(assert (glist? int? '(1 2 3 4)))
(assert (glist? (.\ (_) nil) nil))
(assert (not (glist? int? '(1 2 3 4 five))))
(assert (not (glist? sym? '(1 2 3 4 five))))
(assert (glist? sym? '(five six seven)))
(assert (glist? sym? '(seven)))
(assert (glist? sym? nil))

; struct with each field having the same type
(defun mono_struct?(typ? val)
    (glist? (gtup? (ls sym? typ?) ..) val)
)

; struct where the fields have any type
(defun any_struct?(val)
    (mono_struct? any? val)
)

(assert (any_struct? nil))
(assert (any_struct? (sn nil)))
(sn ((x 1)))
(assert (any_struct? (sn ((x 1)))))

(defun struct?(typ val)
    (cond
        ; is the type actually a struct type?
        ((not (mono_struct? fun? typ))
            (panic `(type #typ is not a struct type)))

        ; is the value a struct?
        ((not (any_struct? val))
            nil)
        
        ; are the variable names the same **and in the same order?**
        ((/= (map fst typ) (map fst val))
            nil)

        ; check that the values correspond to the types
        (else
            (gtup? (map snd typ) (map snd val)))
    )
)

'struct
(def point (sn ((x 10) (y 20))))
point

(sn (
    (sfx (+ 1 2))
    (sfy 10)
))

(sg point x)

(gtup?
    (list int? int? int?)
    '(1 2 3)
)

(assert (=
    '(x y)
    (map fst (sn ((x 10) (y 10))))
))
(assert (=
    (map fst (sn ((x int?) (y int?))))
    (map fst (sn ((x 10) (y 10))))
))
(assert (gtup?
    (map snd (sn ((x int?) (y int?))))
    (map snd (sn ((x 10) (y 10))))
))
(assert (any_struct?
    (sn ((x int?) (y int?)))
))
(assert (any_struct?
    (sn ((x 10) (y 10)))
))

(assert (struct?
    (sn ((x int?) (y int?)))
    (sn ((x 10) (y 20)))
))

(defmacro type(name def)
   `(progn 
        (defun #(symcat name '?) (val)
            (struct? (sn #def) val)
        )
        (defmacro #(symcat name '.new) (vals)
           `(let instance (sn #vals) (progn
                (assert (##(symcat name '?) instance))
                instance
            ))
        )
    )
)

(type PPoint (
    (x int?)
    (y int?)
))

PPoint?
PPoint.new

(def q (PPoint.new(
    (x 10)
    (y 20)
)))
q
(PPoint? q)


'(end of program)
