(def true 1)
(def if (m\ (expr then else)
    `(cond
        (#expr #then)
        (true  #else)
    )
))
(def id (.\ (x) x))

(if nil 2 '(1 2))
(id '(1 2 3))
(def fold (.\ (init fun list)
    (if (pair? list) 
        (fold (fun init (hd list)) fun (tl list))
        init
    )
))
(fold 0 + '(1 2 3 4 5))