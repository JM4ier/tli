(* 1 2 3 4 5 (+ 1 2 3))
(def pi 3)
(list? (quote (1 2 3)))
(def if (m\ (condition this-branch else-branch)(quasiquote
    (cond
        ((unquote condition) (unquote this-branch))
        (1 (unquote else-branch))
    )
)))
(if 1 2 3)