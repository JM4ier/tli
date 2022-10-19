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