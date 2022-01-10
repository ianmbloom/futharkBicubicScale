let map_2d 'a 'b [h][w] (f:a->b) (grid:[h][w]a):[h][w]b =
    map (\(row: [w]a): [w]b ->
           map (\(x:a):b ->
                  f x
               ) row
        ) grid

let map_3d 'a 'b [h][w][j] (f:a->b) (grid:[h][w][j]a):[h][w][j]b =
    map (\(row: [w][j]a): [w][j]b ->
        map (\(cell:[j]a):[j]b ->
            map (\(x:a):b ->
                  f x
            ) cell
        ) row
    ) grid


let map2_2d 'a 'b 'c [h][w] (f:a->b->c) (aGrid:[h][w]a) (bGrid:[h][w]b):[h][w]c =
    map2 (\(aRow: [w]a) (bRow: [w]b) : [w]c ->
           map2 (\(x:a) (y:b) : c ->
                  f x y
               ) aRow bRow
        ) aGrid bGrid


let map3_2d 'a 'b 'c 'd [h][w]
            (f:a->b->c->d)
            (aGrid:[h][w]a)
            (bGrid:[h][w]b)
            (cGrid:[h][w]c)
            :[h][w]d =
    map3 (\(aRow: [w]a) (bRow: [w]b) (cRow: [w]c) : [w]d ->
           map3 (\(x:a) (y:b) (z:c): d ->
                  f x y z
               ) aRow bRow cRow
        ) aGrid bGrid cGrid

let tupleOp1 't (f:t->t) (a:(t,t,t,t)):(t,t,t,t) =
    let (a0, a1, a2, a3) = a
    in  (f a0, f a1, f a2, f a3)

let tupleOp2 't (f:t->t->t) (a:(t,t,t,t)) (b:(t,t,t,t)):(t,t,t,t) =
    let (a0, a1, a2, a3) = a
    let (b0, b1, b2, b3) = b
    in  (f a0 b0, f a1 b1, f a2 b2, f a3 b3)

let reduceTuple1 't (f:t->t->t) (a:t,b:t,c:t,d:t):t =
    f a (f b (f c d))
