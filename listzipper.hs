-----------
-- Zipper
-----------

type Zipper a = ([a], [a])

toZipper :: [a] -> Zipper a
toZipper xs = (xs, [])

fwd :: Zipper a -> Zipper a
fwd ([], bs)     = ([], bs)
fwd (a : as, bs) = (as, a : bs)

bwd :: Zipper a -> Zipper a
bwd (as, [])     = (as, [])
bwd (as, b : bs) = (b: as, bs)