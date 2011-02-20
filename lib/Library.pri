reverse = fn (xs)
   let inner = fn (xs, accum)
      if head(xs) != []
      then inner(tail(xs), head(xs) :: accum)
      else accum
   in inner(xs, [])

map = fn (f, xs)
   let inner = fn (xs, accum)
      if head(xs) != []
      then inner(tail(xs), f(head(xs)) :: accum)
      else reverse(accum)
   in inner(xs, [])

foldl = fn (f, init, xs)
   if head(xs) != []
   then foldl(f, f(init, head(xs)), tail(xs))
   else init

min = fn (xs) head(sort(xs))
max = fn (xs) head(reverse(sort(xs)))
last = fn (xs) head(reverse(xs))
odd = fn (n) n mod 2 != 0
even = fn (n) n mod 2 == 0
bitSet = fn (n, b) (n & (1 << b)) > 0