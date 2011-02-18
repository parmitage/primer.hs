w = 0
s = 1
e = 2
n = 3

rover = fn (x, y, h) [x, y, h]
roverX = fn (r) r at 0
roverY = fn (r) r at 1
roverH = fn (r) r at 2

print = fn (r)
   let x = roverX(r)
   let y = roverY(r)
   let d = ['W', 'S', 'E', 'N'] at roverH(r)
   [x, y, d]

navigate = fn (r, s) foldl(move, r, s)

move = fn (r, c)
   if c == 'L' or c == 'R'
   then rotate(r, c)
   else translate(r, c as int - '0' as int)

translate = fn (r, c)
   if even(roverH(r))
   then rover(roverX(r) + ((roverH(r) - 1) * c), roverY(r), roverH(r))
   else rover(roverX(r), roverY(r) + ((roverH(r) - 2) * c), roverH(r))

rotate = fn (r, d)
   if d == 'L'
   then rover(roverX(r), roverY(r), (roverH(r) + 1) mod 4)
   else rover(roverX(r), roverY(r), (if roverH(r) - 1 == 0
                                     then 4
                                     else roverH(r)) - 1)

print(navigate(rover(10, 10, n), "R1R3L2L1"))