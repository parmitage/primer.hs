Primer
======
Primer is a simple functional programming language written for fun. Pull it from [github](http://github.com/parmitage/primer3). If you have any comments about Primer then please [email](mailto:philip.armitage@gmail.com) me.

To start the REPL, change to the directory where you unpacked the tarball and type:

    ./primer

To load an existing Primer source file pass it as a command line argument:

    ./primer MyFile.pri

Bindings are immutable.

    pi = 3.14159

The body of a function is a single expression, the value of which is the return value.

    areaOfCircle = fn (r) pi * r * r

Functions are first class objects, can be passed anonymously and are higher order.

    simpleOpSquared = fn (f, x) f(x) * f(x)
    simpleOpSquared(fn (x) x / 3, 12)

Primer supports closures.

    makeAdder = fn (y) fn (a) y + a
    add2 = MakeAdder(2)
    add2(2)

__let__ introduces local definitions.

    let x = 12
    x + 4

Lists can be nested and are heterogeneous.

    xs = [4, [5.32, [pi], [], true], AreaOfCircle, 'a', "aaa"]

Several functions provide access to list elements.
 
    head(xs)
    tail(xs)
    last(xs1)
    length(xs)

A list can by accessed by index with the __at__ operator.

    xs at 4

An item can be prepended to a list with the __cons__ operator.

    1 :: [2,3,4]

To concatenate two lists use the __append__ operator.

    [1,2,3] ++ [4,5,6]

Strings are just lists of characters.

    Head("hello")
    "hello" ! 3

The type of a value can be tested with the __is__ operator.

    pi is float

The __as__ operator converts between types.

    123.45 as string
    "123.45" as float

The __if__ statement is an expression so the __else__ branch is mandatory.

    count = fn (xs)
       if xs != []
       then 1 + count(Tail(xs))
       else 0

Tail-recursive functions are optimised as in this accumulator version of Count.

    count = fn (xs)
       let counter = fn (a, xs)
          if xs != []
          then counter(a + 1, Tail(xs))
          else a
       counter(0, xs)

Primer has a modest standard library which can be found in __Library.pri__.

__map__ applies a function to every element in a list and returns a new list containing the transformed values.

    map(fn (x) 2 * x, [1,2,3,4,5])          # [2,4,6,8,10]

__foldl__ and __foldr__ uses a two parameter function to combine successive list elements.

    add = fn (x, y) x + y
    foldl(add, 0, [1,2,3,4,5])              # ((((0+1)+2)+3)+4)+5 = 15
    foldr(add, 0, [1,2,3,4,5])              # 1+(2+(3+(4+(5+0)))) = 15

__filter__ applies a predicate function to each element of a list returning those elements for which the function returns true.

    filter(odd, [1,2,3,2,4,5,6])            # [1,3,5]

__zip__ takes two lists and combines them pairwise to return a new list of 2-tuples.

    zip(l1, [4,5,6])                        # [[1,4],[2,5],[3,6]]

__reverse__ returns a new list in reverse order.

    reverse("hello")                        # "olleh"

__intersperse__ takes an atom and a list and returns a new list with the atom interspersed between the original list elements.

    intersperse('A', l1)                    # [1,'A',2,'A',3]

__take__ returns the first n items from a list.

    take(2, [1,2,3,4])                      # [1,2]

__drop__ returns a list without its first n items.

    drop(1, [1,2,3,4])                      # [2,3,4]

__takeWhile__ evaluates each item in a list in turn using a supplied function and returns the items from the list until the function returns false.

    takeWhile(even, l1)                     # []

__dropWhile__ evaluates each item in a list in turn using a supplied function and drops the items from the list until the function returns false.

    dropWhile(odd, l1)                      # [2,3]

__any__ returns true if any items in a list passes the supplied predicate function otherwise it returns false.

    any(even, [1,2,3,4])                    # true

__all__ returns true if all items in a list passes the supplied predicate function otherwise it returns false.

    all(odd, [3,4,5,6])                     # false

__min__ returns the smallest item in a list. It requires that the list contains items which can be compared.

    min([7,2,4,5,3,8,6])                    # 2

__max__ returns the largest item in a list. It requires that the list contains items which can be compared.

    max([7,2,4,5,3,8,6])                    # 8

__sum__ returns the sum of the items in the list. It requires that the items in the list are numeric.

    sum([1,2,3,4,5])                        # 15

__product__ returns the product of the items in the list. It requires that the items in the list are numeric.

    product([1,2,3,4,5])                    # 120

__sort__ is an implementation of the Quicksort algorithm.

    sort([4,2,8,1])                         # [1,2,4,8]