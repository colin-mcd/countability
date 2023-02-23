# Countability

Implements typeclasses, allowing you to enumerate all the elements of (potentially infinite) countable sets.

For example, to demonstrate the countability of the type `(Bool, Integer)`, you can do
```haskell
> take 20 (enumerate :: [(Bool, Integer)])
[(False,  0), (True,   0), (False,  1), (True,   1), (False, -1),
 (True,  -1), (False,  2), (True,   2), (False, -2), (True,  -2),
 (False,  3), (True,   3), (False, -3), (True,  -3), (False,  4),
 (True,   4), (False, -4), (True,  -4), (False,  5), (True,   5)]
> toIndex (False, -4)
16
```

You can see the `fst`s alternate `[False, True]` and the `snd`s follow the pattern `[0, 1, -1, 2, -2, ...]`.

Any type that is an instance of both `Bounded` and `Enum` can be easily derived:
```haskell
instance Countable Word where
  enumerate = deriveBoundedEnumerate
  toIndex = deriveBoundedIndex
  size = deriveBoundedSize -- if bounded; otherwise:
--size = unbounded
```

Supported higher-order type constructors include:
- Tuples: `()`, `(a, b)`, `(a, b, c)`...
- Sums: `Either a b`, `Maybe a`
- Lists\*: `[a]`

\* Note that this does **_not_** demonstrate the powerset of a countable set is itself countable, because it never reaches any infinite-size sets, e.g. Evens ∉ Enumerate(Naturals)

Extreme examples:
```haskell
> (enumerate :: [(Int, Int)]) !! 54321012
(1832,3380)
```
This example takes about a second on my computer. In the future, it'd be nice to have another function `fromIndex :: Integer -> a` alongside `enumerate :: [a]`, to avoid having to compute the first `54321011` elements when we just want one.

Going the other way, we get _huge_ numbers when we use `String` (i.e. `[Char]`) for two reasons. First, `Char` begins at `NUL` (= 0) and has to work all the way up to `d` (= 100), and second, enumerating lists is **extraordinarily** complex and a specific list will appear after a number of others that is exponential with respect to both the length of the list and the sum of the indices of its elements. So
```haskell
> toIndex "dog"
133499189745056880149688856635597007162669032647290798121690100488888732861290034376435130460514
```
Appears after about $2^{\mathrm{length}(dog) + \mathrm{toIndex}(d) + \mathrm{toIndex}(o) + \mathrm{toIndex}(g) - 1} = 2^{3 + 100 + 111 + 103 - 1} = 2^{316}$ other strings. Impressively, though, that takes only a fraction of a second to compute. However,
```haskell
> toIndex "doge"
676921312041214565326761275425557544784286395355423968547480366360991530225982818124993751490268451683933401113623918910450890
```
takes about 5 seconds on my computer (which, by the way, it's crazy that GHC's `Integer` (i.e. C's `BigNum`) works this well!)