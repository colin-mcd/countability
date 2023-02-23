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