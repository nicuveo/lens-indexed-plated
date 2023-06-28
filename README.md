# Indexed `Plated`

## What is `Plated`?

`lens` provides `Plated`, whose purpose is to help with the traversal of _self-recursive_ types, such as trees. The `Plated` class itself is straightforward:

```haskell
class Plated a where
  plate :: Traversal' a a
```

That is, given some value `a`, provide a `Traversal` that visits all of its immediate self-similar children. A motivating example, taken directly from `Plated`'s documentation, would be the following:

```haskell
data Expr
  = Val Int
  | Neg Expr
  | Add Expr Expr

instance Plated Exepr where
  plate f = \case
    Neg e   -> Neg <$> f e
    Add x y -> Add <$> f x <*> f y
    Val n   -> pure $ Val n
```

Using the combinators provided alongside `Plated`, it is now easy to traverse this recursive structure without having to write the boiler_plate_ ourselves:

```haskell
simplify :: Expr -> Expr
simplify = rewrite \case
  Neg (Neg x)         -> Just x
  Neg (Val n)         -> Just $ Val $ negate n
  Add (Val 0) x       -> Just x
  Add x (Val 0)       -> Just x
  Add (Neg x) (Neg y) -> Just $ Neg $ Add x y
  _                   -> Nothing
```

A limitation of `Plated`, however, is that the function that is expected by the `Traversal` only takes the value `a` itself, without context: in our argument above, the argument to `rewrite` operates on _any_ `Expr`, without knowing whether it is operating at the root of the expression, or at the very bottom of it.

But, in more complicated cases, it can sometimes be useful to track where we are in the larger data structure. For instance, while introspecting a JSON value, it could be useful to know the path that led to the current value.

## Introducing `IndexedPlated`

This library introduces `IndexedPlated`, which is just like `Plated` but carries around an "index" of the user's choosing. The definition of `IndexedPlated` is as follows:

```haskell
class IndexedPlated i a where
  iplate :: i -> IndexedTraversal' i a a
```

If we inline the type aliases (and simplify the result), the difference becomes clearer:

```haskell
 plate ::      (     a -> f b) -> s -> f t
iplate :: i -> (i -> a -> f b) -> s -> f t
```

`iplate` takes an additional "starting index" on top of the root value, and the given function also expects the index matching the given value.

This library also provides most of the combinators that accompany `Plated`. All of them are prefixed with `i`, similarly to how `lens` indicate indexed functions (such as `iover`).

For instance, for Aeson's `Value`, we could write the following:

```haskell
type Path = [Step]
data Step = Key Text | Index Int

instance IndexedPlated Path Value where
  iplate parentPath f = \case
    Object o -> flip traverseWithKey o \key value ->
      f (parentPath <> [Key key]) value
    Array  a -> for (indexed a) \(index, value) ->
      f (parentPath <> [Index index]) value
    String s -> pure $ String s
    Number x -> pure $ Number x
    Bool   b -> pure $ Bool b
    Null     -> pure Null
```

This could be used for error reporting; for instance, this function attempts to replace all strings starting with a `$` by corresponding values from a lookup table. Despite no recursion being present in this function, the underlying use of `IndexedPlated` means that all such possible expansions will be performed, recursively (preventing infinite recursions from recursive anchors is left as an exercise to the reader).

```haskell
expandAnchors
  :: MonadError (Path, Text) m
  => HashMap Text Value
  -> Value
  -> m Value
expandAnchors anchors =
  flip irewriteM [] \path value -> runMaybeT do
    text        <- hoistMaybe $ value ^? _String
    ('$', name) <- hoistMaybe $ uncons text
    case lookup name anchors of
      Nothing  -> throwError (path, name)
      Just res -> pure res
```

## Caveats and limitations

Since indices are user-defined, there is no way for this library to provide a default implementation to `iplate` which, ironically, results in some boilerplate.
