---
title: Type class language extensions
date: 2019-03-19
authors: ajmcmiddlin
---

In this post I'm going to look at a few GHC language extensions related to Haskell's type class
mechanism, including what they do, and what their risks are, if any. This post is a literate
Haskell file and is available in the
[QFPL blog repo](https://github.com/qfpl/blog/blob/master/content/posts/type-class-extensions.lhs).
If you want to check your understanding of the material, I encourage you to load it up in GHCi and
play with it. In particular, enabling and disabling different language extensions can be helpful to
get a clear picture of what each extension allows.

Here are the extensions we're going to cover.

\begin{code}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
\end{code}

Hands up if you've ever seen a compiler error like this:

```
type-class-extensions.lhs:3:3: error:
    • Too many parameters for class ‘Foo’
      (Enable MultiParamTypeClasses to allow multi-parameter classes)
    • In the class declaration for ‘Foo’
  |
3 | > class Foo a b where
  |   ^^^^^^^^^^^^^^^^^^^...
```

We've written some code involving a type class `Foo`, and GHC is telling us that it's invalid unless
we enable an extension. In this case, `MultiParamTypeClasses`. Keep your hand up if you've done
what GHC says without being 100% sure what the extension is actually doing? Yeah, thought so.

The more I've learned about Haskell and GHC's extensions, the less comfortable I am with this
situation. Let's see if we can get a better understanding of some extensions related to type
classes. Before we dig into the extensions, let's look at what a type class is according to the
Haskell 2010 standard.

<h3>Type classes in Haskell 2010</h3>

[Section 4.3.1 of the standard](https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-750004.3)
of the standard covers type classes. To summarise, it says that a type class declaration must have
the following form.

```haskell
class cx => C u where cdecls
```

Looking at each part, type class declarations:

 - _may_ have a context (`cx => `);
 - _must_ have a class name (`C`);
 - _must_ be parameterised over exactly one type (`u`); and
 - _may_ declare one or more members (`where cdecls`).

Some examples are:

```haskell
class Eq a => Ord a where
  compare :: a -> a -> Ordering
  ...

class Show a where
  show :: a -> String
  ...

class (Ord a, Show a) => ShOrd a
```

`Ord` and `Show` are two common type classes. `ShOrd` is one I made up, and may look a little weird
given it doesn't declare any members. Its purpose is to group `Ord` and `Show` constraints into one
class. This can avoid some boilerplate if you find yourself listing many constraints again and
again.

<h3>Type class instances in Haskell 2010</h3>

[Section 4.3.2 of the standard](https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-750004.3)
covers type class instance declarations. In short, it says that a type class instance must have the
following form.

```haskell
instance cx′ => C (T u1 … uk) where { d }
```

An instance declaration:

  - may have a context --- `cx' =>`;
  - must mention the class name --- `C`;
  - must mention the type the instance is for --- `T u1 … uk`.

Furthermore, `T` must be a type constructor and each type variable `u1 … uk` must be distinct.

<h3>`MultiParamTypeClasses`</h3>

Suppose we want to write a type class that is parameterised over two variables. In this case, we're
going to look at `MonadReader` from the `mtl` package. For those not familiar, `MonadReader` allows
us to express a polymorphic type for monads that work like the `Reader` monad. That is, they contain
some environment that is always available without explicitly passing it around as a function
argument.

\begin{code}
class Monad m => MonadReader' r m where
  ask' :: m r
\end{code}

This isn't valid Haskell 2010, because the type class `MonadReader` is parameterised over two type
variables --- `r` and `m`. As the name suggests, the `MultiParamTypeClasses` extension allows us to
write this class.

<h3>`FlexibleInstances`</h3>

**TL;DR**: `FlexibleInstances` relaxes rules about what is a valid instance declaration. For
example, with `FlexibleInstances` turned on an instance may include a type variable more than once,
or use concrete types in place of variables.

Now we've got a type class over multiple parameters, let's take it for a spin!

\begin{code}
instance MonadReader' r ((->) r) where
  ask' = id
\end{code}

Oh no! We have an error.

```
type-class-extensions.lhs:123:10-32: error:
    • Illegal instance declaration for ‘MonadReader' r ((->) r)’
        (All instance types must be of the form (T a1 ... an)
         where a1 ... an are *distinct type variables*,
         and each type variable appears at most once in the instance head.
         Use FlexibleInstances if you want to disable this.)
    • In the instance declaration for ‘MonadReader' r ((->) r)’
    |
123 | instance MonadReader' r ((->) r) where
```

Remember when we looked at what Haskell 2010 says is a valid type class instance, and it specified
that each type variable in the instance head could appear at most once. We've broken that rule here
because `r` appears multiple times. We've also broken the rule that says each type in an instance
head must be of the form `T a1 ... an`, where `T` is a type constructor and not a type variable. The
first type in the instance head, `r`, is a type variable and not a concrete type constructor as
required by Haskell 2010. `FlexibleInstances` lifts these restrictions, and allows us to write such
instances.

Likewise, if we wanted to write an instance for a type whose constructor is applied to one or more
concrete types, we need `FlexibleInstances` enabled.

\begin{code}
class Twizzle a where
  twizzle :: a -> Int

instance Twizzle Integer where
  twizzle = fromInteger

instance Twizzle (Maybe Integer) where
  twizzle = maybe 42 twizzle
\end{code}

The `Twizzle` instance for `Maybe Integer` isn't valid in Haskell 2010, as `Maybe` is applied to the
concrete type `Integer` and not a simple type variable. Again, `FlexibleInstances` relaxes this
constraint and allows these instances.

<h4>`FlexibleInstances`' dark side</h4>

I've seen a few places on the internet mention that `FlexibleInstances` is a benign extension that
comes without risk. It turns out that's not exactly true. Here's an example that produces a
`Data.Set` containing duplicate values, and gives no warnings when compiled with `-Wall` enabled.
As we'll see, this code uses no other extensions, and nothing outside of `base`.

It's worth noting that sets containing duplicate instances can be created _without_ enabling
`FlexibleInstances`, however it requires orphan instances, which will produce a warning and are
considered bad practice by many.


```haskell
-- FIA.hs
module FIA where

data A = A1 | A2 deriving (Eq, Ord, Show)

data Whoopsie a b c =
  Whoopsie a b c
  deriving (Eq, Show)
```

```haskell
-- FIB.hs
{-# LANGUAGE FlexibleInstances #-}

module FIB where

import Data.Set (Set, insert)
import FIA

data B = B deriving (Eq, Ord, Show)

instance Ord c => Ord (Whoopsie A B c) where
  compare (Whoopsie a1 b1 c1) (Whoopsie a2 b2 c2) =
    compare a1 a2 <> compare b1 b2 <> compare c1 c2

insB :: Ord c => Whoopsie A B c -> Set (Whoopsie A B c) -> Set (Whoopsie A B c)
insB = insert
```

```haskell
-- FIC.hs
{-# LANGUAGE FlexibleInstances #-}

module FIC where

import Data.Set (Set, insert)
import FIA

data C = C deriving (Eq, Ord, Show)

instance Ord b => Ord (Whoopsie A b C) where
  compare (Whoopsie a1 b1 c1) (Whoopsie a2 b2 c2) =
    compare a2 a1 <> compare b1 b2 <> compare c1 c2

insC :: Ord b => Whoopsie A b C -> Set (Whoopsie A b C) -> Set (Whoopsie A b C)
insC = insert
```

```haskell
-- Main.hs
module Main where

import Data.Set (Set, empty)

import FIA
import FIB
import FIC

test :: Set (Whoopsie A B C)
test =
  insB (Whoopsie A1 B C) . insC (Whoopsie A1 B C) . insC (Whoopsie A2 B C) $ empty

main :: IO ()
main =
  print test
```

Here's what happens when we compile and run this code.

```
ghc --version
The Glorious Glasgow Haskell Compilation System, version 8.4.4

`> ghc -Wall -fforce-recomp Main.hs -o whoopsie
[1 of 4] Compiling FIA              ( FIA.hs, FIA.o )
[2 of 4] Compiling FIB              ( FIB.hs, FIB.o )
[3 of 4] Compiling FIC              ( FIC.hs, FIC.o )
[4 of 4] Compiling Main             ( Main.hs, Main.o )
Linking whoopsie ...

`> ./whoopsie
fromList [Whoopsie A1 B C,Whoopsie A2 B C,Whoopsie A1 B C]``
```

<h4>`FunctionalDependencies`</h4>

We've now got a type class with multiple parameters thanks to `MultiParamTypeClasses`, and an
instance of that class that has the same type variable multiple times in the instance head thanks to
`FlexibleInstances`. Let's get to using our instance.

\begin{code}
foo' ::
  Integer
foo' =
  (+ 1) <$> ask' $ 100
\end{code}

What's this? Another error?

```
type-class-extensions.lhs:275:13-16: error:
    • Ambiguous type variable ‘t0’ arising from a use of ‘ask'’
      prevents the constraint ‘(MonadReader'
                                  Integer ((->) t0))’ from being solved.
      Probable fix: use a type annotation to specify what ‘t0’ should be.
      These potential instance exist:
        one instance involving out-of-scope types
        (use -fprint-potential-instances to see them all)
    • In the second argument of ‘(<$>)’, namely ‘ask'’
      In the expression: (+ 1) <$> ask'
      In the expression: (+ 1) <$> ask' $ 100
    |
275 |   (+ 1) <$> ask' $ 100
    |             ^^^^
type-class-extensions.lhs:275:20-22: error:
    • Ambiguous type variable ‘t0’ arising from the literal ‘100’
      prevents the constraint ‘(Num t0)’ from being solved.
      Probable fix: use a type annotation to specify what ‘t0’ should be.
      These potential instances exist:
        instance Num Integer -- Defined in ‘GHC.Num’
        instance Num Double -- Defined in ‘GHC.Float’
        instance Num Float -- Defined in ‘GHC.Float’
        ...plus two others
        (use -fprint-potential-instances to see them all)
    • In the second argument of ‘($)’, namely ‘100’
      In the expression: (+ 1) <$> ask' $ 100
      In an equation for ‘foo'’: foo' = (+ 1) <$> ask' $ 100
    |
275 |   (+ 1) <$> ask' $ 100
```

That's funny. `foo' :: Integer`, so shouldn't GHC's type inference know what the type of `ask'` and
the literal `100` should be here? Let's dig in.

`ask' :: MonadReader r m => m r`, and in this case `r ~ Integer` 

