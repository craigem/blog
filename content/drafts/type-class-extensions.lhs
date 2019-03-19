---
title: Type class language extensions
date: 2019-03-19
authors: ajmcmiddlin
---

\begin{code}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
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
we enable an extension. In this case, `MultiParamTypeClasses`.

Keep your hand up if you've done what GHC says without being 100% sure what the extension is
actually doing? Yeah, thought so.

In this post I'm going to look at a few GHC language extensions related to Haskell's type class
mechanism, including what they do, and what their risks are, if any. Before we dig into extensions
of the type class system though, let's look at what a type class is according to the Haskell 2010
standard.

<h3>Type classes in Haskell 2010</h3>

[Section 4.3.1 of the standard](https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-750004.3)
of the standard covers type classes. To summarise, it says that a type class declaration must have
the following form.

```haskell
class cx => C u where cdecls
```
It may have a context (`cx => `), must have a class name `C`, and must have exactly one type
variable `u` that denotes the type of the instance. Following this is an optional list of
declarations (`cdecls`) specifying any members of the class.

Some concrete examples are the `Eq` and `Ord` classes.

```haskell
class Eq a => Ord a where
  compare :: a -> a -> Ordering
  ...

class Show a where
  show :: a -> String
  ...

class (Ord a, Show a) => ShOrd a
```

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

Suppose we want to write a type class that is parameterised over two variables.

\begin{code}
class Has s a where
  get :: s -> a
\end{code}

This isn't valid Haskell 2010, because the type class `MonadReader` is parameterised over two type
variables --- `r` and `m`. As the name suggests, the `MultiParamTypeClasses` extension allows us to
write this class.

That's it, that's all there is to `MultiParamTypeClasses`.

<h3>`FlexibleInstances`</h3>

**TL;DR**: `FlexibleInstances` relaxes rules about what is a valid instance head. For example, with
`FlexibleInstances` turned on an instance may include a type variable more than once, or use
concrete types in place of variables.

Now we've got a type class over multiple parameters, let's take it for a spin!

\begin{code}
instance Has (a,b) a where
  get = fst

instance Has (a,b) b where
  get = snd
\end{code}

Oh no! We have some errors. It turns out we have the same error twice, so I'll only show you the
first one.

```
type-class-extensions.lhs:81:10: error:
    • Illegal instance declaration for ‘Has (a, b) a’
        (All instance types must be of the form (T a1 ... an)
         where a1 ... an are *distinct type variables*,
         and each type variable appears at most once in the instance head.
         Use FlexibleInstances if you want to disable this.)
    • In the instance declaration for ‘Has (a, b) a’
   |
81 | instance Has (a,b) a where
   |          ^^^^^^^^^^^
```

Remember when we looked at what Haskell 2010 says is a valid type class instance, and it specified
that each type variable in the instance head had to be a unique type variable. We've broken that
rule here. `FlexibleInstances` lifts this restriction, and allows us to write such instances.

Likewise, if we wanted to write an instance for a type whose constructor is applied to one or more
concrete types, we need `FlexibleInstances` enabled.

\begin{code}
class Show' a where
  show' :: a -> String

instance Show' Integer where
  show' = show

instance Show' (Maybe Integer) where
  show' Nothing = "Nothing"
  show' (Just n) = "Just " ++ show' n
\end{code}

The `Show'` instance for `Maybe Integer` isn't valid in Haskell 2010, as `Maybe` is applied to the
concrete type `Integer` and not a simple type variable.


<h3>`FunctionalDependencies`</h3>





