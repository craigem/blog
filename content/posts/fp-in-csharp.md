---
title: Functional Programming in C#
date: 2019-02-19
authors: tmorris
---

In this article, we will look at some functional programming and data structure concepts, by demonstrating them using the C# programming language. Much of the code is for demonstration only, to help understand a concept. The code itself has some quirks that make it unusable in real code, but many of those quirks can be overcome (a topic for another time). The goal is to provide new tools and ideas to think about programs, which can be utilised in practice where appropriate.

#### Church-encoding data structures

Church-encoding is introduced here to also introduce some data structures, which will also be used later on, and it is easier (for demonstration) to use a church-encoding in C#. It is also an interesting topic in itself.

###### booleans

Let's start simply. Suppose you had to write your own boolean data type.

One could simply write:

```csharp
enum boolean { True, False };
```

However, the same data structure could also be written using an interface with an abstract function:

```csharp
interface Boolean {
  X Boolean<X>(X fals, X tru);
}
```

We can then write construction functions for `true` and `false` values. The function implementation returns one of its two arguments, depending on if we are denoting `false` or `true`.

```csharp
sealed class True : Boolean {
  private True(){}
  public static readonly True value = new True();
  public X Boolean<X>(X fals, X tru) {
    return tru;
  }
}

sealed class False : Boolean {
  private False(){}
  public static readonly False value = new False();
  public X Boolean<X>(X fals, X tru) {
    return fals;
  }
}
```

We can then write functions for our `Boolean` data type, such as `Negate`, which swaps the `true` or `false` value.

```csharp
static class BooleanExtension {
  public static Boolean Negate(this Boolean p) {
    return p.Boolean<Boolean>(True.value, False.value);
  } // ...
```

We can write a function that combines two boolean values using `And` which corresponds to the more familiar `&&` operation.

```csharp
  // ...
  public static Boolean And(this Boolean p, Boolean q) {
    return p.Boolean<Boolean>(False.value, q);
  } // ...
```

We could also write a "show" function, which is similar in function to `ToString` to display a boolean value:

```csharp
  // ...
  public static string Show(this Boolean p) {
    return p.Boolean<string>("false", "true");
  } // ...
```

or convert to a `bool` as it is more commonly used in C#:

```csharp
  // ...
  public static bool Bool(this Boolean p) {
    return p.Boolean<bool>(false, true);
  }
}
```

Essentially, we can replicate all the functionality of a boolean by this method, without information loss in either direction. We say that `Boolean` data type is *isomorphic* to `bool`, though it is implemented differently.

It might be pointed out that there is a slight difference in the evaluation of the function arguments. For example, the `&&` operation on `bool` does not evaluate its second argument if the first argument is `false`. This does not hold for our `Boolean` data type. We can resolve this discrepancy using `Func`:

```csharp
public static Boolean And(Boolean p, Func<Boolean> q) {
  return p.Boolean<Boolean>(False.value, q());
}
```

In summary, we can use church-encoding to implement data types. Let's look at some more data types

###### optional values

The option data type, sometimes called maybe, has seen a lot of discussion in recent years. It can be thought of intuitively as *a list with a maximum length of 1*. In other words, it has either 0 elements or it has 1 element.

In practical application, it can be thought of as a replacement for `null`. For example, instead of your methods returning `Bobble` which might be `null`, we instead return *a list of 0 or 1 `Bobble`s*.

How might be implement an optional data type? One way is with a church-encoding.

```csharp
interface Optional<A> {
  X Optional<X>(X zero, Func<A, X> one);
}
```

Here we have a data type with 0 or 1 `A` values, implemented as an abstract function that returns a generic `X` and that `X` is arrived at by either the first argument *denoting 0 `A` values*, or the second argument, which is a function that is called on the 1 `A` value.

Let's write the construction functionality for the two cases `Zero` and `One`.

```csharp
sealed class Zero<A> : Optional<A> {
  private Zero(){}

  public static Optional<A> value {
    get {
      return new Zero<A>();
    }
  }

  public X Optional<X>(X zero, Func<A, X> one) {
    return zero;
  }
}

sealed class One<A> : Optional<A> {
  private readonly A v;

  private One(A v) {
    this.v = v;
  }

  public static Optional<A> value(A a) {
    return new One<A>(a);
  }

  public X Optional<X>(X zero, Func<A, X> one) {
    return one(v);
  }
}
```

We can write some interesting functions on the `Optional` data type. For example, a function that takes every `A` in an `Optional<A>` and converts into a `B` using a function, producing an `Optional<B>`.

```csharp
static class OptionalExtensions {
  public static Optional<B> Select<A, B>(this Optional<A> x, Func<A, B> fn) {
    return x.Optional(Zero<B>.value, a => One<B>.value(fn(a)));
  } // ...
```

This function is relevant to [LINQ](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/concepts/linq/basic-linq-query-operations#selecting-projections), which we will talk about later.

It is also relevant to [the null-conditional (or null-propagation) operator](https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/operators/null-conditional-operators) which is denoted `?.` in regular syntax.

Let's look at an expression using this operator.

```csharp
string? n = person?.name
```

This expression first checks the `person` value and determines if there are 0 or 1 available, with 0 denoted by `null`. If there are 0 (it is `null`), then 0 string values are returned by assigning `n` to `null`. However, if there is 1 value available, then the `name` member is called on that value and assigned to `n`.

This is similar in functionality to what our `Select` function does. If we considered implementing our data types this way, the expression would become:

```csharp
Optional<string> n = person.Select(name);
```

where `name` is a function that returns the member of the object. It gets a little muddier though, because the `name` function might also return `null`, which is where the equivalence comes apart.

We can take care of this though.

```csharp
  // ...
  public static Optional<B> SelectMany<A, B>(this Optional<A> x, Func<A, Optional<B>> fn) {
    return x.Optional(Zero<B>.value, fn);
  } // ...
```

This function is similar to `Select` though it can do one extra thing. The function argument does not turn the `A` into a `B` like it did earlier, but instead, turns the `A` into *0 or 1* `B` values.

This means that if our `name` member might return `null`, this would correspond to using `SelectMany`:

```csharp
Optional<string> n = person.SelectMany(name);
```

We could continue to chain these functions, as they continue to return 0 or 1 values:

```csharp
Optional<string> n = person.SelectMany(name.SelectMany(middle.SelectMany(thirdLetter)));
```

We can write another function that returns the `A` value from the `Optional<A>` if it is there, or returns another given value if it is not.

```csharp
  // ...
  public static A Get<A>(this Optional<A> x, A a) {
    return x.Optional(a, q => q);
  }
}
```

We can think of this function as taking 0 or 1 `A` values and exactly 1 `A` value, then returns exactly 1 `A` value. If there is an `A` value contained in the first argument, it is returned; otherwise, the second argument is returned.

This corresponds very closely to the functionality of [the null-coalescing operator](https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/operators/null-coalescing-operator), which is denoted as `??` in C#.

Suppose we have a potentially nullable `int` value and we wish to get the `int` value out of it if it is there, or a default of `99` if it is not.

```csharp
int? x = ...;

int r = x ?? 99;
```

This corresponds to the `Get` function as follows:

```csharp
Optional<int> x = ...;

int r = x.Get(99);
```

We could continue writing useful functions for the `Optional` data type, as it gives potential for a rich API. Let's move on.

So far we have encoded two data structures:

* one of two values as `Boolean`
* a list of either 0 or 1 values as `Optional`

What about a list of 0 or *many* values?

###### lists

What is the church-encoding for lists? There is a mechanical way to calculate it, but we can also intuit it.

A list may be thought of as one of two cases:

* empty list, no elements
* one element and another list

Using this definition, we can construct any list of values. For example, the list `[1,2,3]` can be described as:

<div class="card mb-2"><div class="card-body">
"one element `1` and another list that is one element `2` and another list that is one element `3` and another list which is empty list."</div></div>

To parenthesise this statement to illustrate the grouping:

<div class="card mb-2"><div class="card-body">"one element `1` and another list that is (one element `2` and another list that is (one element `3` and another list which is empty list))."</div></div>

We can see that this is a *recursively* defined data type. That is to say, in the definition of a list, it references itself. This generally implies that the implementation of the recursive case (the second one) will be recursive.

Let's write this as a church-encoded data structure.

```csharp
interface List<A> {
  X List<X>(X empty, Func<A, X, X> oneAnd);
}
```

We can then write the two cases as implementation. Note the recursion in the implementation of `OneAnd#List`.


```csharp
class Empty<A> : List<A> {
  private Empty(){}

  public static List<A> value {
    get {
      return new Empty<A>();
    }
  }

  public X List<X>(X empty, Func<A, X, X> oneThen) {
    return empty;
  }
}

class OneAnd<A> : List<A> {
  private readonly A v;
  private readonly List<A> w;

  private OneAnd(A v, List<A> w) {
    this.v = v;
    this.w = w;
  }
  
  public static List<A> value(A v, List<A> w) {
    return new OneAnd<A>(v, w);
  }

  public X List<X>(X empty, Func<A, X, X> oneThen) {
    return oneThen(v, w.List(empty, oneThen));
  }
}
```

We can construct a list by one of two ways.

* calling `Empty#value` with no arguments
* calling `OneAnd#value` requiring arguments of one element and another list

Let's construct the list `[1,2,3]`.

```csharp
List<int> _123 = OneAnd<int>.value(1, OneAnd<int>.value(2, OneAnd<int>.value(3, Empty<int>.value)));
```

Similar to the `Optional` data type, we can also write some interesting functions on the `List` data type. For example, a function that takes every `A` in a `List<A>` and converts into a `B` using a function, producing a `List<B>`.

```csharp
static class ListExtensions {
  public static List<B> Select<A, B>(this List<A> x, Func<A, B> fn) {
    return x.List<List<B>>(Empty<B>.value, (a, b) => OneAnd<B>.value(fn(a), b));
  } // ...
```

Let's write a function to make a list with one element.

```csharp
  public static List<A> SingleList<A>(this A a) {
    return OneAnd<A>.value(a, Empty<A>.value);
  } // ...
```

We can write a function to append two lists together.

```csharp
  public static List<A> Append<A>(this List<A> x, List<A> y) {
    return x.List<List<A>>(y, OneAnd<A>.value);
  } // ...
```

Or, a function that appends two lists, for a function applied to each element in a list. This can be approximately thought of as a for-loop that builds up a new list inside the body of the list.

```csharp
  public static List<B> SelectMany<A, B>(this List<A> x, Func<A, List<B>> fn) {
    return x.List<List<B>>(Empty<B>.value, (a, b) => fn(a).Append(b));
  } // ...
```

This function can be thought of intuitively as, take every element in a list (of type `A`), apply a function that produces a new list (of type `List<B>`), then append all those lists together.

We can use this function to implement a URL encoder. That is, given a list of characters, if any of those characters need special encoding (e.g. space), we produce a new list of characters for that encoding.

```csharp
  public static List<char> f(List<char> url) {
    return url.SelectMany(c =>
      c == ' ' ?
        OneAnd<char>.value('%', OneAnd<char>.value('2', OneAnd<char>.value('0', Empty<char>.value))) :
      c == '<' ?
        OneAnd<char>.value('%', OneAnd<char>.value('2', OneAnd<char>.value('2', Empty<char>.value))) :
      // etc etc
      c.SingleList()
    );
  }
}
```

----

#### Summary

Let's summarise.

We have looked at church-encoding for data structures. We first encoded a `Boolean` data type. Next, we created the `Optional` data type, which may be thought of as a list with a maximum length of 1. We then encoded a list.

We implemented some interesting functions for these data types, then visited a use-case for `List#SelectMany` by implementing a URL encoder.

There are some emerging patterns here. In particular, the `Optional#Select` and `List#Select` functions are the same in type signature, but for the data type name itself. This can also be said about `Optional#SelectMany` and `List#SelectMany`. If we have a look in the base libraries, we will see others, such as `IEnumerable` and `IQueryable`. What other things fit this pattern?

----

#### `Select` and `SelectMany`

Let's look at another one. But first, let's more concretely define that pattern.

The `Select` function is defined on some data type name `T` such that it implements a function with the type:

```csharp
T<B> Select<A, B>(T<A>, Func<A, B>)
```

The `SelectMany` function is defined on some data type name `T` such that it implements a function with the type:

```csharp
T<B> Select<A, B>(T<A>, Func<A, T<B>>)
```

We take particular note of the fact that "things that can be `T`" are things that have exactly one generic argument. For example, `Optional` and `List`. We could not use say `int` which has no generic arguments. It would not make sense to use `int` because there is no sensible thing that is `int<A>`.

If we try to use things that have *two* generic arguments, these also do not fit. However, if we *apply one generic* to it, then that means we have a thing that takes one more generic.

For example, we have used `Func` which takes two generic arguments. If we give it one generic argument, we might be able to implement `Select` and `SelectMany`. Let us make this argument itself generic. We will call it `Q`.

Therefore, the type for `Select` will be:

```csharp
Func<Q, B> Select<Q, A, B>(Func<Q, A>, Func<A, B>)
```

Let's try writing this function.

```csharp
public static Func<Q, B> Select<Q, A, B>(Func<Q, A> x, Func<A, B> fn) {
  return q => fn(x(q));
}
```

Yes, we were able to achieve an implementation! Even more interesting, it is *the only possible* implementation for this type. That is to say, given this type, the implementation must do the thing that we have just written. This has some caveats; where we have ignored `null`, recursing forever, the `default` keyword and a few other nuances. The type signature led us to the implementation in this case.

What about `SelectMany`?

The structure of `SelectMany` is:

```csharp
T<B> Select<A, B>(T<A>, Func<A, T<B>>)
```

Therefore, our implementation will have this type:

```csharp
Func<Q, B> Select<Q, A, B>(Func<Q, A>, Func<A, Func<Q, B>>)
```

Can we write it?

```csharp
public static Func<Q, B> Select<Q, A, B>(Func<Q, A> x, Func<A, Func<Q, B>> fn) {
  return q => fn(x(q))(q);
}
```

Yep! And again, this type always leads to this implementation.

There are many other candidates that fit this pattern. Millions actually. This pattern has a canonical name.

Things that can implement `Select` are called *covariant functors*. The implementation must satisfy a couple of additional constraints to be called this, and our implementations do satisfy it.

Things that can implement `SelectMany` are called *monads*, again with a couple of additional constraints which have been satisfied.

What can we do with them?

----

#### LINQ

There are many components to [LINQ](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/concepts/linq/). We will focus on query operations; specifically those that utilise `Select` and `SelectMany`.


----

notes

C#

* church-encoded list
* Optional
* LINQ
  * traverse
  * liftA2
  * url-encoder
  * reader
* null-conditional/propagation operator ?. (Optional#fmap)
* null-coalescing operator ?? (fromOption)
* using both null-conditional and coalescing ~ SelectMany
* lens/store/tuples

* *insert usages, with commentary throughout*
