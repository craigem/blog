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
}
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

We can then write the two cases as implementation. Note the recursion in the implementation of `OneAnd`.


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
  
  public X List<X>(X empty, Func<A, X, X> oneThen) {
    return oneThen(v, w.List(empty, oneThen));
  }
}
```

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
