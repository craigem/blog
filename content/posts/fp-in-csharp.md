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
public sealed class True : Boolean {
  private True(){}
  public static readonly True x = new True();
  public X Boolean<X>(X fals, X tru) {
    return tru;
  }
}

public sealed class False : Boolean {
  private False(){}
  public static readonly False x = new False();
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
