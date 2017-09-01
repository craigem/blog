---
title: Thinking Differently
date: 2017-07-28
authors: ielliott
---

Here's a programming exercise. Feel free to attempt it in your language of choice. You'll
implement a number of functions of increasing difficulty.

1.
   ### Challenge
   Write a function `sumEvens(List<int>) -> int`, that sums all the even numbers stored in
   one of your language's built-in list type.
   
   If your language has a built-in function for this, please ignore it and re-implement it
   yourself.
   
   ### Tests
   
   * `sumEvens([]) == 0`
   * `sumEvens([1]) == 1`
   * `sumEvens([-1]) == -1`
   * `sumEvens([1, 2]) == 3`
   * `sumEvens([2, 1]) == 3`
   
2.
   ### Challenge
   Write a function `chainNegating(List<Function<Bool, Bool>>) -> Function<Bool, Bool>` that 
   chains the input functions from left to right, but negates the output of each function in
   the list before chaining it into the next one. e.g.
   `chainNegating([a, b, c])(x)` will be the same as `not(c( not(b( not(a(x)) )) ))`
   
   ### Tests
   * `chainNegating([])(x) == x`
   * `chainNegating([a => a && True, a => a || False])(True) == True`
   * `chainNegating([a => xor(a, False), a => a && False])(False) == True`
   
3. 
   ### Setup
   Implement a generic binary tree, `BinTree<A>`. A `BinTree<A>` is either a `Leaf`
   containing a single `A`, or a `Branch` containing an `A` and two `BinTree<A>`s.
   Here's an example of a `BinTree<int>`: `Branch(1, Branch(2, Leaf(4), Leaf(5)), Branch(3, Leaf(6), Leaf(7)))`,
   which can be visualized as follows:
   ```
         1
        / \
       /   \
      /     \
     2       3
    / \     / \
   4   5   6   7 
   ```
   
   ### Challenge
   Write a function `lengthProduct(BinTree<string>) -> int` that computes the product
   of the lengths of the strings stored in the input.
  
   ### Tests
   * `lengthProduct(Leaf("hello")) == 5`
   * `lengthProduct(Branch("a", Leaf("b"), Leaf("c"))) == 1`
   * `lengthProduct(Branch("a", Leaf("bb"), Leaf("ccc"))) == 6`
   * `lengthProduct(Branch("a", Branch("bb", Leaf("dddd"), Leaf("eeee")), Leaf(""))) == 0`
     
4.
   ### Challenge
   The three challenges above all share a common 'essence': visiting all the elements of a
   structure in order, transforming them each element, and using the result of each transformation
   to accumulate a final result.
   
   Your goal is to write a function `visitAccumulate` which generalizes `sumEvens`, `chainNegating`
   and `lengthProduct`. The number of arguments and their types are up to you, as long as there
   is some configuration of arguments such that it behaves *exactly* the same as the
   aforementioned functions.

   This system also needs to be extensible: adding extra behavior should not require modifying
   `visitAccumulate`'s source code, or the source of any related helpers.
   
   ### Tests
   Since this is more of a free-form software engineering task, it is harder to validate your
   solution. In addition to mimicking `sumEvens`, `chainNegating`, and `lengthProduct`, with the
   correct answer `visitAccumulate` will also be able to:
   * Behave like `sumEvens`, but for odd numbers
   * Behave like `sumEvens`, but computes the product instead of the sum
     (the empty list gives a product of 1)
   * Output a list of all the values in a `BinTree<A>`
   * Concatenate a list of strings into a single string
   * Concatenate a list of lists into a single list
   Despite the contrived nature of challenges 1-3, a good solution to this challenge will give rise
   to very natural, useful behaviors.
