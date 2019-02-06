## sbt project compiled with Dotty

This is a little test project to explore dotty features, and in particular how they interact with finally-tagless
encodings. The project makes use of several language features:

* typeclass instances and derivation
* partially applied type parameter lists
* SAMs (single abstract method) sugar
* opaque types
* `implied` `for` `given` syntax
* `given` function lambdas

### The Conceit

Finally tagless encodings are sold as the holy grail of software development design patterns. They provide a hard
abstraction between the syntax (the words being used) and the semantics (what actually happens when you use those
words).
It also addresses the question of extensibility of a core language, allowing multiple small languages to be composed as
needed, and existing operations generalised over the extended language.

Here we look at a toy domain of boolean logic.
We develop typeclasses for common logical conjunctions (and, or, not, ...) and a range of contexts within which these
can be evaluated.
Contexts include:

* Bool - the context of the boolean constants `true` and `false`.
* Stringify - the context of string representation, encoded as something that can be written to an `Appendable`.
* NNF - negation normal form, where negations are pushed as far as possible towards the terminals of an expression, and
double-negations eliminated

Contexts are modelled as types.
To reduce the overhead of boxing, these are often opaque types.
For example, the Stringify context is declared as:

```scala
  opaque type Stringify = Appendable => Unit
```

By using the dotty opaque type mechanic, we can completely hide the underlying type of `Stringify` without introducing
a new "real" type, or the overhead associated with boxing it into a case class instance.
By providing an instance of `Seimgroup` for these contextual types, where appropriate, client code can manipulate them
without needing to know anything about their implementation.

Each context also provides an instance of `RunDSL`, which abstracts the idea of 'running' a representation into a result
value.
For Stringify, running it means exposing the underlying function so that the user can then pass in an appendable.

```scala
implied RunStringify[B] for RunDSL[Stringify, Appendable => Unit] { ... }
```

Notice we're using the `implied` `for` syntax here to declare a value that can be derived during compilation.
In this case, the value is declared in-place as an implementation of `RunDSL` by putting the implementation directly
after the `for` type.

Running representations is always a no-args operation.
If the particular representation requires values to be supplied, then *running* constructs the function that can be
supplied those values. 

Evaluation contexts have two concerns.
The calling context sometimes needs to select which environment the value is to be made within, and the value returned
may need to be one of a range of distinct possibilies.
Let's look at the former first.

```scala
@FunctionalInterface
trait NNF[T] {
  def negationOf: T
  def terminal: T
}
```

This declares a negation normal form context.
It has two environments within which a term will evaluate itself.
In the `terminal` context, a term will evaluate itself as if it were a terminal with respect to the negation normal form
rewrite.
In the `negationOf` context, a term will evaluate itself as if it was being negated.

The implementation of `Not[NNF[T]]` is then trivial.

```scala
implied NNFNot[B] for Not[NNF[B]] = (lhs: NNF[B]) => new {
  override def negationOf: B = lhs.terminal
  override def terminal: B = lhs.negationOf
}
```

Notice that we've actually made use of the other variant of the `for` syntax here, to declare an `implied` value by
an expression rather than an implementation.
In this case, the expression is a function that will return a new implementation of `NNF` which flips the
terminal/negation logic of the expression handed to it.
As the `NNF` interface is declared as a SAM, the compiler takes care of wrapping up our function as an implementation.
You can see by just reading the code that this pushes negations to wards the leaves, and also eliminates
double-negation as it goes. 

Since every self-contained negation normal form expression starts being evaluated without negation, the `RunDSL`
implementation is also trivial.

```scala
implied RunNNF[B] of RunDSL[NNF[B], B] {
  override def runDSL(b: NNF[B]): B = NNF.run(b)
}
```

All that is left is to provide implementations for the other operators.
These are also trivial.
Terms that wrap other terms do whatever internal transforms are required to perform the negation re-write, and push
negation down to their children.
For example:

```scala
instance def NNFAnd[B] with (A: And[B], O: Or[B]): And[NNF[B]] = (lhs: NNF[B], rhs: NNF[B]) => new {
  override def terminal: B = A.and(lhs.terminal, rhs.terminal)
  override def negationOf: B = O.or(lhs.negationOf, rhs.negationOf)
}
```

Terminal terms, such as variables, take care to negate themselves.

```Scala

```

This stacking of representations allows us to do things like run a logical expression in the context `NNF[Stringify]` to
give a negation-normal form of a stringified result.
It allows us to literally rewrite the text representation of a boolean expression into negation normal form, which I
personally think is pretty cool.

The actual running of an expression is done through a helper method that has partially specified type members.
The `RuNDSL` object supplies a postfix `run` method to calculate the result, as a syntactic convenience.
So for example:

```scala
def notAndTNotF[B] with (A: And[B], N: Not[B], T: TruthValues[B]): B = 
  not(and(⊤, not(⊥)))

(notAndTNotF : NNF[Stringify]).run.run(System.out)
```

The expression is first evaluated in a specific context, in this case `NNF[Stringify]`.
This initial value is then `run` to perform the negation-normal form transformation.
This produces an intermediate result of the type `Stringify` which is then in turn run, producing an
`Appendable => Unit` which is then applied to `System.out` to actually print it. 
 
### Usage

Now that the implied/given/for syntax has been merged into trunk, this project is built using the dotty nighty build.

For more information on the sbt-dotty plugin, see the
[dotty-example-project](https://github.com/lampepfl/dotty-example-project/blob/master/README.md).
