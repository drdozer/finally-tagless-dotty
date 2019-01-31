## sbt project compiled with Dotty

This is a little test project to explore dotty features, and in particular how they interact with finally-tagless
encodings. The project makes use of several language features:

* typeclass instances and derivation
* partially applied type parameter lists
* opaque types
* instance of with syntax

### The Conceit

Finally tagless encodings are sold as the holy grail of software development design patterns. They provide a hard
abstraction between the syntax (the words being used) and the semantics (what actually happens when you use those
words).
It also addresses the question of extensibility of a core language, allowing multiple small languages to be composed as
needed, and existing operations generalised over the extended language.

Here we look at a toy domain of boolean logic.
There are typeclasses for common logical conjunctions (and, or, not, ...) and a range of contexts within which these can
be evaluated. Contexts include:

* Bool - the context of the boolean constants `true` and `false`.
* Stringify - the context of string representation, encoded as something that can be written to an `Appendable`.
* NNF - negation normal form, where negations are pushed as far as possible towards the terminals of an expression, and
double-negations eliminated

Contexts are modelled as opaque types. For example, the Stringify context is declared as:

```scala
  opaque type Stringify = Appendable => Unit
```

By using the dotty opaque type mechanic, we can completely hide the underlying type of `Stringify` without introducing
a new "real" type, or the overhead associated with boxing it into a case class instance.

Each context also provides an instance of `RunDSL`, which abstracts the idea of 'running' a representation into a final
value.
For Stringify, running it means exposing the underlying function so that the user can then pass in an appendable.

```scala
instance RunStringify[B] of RunDSL[Stringify, Appendable => Unit] { ... }
```

Evaluation contexts have two concerns.
The calling context sometimes needs to select which environment the value is to be made within, and the value returned
may need to be one of a range of distinct possibilies.
Let's look at the former first.

```scala
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
instance def NNFNot[B]: Not[NNF[B]] = (lhs: NNF[B]) => new {
  override def negationOf: B = lhs.terminal
  override def terminal: B = lhs.negationOf
}
```

That is, whenever a negation is evaluated within a negated context, return the non-negated child, and
whenever a negation is evaluated in the non-negated context, evaluate the negation of the child.
This pushes negation down towards the leaves, and self-evidently eliminates double-negations on the way. 

And since every self-contained negation normal form expression starts being evaluated without negation, the `RunDSL`
instance is also trivial.

```scala
instance RunNNF[B] of RunDSL[NNF[B], B] {
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
get a `Stringify`, and then in turn run that to get an `Appendable => Unit` which can then be written out.
It allows us to literally rewrite the text representation of a boolean expression into negation normal form, which I
personally think is pretty cool.

The actual running of an expression is done through a helper method that has partially specified type members.
So for example:

```scala
def notAndTNotF[B] with (A: And[B], N: Not[B], T: TruthValues[B]): B = 
  not(and(⊤, not(⊥)))

runDSL[Rep = Stringify](
  runDSL[Rep = NNF[Stringify]](
    notAndTNotF)
      ) apply System.out
```

This will run a DSL value with a representation in `Stringify` which happens to be itself the result of running an
value in the `NNF` representation, where that value is `notAndTNotF`.
As you can see, we have essentially a complete decoupling between the syntactic expression `notAndTNotF` and how it is
interpreted when we choose to run it.
 
### Usage

Now that the instance/for/with syntax has been merged into trunk, this project is built using the dotty nighty build.

For more information on the sbt-dotty plugin, see the
[dotty-example-project](https://github.com/lampepfl/dotty-example-project/blob/master/README.md).
