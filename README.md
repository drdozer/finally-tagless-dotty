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
So for example, the negation normal form context and the associated run instance are like this:

```scala
opaque type NNF[T] = Boolean => T

instance RunNNF[B] of RunDSL[NNF[B], B] { ... }
```

This declares that we can run a negation normal form context to give a value in some underlying representation.
The interpretation of the `Boolean` argument is clearly implementation-specific, and should not even be visible to
people evaluating an expression into the context.
Making it an opaque type hides this to the outside world.
They can't see or access this `Boolean`, and nor do they need to know what passing in `true` or `false` means.

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

This project is built using the add-witness version of dotty.
You can build this version of dotty by checking out the dotty project and adding in the staging repo.

```
git clone git@github.com:lampepfl/dotty.git
cd dotty
git submodule update --init
git remote add staging https://github.com/dotty-staging/dotty
git fetch staging add-witness
git checkout staging/add-witness
sbt dotty-bootstrapped/publishLocal
```

For more information on the sbt-dotty plugin, see the
[dotty-example-project](https://github.com/lampepfl/dotty-example-project/blob/master/README.md).
