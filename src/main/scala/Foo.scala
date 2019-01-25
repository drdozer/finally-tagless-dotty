//@FunctionalInterface
//trait Foo[T] {
//  def foo(msg: String): T
//}
//
//object Foo {
//  // uses SAM sugar
//  implicit def StringFoo: Foo[String] = msg => msg.reverse
//
//  // also uses SAM sugar
//  instance def IntFoo: Foo[Int] = _.length
//
//  // full declaration works
//  instance DoubleFoo of Foo[Double] {
//    override def foo(msg: String): Double = 1.0 / msg.length
//  }
//
//  // but SAM doesn't
//  instance FloatFoo of Foo[Float] = 1.0f / _.length
//}