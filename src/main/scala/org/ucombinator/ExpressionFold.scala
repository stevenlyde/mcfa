package org.ucombinator

object ExpressionFold {

  def apply[A](e: Exp, a: A, f: (A, Exp) => A): A = fold(e, a, f)

  private def fold[A](e: Exp, a: A, f: (A, Exp) => A): A = {
    val a1 = f(a, e)
    e match {
      case App(fun, args) =>
        val a2 = fold(fun, a1, f)
        val a3 = args.positionals.foldLeft(a2)((a, arg) => fold(arg.exp, a, f))
        args.keywords.foldLeft(a3)((a, arg) => fold(arg.exp, a, f))

      case Lambda(formals, ExpBody(body)) =>
        fold(body, a1, f)

      case Seq(SetBang(name, value), call) =>
        val a2 = fold(value, a1, f)
        fold(call, a2, f)

      case If(condition, ifTrue, ifFalse) =>
        val a2 = fold(condition, a1, f)
        val a3 = fold(ifTrue, a2, f)
        fold(ifFalse, a3, f)

      case Ref(name) => a1
      case Lit(value) => a1
      case Void() => a1
      case Undefined() => a1
    }
  }

}
