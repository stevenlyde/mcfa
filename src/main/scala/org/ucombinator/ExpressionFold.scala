package org.ucombinator

class ExpressionFold() {

  type Variable = SName
  type Label = Int

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

  private def update(vs: Map[Variable, Label], f: Formal) =
    if (vs.contains(f.name)) vs else vs.updated(f.name, f.label)

  def label(e: Exp): Map[Variable, Label] = {
    fold(e, Map[Variable, Label](), (vs: Map[Variable, Label], e: Exp) => {
      e.label
      e match {
        case Lambda(formals, ExpBody(body)) =>
          val vs1 = formals.positionals.foldLeft(vs)(update)
          formals.keywords.foldLeft(vs1)(update)
        case _ => vs
      }
    })
  }

  def types(e: Exp): LabelTypes = {
    val ts = new LabelTypes(Set(), Set(), Set())
    fold(e, ts, (ts: LabelTypes, e: Exp) => e match {
      case Lambda(_, _) => ts.addLambda(e.label)
      case Lit(SBoolean(_)) => ts.addBoolean(e.label)
      case _ => ts
    })
  }

  def print(e: Exp) {
    fold(e, (), (_: Unit, e: Exp) => println(s"${e.label}: $e"))
  }

}

class LabelTypes(lambdas: Set[Int], booleans: Set[Int], primitives: Set[Int]) {

  def addLambda(label: Int) = new LabelTypes(lambdas + label, booleans, primitives)

  def addBoolean(label: Int) = new LabelTypes(lambdas, booleans + label, primitives)

  def addPrimitive(label: Int) = new LabelTypes(lambdas, booleans, primitives + label)

  def isLambda(label: Int) = lambdas.contains(label)

  def isBoolean(label: Int) = lambdas.contains(label)

  def isPrimitive(label: Int) = lambdas.contains(label)

}
