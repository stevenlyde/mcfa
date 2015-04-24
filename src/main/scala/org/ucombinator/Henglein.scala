package org.ucombinator

import scala.collection.immutable.{SortedMap, TreeMap}
import scala.collection.mutable.{Map => MutableMap}

trait Constraint

case class InequalityConstraint(c: Constructor, a: Int) extends Constraint

case class EqualityConstraint(a1: Int, a2: Int) extends Constraint


case class Constructor(val keywords: SortedMap[SKeyword, Int], val positionals: List[Int]) {

  def this() = this(TreeMap(), List())

  def apply(keyword: SKeyword): Int = keywords(keyword)

  def apply(position: Int): Int = positionals(position)

  def update(keyword: SKeyword, label: Int): Constructor =
    new Constructor(keywords + (keyword -> label), positionals)

  def ::(label: Int): Constructor =
    new Constructor(keywords, label :: positionals)

  val ctype: ConstructorType = ConstructorType(positionals.length, keywords.keySet)

}

case class ConstructorType(arity: Int, keywords: Set[SKeyword])


class Henglein(vs: Map[SName, Int]) {

  type Label = Int

  def labelOf(e: Exp) = e match {
    case Ref(name) => if (vs.contains(name)) vs(name) else e.label // use the label of the expression for primitives
    case _ => e.label
  }

  def argsConstructor(args: List[Argument], ctor: Constructor = new Constructor()): Constructor = args match {
    case Nil => ctor
    case hd :: tl => {
      val c = argsConstructor(tl, ctor)
      hd match {
        case PosArgument(exp) => labelOf(exp) :: c
        case KeywordArgument(keyword, exp) => c(keyword) = labelOf(exp)
      }
    }
  }

  def formalsConstructor(formals: Formals): Constructor = {
    val keywords = formals.keywords.foldLeft(TreeMap[SKeyword, Int]())((keywords, formal) =>
      keywords + (formal.keyword -> vs(formal.name)))
    val positionals = formals.positionals.map(formal => vs(formal.name))
    new Constructor(keywords, positionals)
  }

  def generateConstraints(e: Exp, cs: List[Constraint]): List[Constraint] = e match {

    case App(f, ListArguments(args)) =>
      val c = argsConstructor(args)
      val newConstraints = generateConstraints(f, InequalityConstraint(c, labelOf(f)) :: cs)
      args.foldLeft(newConstraints)((cs, arg) => arg match {
        case PosArgument(exp) => generateConstraints(exp, cs)
        case KeywordArgument(keyword, exp) => generateConstraints(exp, cs)
      })

    case Lambda(formals, ExpBody(body)) =>
      val c = formalsConstructor(formals)
      generateConstraints(body, InequalityConstraint(c, labelOf(e)) :: cs)

    case Seq(SetBang(name, value), call) =>
      val tmp = generateConstraints(value, EqualityConstraint(vs(name), labelOf(value)) :: cs)
      generateConstraints(call, tmp)

    case If(condition, ifTrue, ifFalse) =>
      val cs1 = generateConstraints(condition, cs)
      val cs2 = generateConstraints(ifTrue, cs1)
      generateConstraints(ifFalse, cs2)

    case Ref(name) => cs
    case Lit(value) => cs
    case Void() => cs
    case Undefined() => cs

  }

  def constructorConstraints(c1: Constructor, c2: Constructor): List[Constraint] = {
    val lst1 = for ((a1, a2) <- c1.positionals.zip(c2.positionals)) yield EqualityConstraint(a1, a2)
    val lst2 = for ((keyword, label) <- c1.keywords) yield EqualityConstraint(label, c2(keyword))
    lst1 ++ lst2
  }

  def updateConstructors(cs1: MutableMap[ConstructorType, Constructor],
                         cs2: MutableMap[ConstructorType, Constructor], a: Label): List[Constraint] = {
    var constraints = List[Constraint]()
    for ((t, c) <- cs2) {
      if (cs1.contains(t)) constraints = InequalityConstraint(c, a) :: constraints
      else cs1(t) = c
    }
    constraints
  }

  def normalize(cs: List[Constraint]): DisjointSet[Label] = {
    var worklist = cs

    val count = Term.allocateLabel() - 1
    val sets = new DisjointSet[Label](1 to count)
    val csets = new DisjointSet[Constructor](cs.collect { case InequalityConstraint(c, a) => c })
    val leqs = (1 to count).foldLeft(Map[Label, MutableMap[ConstructorType, Constructor]]())((leqs, label) => {
      leqs.updated(label, MutableMap())
    })

    while (!worklist.isEmpty) {
      val current = worklist.head
      worklist = worklist.tail
      current match {

        case InequalityConstraint(c, a) =>
          val cc = csets.find(c)
          val ca = sets.find(a)
          val constructors = leqs(ca)
          val constructorType = cc.ctype
          if (!constructors.contains(constructorType))
            constructors(constructorType) = cc
          else {
            val cl = constructors(constructorType)
            if (cc != cl) {
              csets.union(cc, cl)
              worklist ++= constructorConstraints(cc, cl)
            }
          }

        case EqualityConstraint(a1, a2) =>
          val ca1 = sets.find(a1)
          val ca2 = sets.find(a2)
          if (ca1 != ca2) {
            val ca = sets.union(ca1, ca2)
            worklist ++= updateConstructors(leqs(ca), leqs(if (ca == ca1) ca2 else ca1), ca)
          }
      }
    }

    sets
  }

  def run(exp: Exp) = {
    val cs = generateConstraints(exp, List())
    normalize(cs)
  }

}
