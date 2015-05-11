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

  var label: Int = 0
  def setLabel(label: Int) { this.label = label }

  lazy val index = ConstructorType.indexOf(positionals.length, keywords.keySet)

}

case class ConstructorType(arity: Int, keywords: Set[SKeyword])

object ConstructorType {

  var count = 0
  val ids = MutableMap[ConstructorType, Int]()

  def indexOf(arity: Int, keywords: Set[SKeyword]): Int = {
    ids.getOrElseUpdate(ConstructorType(arity, keywords), {
      val i = count
      count += 1
      i
    })
  }

  def getCount = count

}

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
      c.index
      val newConstraints = generateConstraints(f, InequalityConstraint(c, labelOf(f)) :: cs)
      args.foldLeft(newConstraints)((cs, arg) => arg match {
        case PosArgument(exp) => generateConstraints(exp, cs)
        case KeywordArgument(keyword, exp) => generateConstraints(exp, cs)
      })

    case Lambda(formals, ExpBody(body)) =>
      val c = formalsConstructor(formals)
      c.index
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

  def updateConstructors(cs1: Int, cs2: Int, leqs: Array[Constructor], constructorTypeCount: Int): List[Constraint] = {
    var constraints = List[Constraint]()
    for (i <- 0 until constructorTypeCount) {
      if (leqs(cs2 * constructorTypeCount + i) != null) {
        if (leqs(cs1 * constructorTypeCount + i) != null) {
          constraints = InequalityConstraint(leqs(cs2 * constructorTypeCount + i), cs1) :: constraints
        }
        else {
          leqs(cs1 + i) = leqs(cs2 + i)
        }
      }
    }
    constraints
  }

  def normalize(cs: List[Constraint]): ArrayDisjointSet = {
    var worklist = cs

    val labelCount = Term.allocateLabel() - 1
    val constructorTypeCount =  ConstructorType.getCount

    val constructorCount = cs.length
    val constructors = new Array[Constructor](constructorCount)
    for ((InequalityConstraint(c, a), i) <- cs.zipWithIndex) {
      c.setLabel(i)
      constructors(i) = c
    }

    val sets = new ArrayDisjointSet(labelCount + 1)
    val csets = new ArrayDisjointSet(constructorCount)

    val leqs = new Array[Constructor]((labelCount + 1) * constructorTypeCount)

    while (!worklist.isEmpty) {
      val current = worklist.head
      worklist = worklist.tail
      current match {

        case InequalityConstraint(c, a) =>
          val cc = constructors(csets.find(c.label))
          val ca = sets.find(a)
          if (leqs(ca * constructorTypeCount + cc.index) == null)
            leqs(ca * constructorTypeCount + cc.index) = cc
          else {
            val cl = constructors(csets.find(leqs(ca * constructorTypeCount + cc.index).label))
            if (cc != cl) {
              csets.union(cc.label, cl.label)
              worklist ++= constructorConstraints(cc, cl)
            }
          }

        case EqualityConstraint(a1, a2) =>
          val ca1 = sets.find(a1)
          val ca2 = sets.find(a2)
          if (ca1 != ca2) {
            val ca = sets.union(ca1, ca2)
            worklist ++= updateConstructors(ca, if (ca == ca1) ca2 else ca1, leqs, constructorTypeCount)
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
