package org.ucombinator

/* Syntax */


object Term {
  private var maxLabel : Int = 0

  def allocateLabel() : Int = {
    maxLabel += 1
    maxLabel
  }
}

object Exp {
  def let (exp : Exp) (k : Exp => Exp) = {
    exp match {
      case _ : Lit => k(exp)
      case _ : Ref => k(exp)
      case _ : Void => k(exp)
      case _ : Undefined => k(exp)
      case _ => {
        val tmp = SName.gensym("$tmp")
        new Let(tmp,exp,k(Ref(tmp)))
      }
    }
  }
}


abstract class Exp extends Ordered[Exp] {
  lazy val label = Term.allocateLabel()

  def isPure : Boolean ;
  def mustHalt : Boolean ;
  
  def isDuplicable : Boolean ;

  def isAtomic = isPure && mustHalt

  def free : Set[SName] ;

  def compare (that : Exp) = this.label compare that.label
}

abstract class Def {
  lazy val label = Term.allocateLabel()

  def name : SName ;
  def value : Exp ;
}


case class Program(val defs : List[Def]) {
  lazy val label = Term.allocateLabel()

  override def toString = defs mkString "\n\n"
  
  def toExp : Exp = {
    defs match {
      case List() => {
        Void()
      }
      case List(ImplicitDef(exp)) => {
        exp
      }
      case _ => {
        val bindings = Bindings(defs map (d => Binding(d.name, d.value)))
        LetRec(bindings,Body(List(),List(Ref(defs.last.name))))
      }
    }
  }
}

abstract class Formals {
  lazy val label = Term.allocateLabel()

  def positionals : List[PosFormal] ;

  def keywords : List[KeywordFormal] ;

  lazy val keywordSet : Set[SKeyword] = Set() ++ (keywords map (_.keyword))

  def :: (formal : Formal) : Formals ;

  def accepts (args : Arguments) : Boolean ;

  def bound : Set[SName] ;
}

abstract class Formal {
  val name : SName ;
  lazy val label = Term.allocateLabel()
}

case class PosFormal(val name : SName) extends Formal {
  override def toString = name.toString
}

case class KeywordFormal(val keyword : SKeyword, val name : SName) extends Formal {
  override def toString = keyword + " " + name.toString
}


case class VarFormals(rest : SName) extends Formals {
  override def toString = rest.toString
  def :: (formal : Formal) = MultiFormals(List(formal),rest)
  lazy val bound = Set(rest)

  val positionals = List()
  val keywords = List()

  def accepts (args : Arguments) : Boolean = (args.keywords.length == 0)
}

case class MultiFormals(formals : List[Formal], rest : SName) extends Formals {
  override def toString = "(" + (formals mkString " ") + " . " + rest + ")"
  def :: (formal : Formal) = MultiFormals(formal :: formals, rest)
  lazy val bound = Set(rest) ++ (formals map (_.name))
  
  private val (_positionals,_keywords) = formals partition (_.isInstanceOf[PosFormal])
  
  val positionals = _positionals.asInstanceOf[List[PosFormal]]
  val keywords = _keywords.asInstanceOf[List[KeywordFormal]]

  def accepts (args : Arguments) : Boolean = {
    if (args.positionals.length < this.positionals.length) 
      return false

    this.keywordSet == args.keywordSet
  }
}

case class ListFormals(formals : List[Formal]) extends Formals {
  override def toString = "(" + (positionals mkString " ") + " " + (keywords mkString " ") + ")"
  def :: (formal : Formal) = ListFormals(formal :: formals)
  lazy val bound = Set() ++ (formals map (_.name))

  private val (_positionals,_keywords) = formals partition (_.isInstanceOf[PosFormal])
  
  val positionals = _positionals.asInstanceOf[List[PosFormal]]
  val keywords = _keywords.asInstanceOf[List[KeywordFormal]]

  def accepts (args : Arguments) : Boolean = {
    if (args.positionals.length != this.positionals.length) 
      return false

    this.keywordSet == args.keywordSet
  }
}



abstract class Arguments {
  lazy val label = Term.allocateLabel()
  def positionals : List[PosArgument] ;
  def keywords : List[KeywordArgument] ;
  lazy val keywordSet : Set[SKeyword] = Set() ++ (keywords map (_.keyword))
  def :: (argument : Argument) : Arguments ;
  def free : Set[SName] ;
}

object InternalPrimArguments extends ListArguments(List(PosArgument(Undefined())))

abstract class Argument {
  lazy val label = Term.allocateLabel()
  def exp : Exp ;
  def free : Set[SName] ;
}

case class PosArgument(val exp : Exp) extends Argument {
  override def toString = exp.toString
  lazy val free : Set[SName] = exp.free
}

case class KeywordArgument(val keyword : SKeyword, val exp : Exp) extends Argument {
  override def toString = keyword + " " + exp.toString
  lazy val free : Set[SName] = exp.free
}

case class ListArguments(val args : List[Argument]) extends Arguments {
  override def toString = (positionals mkString " ") + " " + (keywords mkString " ")
  def :: (argument : Argument) : Arguments = 
    ListArguments(argument :: args)

  lazy val free = Set() ++ (args flatMap (_.free))

  private lazy val (_positionals,_keywords) = args partition (_.isInstanceOf[PosArgument])

  lazy val positionals = _positionals.asInstanceOf[List[PosArgument]]
  lazy val keywords = _keywords.asInstanceOf[List[KeywordArgument]]
}


case class Body (val defs : List[Def], val exps : List[Exp]) {
  lazy val label = Term.allocateLabel()

  override def toString = {
    (defs mkString " ") + " " +
    (exps mkString " ") 
  }

  def toLetRec : Exp  = {
    val bindings = Bindings(defs map (d => Binding(d.name, d.value)))
    LetRec(bindings,Body(List(),exps))
  }

  lazy val free : Set[SName] = {
    val bound = defs map (_.name)
    val free : Set[SName] = Set() ++ (defs flatMap (_.value.free))
    (free ++ (exps flatMap (_.free))) -- bound
  }
}


object ExpBody {
  def apply(exp : Exp) : Body = {
    exp match {
      case Begin(body @ Body(List(),List(_))) => body
      case _ => Body(List(),List(exp))
    }
  }
  def unapply(body : Body) : Option[Exp] = body match {
    case Body(List(),List(exp)) => Some(exp)
    case _ => None
  }
}




case class ImplicitDef(val value : Exp) extends Def {
  override def toString = value.toString
  lazy val name = SName.gensym("_") 
}

case class VarDef(val name : SName, val value : Exp) extends Def {
  override def toString = "(define "+name+" " +value.toString+")"
}

case class FunctionDef(val name : SName, val formals : Formals, val body : Body) extends Def {
  override def toString = "(define "+name+" (lambda "+formals+" " +body.toString+"))"

  def value = Lambda(formals,body)
}



// Core syntax:
case class Ref(val name : SName) extends Exp {
  override def toString = name.toString
  
  def isPure = true 
  def mustHalt = true 

  def isDuplicable = true 

  lazy val free : Set[SName] = Set(name)
}

abstract case class Lit(val sexp : SExp) extends Exp {
  def isPure = true 
  def mustHalt = true 

  lazy val free : Set[SName] = Set()
}

class SelfLit(val value : SExp) extends Lit(value) {
  override def toString = sexp.toString

  def isDuplicable = true 
}

object SelfLit {
  def apply(value : SExp) = new SelfLit(value)
}

class QuoteLit(val value : SExp) extends Lit(value) {
  override def toString = "(quote " +value.toString+ ")"
  
  def isDuplicable = value match {
    case _ : SSymbol => true
    case _ : SInt => true
    case _ : SNil => true
    case _ => false
  }

  def expansion : Exp = {
    value match {
      case hd :+: tl => ConsExp(QuoteLit(hd).expansion,QuoteLit(tl).expansion)
      case _ => this
    }
  }
}

object QuoteLit {
  def apply(value : SExp) = new QuoteLit(value)
}

case class App(val fun : Exp, val args : Arguments) extends Exp {
  override def toString = "(" + fun + " " + args + ")"

  def this (fun : Exp, args : Exp*) = 
    this (fun,ListArguments(args.toList map (PosArgument(_))))

  def isPure = false
  def mustHalt = false

  def isDuplicable = false 

  lazy val free = fun.free ++ args.free
}

case class Lambda(val formals : Formals, val body : Body) extends Exp {
  override def toString = "(lambda "+formals+" "+body+")"

  def isPure = true // -- the expression, not the function itself.
  def mustHalt = true // -- the expression, not the function itself.

  def isDuplicable = false 

  lazy val free = body.free -- formals.bound
}

class ULambda(fs : Formals, b : Body) extends Lambda(fs,b)

object ULambda {
  def apply(fs : Formals, b : Body) = new ULambda(fs, b)
}

class KLambda(fs : Formals, b : Body) extends Lambda(fs,b)

object KLambda {
  def apply(fs : Formals, b : Body) = new KLambda(fs, b)
}

case class If(val condition : Exp, ifTrue : Exp, ifFalse : Exp) extends Exp {
  override def toString = "(if " +condition +" "+ ifTrue +" "+ ifFalse + ")"

  def isPure = false
  def mustHalt = false

  def isDuplicable = false 

  lazy val free = condition.free ++ ifTrue.free ++ ifFalse.free
}

case class SetBang(val name : SName, val value : Exp) extends Exp {
  override def toString = "(set! "+name+" "+value+")"

  def isPure = false
  def mustHalt = value.mustHalt

  def isDuplicable = false 

  lazy val free = Set(name) ++ value.free
}

case class Values(exps : Arguments) extends Exp {
  override def toString = "(values "+exps+")"

  def isPure = false
  def mustHalt = false

  def isDuplicable = false 

  lazy val free = exps.free
}

case class LetValues(formals : List[Formals], values : List[Exp], body : Body) extends Exp {
  def isPure = false
  def mustHalt = false

  def isDuplicable = false 

  lazy val free = Set() ++ (values flatMap (_.free)) ++ (body.free -- (formals flatMap (_.bound)))
}

case class Begin(body : Body) extends Exp {
  override def toString = "(begin " +body+ ")"

  def isPure = false
  def mustHalt = false

  def isDuplicable = false 

  lazy val free = body.free
}


case class Undefined() extends Exp {
  override def toString = "'undefined"

  def isPure = true
  def mustHalt = true

  def isDuplicable = true

  lazy val free : Set[SName] = Set()
}

case class Void() extends Exp {
  override def toString = "(void)"

  def isPure = true
  def mustHalt = true

  def isDuplicable = true

  lazy val free : Set[SName] = Set()
}





// Syntactic sugar:

object Seq {
  def apply(first : Exp, second : Exp) : Exp = Begin(Body(List(),List(first,second)))
  def unapply(exp : Exp) : Option[(Exp,Exp)] = exp match {
    case Begin(Body(List(),List(first,second))) => Some((first,second))
    case _ => None
  }
}


case class And(exps : List[Exp]) extends Exp {
  def isPure = false
  def mustHalt = false
  def isDuplicable = false 

  lazy val free = Set() ++ (exps flatMap (_.free))

  lazy val toIf : Exp = exps match {
    case List() => SelfLit(SBoolean(true))
    case List(exp) => exp
    case hd :: tl => If(hd,And(tl).toIf,SelfLit(SBoolean(false)))
  }
}



case class Or(exps : List[Exp]) extends Exp {
  def isPure = false
  def mustHalt = false
  def isDuplicable = false 

  lazy val free = Set() ++ (exps flatMap (_.free))

  lazy val toIf : Exp = exps match {
    case List() => SelfLit(SBoolean(false))
    case List(exp) => exp
    case hd :: tl => 
      Exp.let (hd) (hd =>
        If(hd,hd,Or(tl).toIf))
  }
}


abstract class CondClause {
  def free : Set[SName] ;
}


case class SelfCondClause (val test : Exp) extends CondClause {
  lazy val free = test.free 
}

case class TestCondClause (val test : Exp, val exps : List[Exp]) extends CondClause {
  lazy val free = test.free ++ (exps flatMap (_.free))
}


case class ProcCondClause (val test : Exp, val proc : Exp) extends CondClause {
  lazy val free = test.free ++ proc.free
}

case class ElseCondClause (val exps : List[Exp]) extends CondClause {
  lazy val free =  Set() ++ (exps flatMap (_.free))
}

case class Cond(clauses : List[CondClause]) extends Exp {

  def isPure = false
  def mustHalt = false

  def isDuplicable = false 

  lazy val free = Set() ++ (clauses flatMap (_.free))

  def toIf(clauses : List[CondClause]) : Exp = {
    clauses match {
      case SelfCondClause(test) :: tl => 
        Exp.let (test) (v => If(v,v,toIf(tl)))
      case TestCondClause(test,exps) :: tl => If(test,Begin(Body(List(),exps)),toIf(tl))
      case List(ElseCondClause(exps)) => Begin(Body(List(),exps))
      case Nil => Void()
    }
  }

  lazy val toIf : Exp = toIf(clauses)
}




case class Binding (val name : SName, val value : Exp) {
  override def toString = "(" + name + " " + value + ")"
}

case class Bindings (val bindings : List[Binding]) {
  override def toString = bindings mkString " "

  lazy val names = bindings map (_.name)
  lazy val values = bindings map (_.value)

  def map (f : Binding => Binding) : Bindings = {
    Bindings(bindings map f)
  }
}

abstract class LetForm (val bindings : Bindings, val body : Body) extends Exp {
  def isPure = false
  def mustHalt = false

  def isDuplicable = false 

  protected def toString (head : String) = 
    "(" + head +" ("+ bindings +") " +body+")"

}

case class Let (_bindings : Bindings, _body : Body) extends LetForm(_bindings,_body) {
  override def toString = toString("let")

  def this (name : SName, value : Exp, exp : Exp) = this(Bindings(List(Binding(name,value))),Body(List(),List(exp)))

  lazy val free = Set() ++ (bindings.values flatMap (_.free)) ++ (body.free -- (bindings.names))

  def toLetStar = {
    val free = (bindings.values) flatMap (_.free)
    val rebinding = bindings.names exists (name => free contains name)
    if (rebinding)
      throw new Exception("Program not alphatised!")
    LetStar(bindings,body)
  }
}

case class LetRec (_bindings : Bindings, _body : Body) extends LetForm(_bindings,_body) {
  override def toString = toString("letrec")

  lazy val toLetsAndSets : Exp = {
    val newBindings = bindings map (b => Binding(b.name,Undefined()))
    val sets = bindings.bindings map (b => SetBang(b.name, b.value))
    Let(newBindings,Body(List(),sets ++ List(Begin(body))))
  }


  lazy val free = Set() ++ (bindings.values flatMap (_.free)) ++ body.free -- bindings.names
}

case class LetStar (_bindings : Bindings, _body : Body) extends LetForm(_bindings,_body) {
  override def toString = toString("let*")

  private def freeIn (bindings : List[Binding],body : Body) : Set[SName] = {
    if (bindings isEmpty)
      body.free
    else
      Set() ++ bindings.head.value.free ++ (freeIn(bindings.tail,body) - bindings.head.name)
  }

  lazy val toLets : Exp = bindings match {
    case Bindings(List()) => Begin(body)
    case Bindings(List(binding)) => Let(bindings,body)
    case Bindings(hd::tl) => Let(Bindings(List(hd)),
                                 ExpBody(LetStar(Bindings(tl),body).toLets))
  }

  lazy val free = freeIn(bindings.bindings,body)
}


object ListExp {
  import CommonSSymbols._

  def apply(exps : Exp*) : Exp = {
    apply(exps.toList)
  }

  def apply(exps : List[Exp]) : Exp = {
    exps match {
      case hd :: tl => new App(Ref(CommonSSymbols.SCons),hd,apply(tl))
      case Nil => QuoteLit(SNil())
    }
  }

  def unapply (exp : Exp) : Option[List[Exp]] = {
    exp match {
      case App(Ref(SListSym),ListArguments(args)) => Some(args map (_.exp))
      case _ => None
    }
  }
}


object ConsExp {
  def apply(car : Exp, cdr : Exp) : Exp = {
    new App(Ref(CommonSSymbols.SCons), car,cdr)
  }
}
