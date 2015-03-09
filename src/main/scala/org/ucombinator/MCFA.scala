package org.ucombinator

import scala.collection.immutable.{Set, SortedSet, TreeSet, SortedMap, TreeMap}

import scala.util.parsing.combinator._ ;
import scala.util.parsing.input.Positional  ;
import scala.util.matching.Regex ;




/* Comparison utilities. */

object ComparisonUtilities {

  def compare2[A <: Ordered[A], B <: Ordered[B]] (a1 : A, b1 : B) (a2 : A, b2 : B) : Int = {
    val cmpA = a1 compare a2
    if (cmpA != 0)
      cmpA
    else
      b1 compare b2
  }

  def compare3[A <: Ordered[A], B <: Ordered[B], C <: Ordered[C]] (a1 : A, b1 : B , c1 : C) (a2 : A, b2 : B, c2 : C) : Int = {
    val cmpA = a1 compare a2
    if (cmpA != 0)
      cmpA
    else {
      val cmpB = b1 compare b2
      if (cmpB != 0)
        cmpB
      else
        c1 compare c2
    }
  }


  def compareLists[A <% Ordered[A]] (l1 : List[A], l2 : List[A]) : Int = (l1,l2) match {
    case (hdA :: tlA, hdB :: tlB) => {
      val cmpHD = hdA compare hdB
      if (cmpHD != 0)
        cmpHD
      else
        compareLists(tlA,tlB)
    }
    case (List(),List()) => 0 
    case (hd :: tl,List()) => 1    
    case (List(),hd :: tl) => -1
  }
}



/* S-Expressions. */


object SExp {
  var shouldNamesBeSymbols = true

  def apply(list : List[SExp]) : SExp = list match {
    case hd :: tl => :+:(hd,apply(tl))
    case Nil => SNil()
  }

  def apply(list : List[SExp], tombstone : SExp) : SExp = list match {
    case hd :: tl => :+:(hd,apply(tl,tombstone))
    case Nil => tombstone
  }


  def parseAllIn (filename : String) : List[SExp] = {
    val input = scala.io.Source.fromFile(filename).mkString("")
    parseAll(input)
  }

  def parseAll (input : String) : List[SExp] = {
    val p = new SExpParser
    p.parseAll(input)
  }

  private var maxSerialNumber = 0

  def allocateSerialNumber() : Long = {
    maxSerialNumber += 1
    maxSerialNumber
  }
}


abstract class SExp extends Positional {
  lazy val serialNumber : Long = SExp.allocateSerialNumber()

  def toString : String ;

  def toList : List[SExp] ;
  def toDottedList : (List[SExp],SExp) ;

  def isKeyword : Boolean ;
  def isInteger : Boolean ;
  def isList : Boolean ;
  def isPair : Boolean ; 
  def isNull : Boolean ;
  def isSymbol : Boolean ;
  def isName : Boolean ;
}


final case class SInt(val value : BigInt) extends SExp {
  override def toString = value.toString

  def toList = throw new Exception("Cannot convert integer to list.")
  def toDottedList = (List(),this)

  def isKeyword = false
  def isInteger = true
  def isList = false
  def isPair = false
  def isNull = false
  def isName = false
  def isSymbol = false
}


final case class SChar(val value : Char) extends SExp {
  override def toString = "#\\" + value.toString

  def toList = throw new Exception("Cannot convert integer to list.")
  def toDottedList = (List(),this)

  def isKeyword = false
  def isInteger = false
  def isChar = true
  def isList = false
  def isPair = false
  def isNull = false
  def isName = false
  def isSymbol = false
}


final case class SText(val value : String) extends SExp {
  // TODO/FIXME: Escape the string value
  override def toString = "\"" + value + "\""

  def toList = throw new Exception("Cannot convert string to list.")
  def toDottedList = (List(),this)

  def isKeyword = false
  def isInteger = false
  def isString = true
  def isList = false
  def isPair = false
  def isNull = false
  def isName = false
  def isSymbol = false
}


case class SBoolean(val value : Boolean) extends SExp {
  override def toString = (if (value) { "#t" } else { "#f" })

  def toList = throw new Exception("Cannot convert Boolean to list.")
  def toDottedList = (List(),this)

  def isFalse = value

  def isKeyword = false
  def isInteger = false
  def isString = false
  def isBoolean = true
  def isList = false
  def isPair = false
  def isNull = false
  def isName = false
  def isSymbol = false  
}


case class SKeyword(val string : String) extends SExp with Ordered[SKeyword] {
  override def toString = "#:" + string

  def toList = throw new Exception("Cannot convert keyword to list.")
  def toDottedList = (List(),this)

  def isFalse = false

  def isKeyword = true
  def isInteger = false
  def isString = false
  def isBoolean = false
  def isList = false
  def isPair = false
  def isNull = false
  def isName = false
  def isSymbol = false  

  def compare (that : SKeyword) = this.string compare that.string
}


abstract class SSymbol (val string : String) extends SExp {
}


final case class SName(s : String, version : Int) extends SSymbol(s) with Ordered[SName] {
  def compare (that : SName) : Int = that match {
    case SName(s2,v2) => {
      val cmpString = s compare s2
      if (cmpString != 0)
        cmpString
      else
        version compare v2
    }
  }

  override def toString = 
    if (version == 0) {
      string
    } else {
      if (SExp.shouldNamesBeSymbols) 
        s + "$$" + version
      else
        "#name[" + string + " " + version + "]"
    }

  def toList = throw new Exception("Cannot convert symbol to list.")
  def toDottedList = (List(),this)

  def isKeyword = false
  def isInteger = false
  def isList = false
  def isPair = false
  def isNull = false
  def isSymbol = true
  def isName = true

  override def equals (a : Any) = a match {
    case SName(s2,v2) => (s equals s2) && (version == v2)
    case _ => false
  }
}

final case class SNil() extends SExp {
  override def toString = "()"

  def toList = List()
  def toDottedList = (List(),this)

  def isKeyword = false
  def isInteger = false
  def isList = true
  def isPair = false
  def isNull = true
  def isName = false
  def isSymbol = false
}

final case class :+:(var car : SExp, var cdr : SExp) extends SExp {
  override def toString = this.toDottedList match {
    case (l,SNil()) => "(" + (l mkString " ") + ")"
    case (l,end) => "(" + ((l mkString " ") + " . " + end) + ")"
  }

  def toList = car :: cdr.toList
  def toDottedList : (List[SExp],SExp) = {
    val (lst,end) = cdr.toDottedList
    return (car :: lst, end)
  }

  def isKeyword = false
  def isInteger = false
  def isList = cdr.isList
  def isPair = true
  def isNull = false
  def isName = false
  def isSymbol = false
}


object SList {
  def apply(sx : SExp*) : SExp = 
    SExp(sx.toList)

  def unapplySeq(sx : SExp) : Option[List[SExp]] = {
    if (sx isList) 
      Some(sx toList)
    else
      None
  }
}



object SKeyword {

  private val keywordTable = scala.collection.mutable.HashMap[String,SKeyword]()

  def from (string : String) : SKeyword = {
    (keywordTable get string) match {
      case Some(kw) => kw
      case None => {
        val kw = SKeyword(string)
        keywordTable(string) = kw
        kw
      }
    }
  }
}


object SName {

  private val nameTable = scala.collection.mutable.HashMap[String,SName]()
  private val maxTable = scala.collection.mutable.HashMap[String,SName]()

  def from (string : String) : SName = {
    (nameTable get string) match {
      case Some(name) => name
      case None => {
        val name = SName(string,0)
        nameTable(string) = name
        name
      }
    }
  }

  def from (symbol : SSymbol) : SName = {
    from(symbol.string)
  }

  def gensym(string : String) : SName = {
    (maxTable get string) match {
      case Some(SName(_,v)) => {
        val name = SName(string,v+1)
        maxTable(string) = name
        name
      }
      case None => {
        val name = SName(string,1)
        maxTable(string) = name
        name        
      }
    }
  }

  def gensym(symbol : SSymbol) : SName = {
    gensym(symbol.string)
  }
}




object CommonSSymbols {

  val SQuote = SName.from("quote")
  val SQuasiquote = SName.from("quasiquote")
  val SUnquote = SName.from("unquote")
  val SUnquoteSplicing = SName.from("unquote-splicing")

  val SCons = SName.from("cons")
  val SListSym = SName.from("list")
  val SAppend = SName.from("append")

  val SDefine = SName.from("define")

  val SLambda  = SName.from("lambda")

  val SLet     = SName.from("let")
  val SLetStar = SName.from("let*")
  val SLetRec  = SName.from("letrec") 

  val SSetBang = SName.from("set!")
  val SBegin = SName.from("begin")
  val SVoid = SName.from("void")

  val SIf = SName.from("if")
  val SCond = SName.from("cond")
  val SElse = SName.from("else")
  val SRightArrow = SName.from("=>")
  val SAnd = SName.from("and")
  val SOr = SName.from("or")

  val SValues = SName.from("values")
  val SLetValues = SName.from("let-values")

}



class SExpParser extends RegexParsers {

  override def skipWhitespace = true 

  override protected val whiteSpace = new Regex("(\\s*([;][^\\r\\n]*)?)*")

  private def integer : Parser[SExp] = 
    regex(new Regex("-?[0-9]+")) ^^ { case s => SInt(Integer.parseInt(s)) }

  private def strue : Parser[SExp] =
    "#t" ^^ { case "#t" => SBoolean(true) }

  private def sfalse : Parser[SExp] =
    "#f" ^^ { case "#f" => SBoolean(false) }

  private def schar : Parser[SExp] =
    ("#\\" ~ regex(new Regex("[^\\r\\n\\t ]"))) ^^ { case "#\\" ~ c => SChar(c.charAt(0)) }

  private def symbol : Parser[SExp] = 
    regex(new Regex("([^.#; \\t\\r\n()',`\"][^; \\t\\r\\n()',`\"]*|[.][^; \\t\\r\\n()',`\"]+)")) ^^ { case s => SName.from(s) } 

  private def keyword : Parser[SExp] = 
    regex(new Regex("([#][:][^; \\t\\r\\n()',`\"]+)")) ^^ { case s => SKeyword.from(s.substring(2)) }

  private def text : Parser[SExp] =
    "\"\"" ^^  { case _ => SText("") } |
    regex(new Regex("\"([^\"\\\\]|\\\\.|\\\\\\\\|)*\"")) ^^ { case s => SText(s.substring(1,s.length()-1)) }
  
  private def nil : Parser[SExp] = 
    ("(" ~ ")") ^^ { case "(" ~ ")" => SNil() }

  private def sboolean : Parser[SExp] = strue | sfalse

  private def sxlist : Parser[SExp] = 
    ("(" ~ sexplist ~ ")") ^^ { case "(" ~ l ~ ")" => l }

  private def special : Parser[SExp] = 
    (",@" ~ sexp) ^^ { case ",@" ~ sexp => SExp(List(CommonSSymbols.SUnquoteSplicing,sexp)) } |
    ("'"  ~ sexp) ^^ { case "'"  ~ sexp => SExp(List(CommonSSymbols.SQuote,sexp)) } |
    ("`"  ~ sexp) ^^ { case "`"  ~ sexp => SExp(List(CommonSSymbols.SQuasiquote,sexp)) } | 
    (","  ~ sexp) ^^ { case ","  ~ sexp => SExp(List(CommonSSymbols.SUnquote,sexp)) } 
                     

  private def sexp : Parser[SExp] = positioned(nil | sxlist | integer | sboolean | keyword | schar | text | symbol  | special)

  private def sexplist : Parser[SExp] = 
    rep(sexp) ~ (("." ~ sexp) ?) ^^
     { case sexps ~ Some("." ~ sexp) => SExp(sexps,sexp) 
       case sexps ~ None => SExp(sexps) }

  def parse (input : String) : SExp = parse(sexp,input).get

  def parseAll (input : String) : List[SExp] = parse(phrase(rep(sexp)),input).get
}







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







/**
 An RnRSParser parses macro-expanded programs into an AST.
 */
class RnRSParser {
  
  import CommonSSymbols._ ;

  var expandQuotes = true ;

  def parseExp(sexp : SExp) : Exp = {
    // DEBUG
    // println("parsing exp: " + sexp)

    sexp match {
      
      // Literals:
      case _ : SInt => SelfLit(sexp)
      case t : SText => SelfLit(t)
      case b : SBoolean => SelfLit(b)
      case c : SChar => SelfLit(c)
      case n : SName => Ref(n)
      case SQuote :+: sexp :+: SNil() => {
        if (expandQuotes)
          QuoteLit(sexp).expansion
        else 
          QuoteLit(sexp)
      }


      // Quasi-literals:
      case SQuasiquote :+: qqexp :+: SNil() => {
        // println("processing quasiquote: " + qqexp) // DEBUG
        parseQuasiquote(1,qqexp)
      }

      // Functions:
      case SLambda :+: formals :+: body => 
        Lambda(parseFormals(formals), parseBody(body))

      // Side effects and sequencing:
      case SSetBang :+: (name : SName) :+: value :+: SNil() =>
        SetBang(name,parseExp(value))
      case SBegin :+: body =>
        Begin(parseBody(body))

      // Conditionals:
      case SIf :+: condition :+: ifTrue :+: ifFalse :+: SNil() =>
        If(parseExp(condition), parseExp(ifTrue), parseExp(ifFalse))
      case SIf :+: condition :+: ifTrue :+: SNil() =>
        If(parseExp(condition), parseExp(ifTrue), Void())
      case SCond :+: clauses =>
        Cond(clauses.toList map parseCondClause)
      case SOr :+: exps =>
        Or(exps.toList map parseExp)
      case SAnd :+: exps =>
        And(exps.toList map parseExp)

      // Binding forms:
      case SLet :+: bindings :+: body =>
        Let(parseBindings(bindings),parseBody(body))
      case SLetStar :+: bindings :+: body =>
        LetStar(parseBindings(bindings),parseBody(body))
      case SLetRec :+: bindings :+: body =>
        LetRec(parseBindings(bindings),parseBody(body))

      // Applications:
      case fun :+: args => 
        App(parseExp(fun), parseArguments(args))
    }
  }

  def parseQuasiquote(depth : Int, qqexp : SExp) : Exp = {
    qqexp match {
      case SList(SUnquote,sexp) => 
        if (depth == 1) {
          parseExp(sexp)
        } else {
          ListExp(QuoteLit(SUnquote),parseQuasiquote(depth-1,sexp))
        }

      case SList(SQuasiquote,sexp) => 
        ListExp(QuoteLit(SQuasiquote),parseQuasiquote(depth+1,sexp))
      
      case hd @ SList(SUnquoteSplicing,sexp) :+: tl => {
        if (depth == 1) {
          new App(Ref(SAppend),parseExp(sexp),parseQuasiquote(depth,tl))
        } else {
          ConsExp(ListExp(QuoteLit(SUnquoteSplicing),parseQuasiquote(depth-1,sexp)),parseQuasiquote(depth,tl))
        }
      }

      case hd :+: tl =>
        ConsExp(parseQuasiquote(depth,hd), parseQuasiquote(depth,tl))

      case sexp => 
        QuoteLit(sexp)
    }    
  }

  def parseDefExps(sxl : List[SExp]) : (List[Def],List[Exp]) = sxl match {
    case Nil => (Nil,Nil)
    case hd :: tl => {
      val (defs,exps) = parseDefExps(tl)
      hd match {
        case SDefine :+: _ => (parseDef(hd) :: defs, exps)
        case _ => (defs, parseExp(hd) :: exps)
      }
    }
  }

  def parseBody(sexps : SExp) : Body = {
    val sxl = sexps.toList
    val (defs,exps) = parseDefExps(sxl)
    Body(defs,exps)
  }
  
  def parseFormals(sxl : SExp) : Formals = {
    if (sxl.isList) {
      val names = sxl.toList
      ListFormals(names map ((n : SExp) => PosFormal(n.asInstanceOf[SName])))
    } else if (sxl.isPair) {      
      val (names,rest) = sxl.toDottedList
      if (names isEmpty)
        VarFormals(rest.asInstanceOf[SName])
      else
        MultiFormals(names map ((n : SExp) => PosFormal(n.asInstanceOf[SName])), rest.asInstanceOf[SName])
    } else if (sxl.isName) {
      VarFormals(sxl.asInstanceOf[SName])
    } else {
      throw new Exception("Unhandled case for formals")
    }
  }

  def parseArguments(sxl : SExp) : Arguments = {
    ListArguments(sxl.toList map (x => PosArgument(parseExp(x))))
  }

  def parseBinding(binding : SExp) = {
    binding match {
      case SList(name : SName, value : SExp) => Binding(name,parseExp(value))
    }
  }

  def parseBindings(bindings : SExp) = {
    val binds = bindings.toList
    Bindings(binds map parseBinding)
  }

  def parseDef(sexp : SExp) : Def = {
    sexp match {
      case SDefine :+: (name : SName) :+: value :+: SNil() => 
        VarDef(name, parseExp(value))
      case SDefine :+: ((name : SName) :+: formals) :+: body => 
        FunctionDef(name, parseFormals(formals), parseBody(body))
      case _ => 
        ImplicitDef(parseExp(sexp))
    }
  }

  def parseCondClause (sexp : SExp) : CondClause = {
    sexp match {
      case test :+: SNil() => 
        SelfCondClause(parseExp (test))

      case SElse :+: exps =>
        ElseCondClause(exps.toList map parseExp)

      case test :+: SRightArrow :+: proc :+: SNil() =>
        ProcCondClause(parseExp(test), parseExp(proc))
        
      case test :+: exps =>
        TestCondClause(parseExp(test), exps.toList map parseExp)
    }
  }

  def parseProgram (sxl : List[SExp]) : Program = {
    Program(sxl map parseDef)
  }

}



object RnRSParser {

  def apply (sexps : List[SExp]) : Program = {
    val p = new RnRSParser
    p.parseProgram(sexps)
  }

}








class ANormalizer {

  var convertBodyToLetRec = true

  var convertLetRecToLetsAndSets = true


  def normalize (exp : Exp) : Exp = normalizeExp (exp) (e => e)

  def normalize (body : Body) : Body = {
    if (convertBodyToLetRec) {
      ExpBody(normalize(body.toLetRec))
    } else {
      body match {
        case Body(defs,exps) => Body(defs map normalize, exps map normalize)
      }
    }
  }
  
  def normalize (defn : Def) : Def = defn match {
    case ImplicitDef(exp) => ImplicitDef(normalize(exp))
    case VarDef(name,exp) => VarDef(name,normalize(exp))
    case FunctionDef(name,formals,body) => FunctionDef(name,formals,normalize(body))
  }



  def normalizeExp (exp : Exp) (k : Exp => Exp) : Exp = 
    exp match {
      case Ref(name) => k(exp)
      case Lit(sexp) => k(exp)
      case Undefined() => k(exp)
      case Void() => k(exp)

      case Lambda(formals,body) => 
        k(Lambda(formals,normalize(body)))

      case If(cond,ifTrue,ifFalse) => 
        normalizeName (cond) (cond =>
          k(If(cond, normalize(ifTrue), normalize(ifFalse))))
      case cond : Cond =>
        normalizeExp (cond.toIf) (k)
      case or : Or =>
        normalizeExp (or.toIf) (k)
      case and : And =>
        normalizeExp (and.toIf) (k)
        

      case Let(Bindings(List()),body) => 
        Begin(normalizeBody (body) (k))
      case Let(Bindings(List(Binding(name,value))),body) =>
        normalizeExp (value) (value =>
          Let(Bindings(List(Binding(name,value))),normalizeBody (body) (k)))
      // FIXME: Assumes alphatization; check/fix if not true:
      case let @ Let(Bindings(hd::tl),body) => 
        normalizeExp (let.toLetStar) (k)

      case lets @ LetStar(_,_) => 
        normalizeExp (lets.toLets) (k)

      case SetBang(name,value) =>
        normalizeLinearName (value) (value =>
          Seq(SetBang(name,value),
              k(Void())))

      case Begin(ExpBody(exp)) => 
        normalizeExp (exp) (k)
      case Begin(Body(List(),hd::tl)) =>
        Seq(normalize(hd),
            normalizeExp (Begin(Body(List(),tl))) (k))
      case Begin(body) if !convertBodyToLetRec =>
        Begin(normalizeBody (body) (k))
      case Begin(body) if convertBodyToLetRec =>
        normalizeExp (body.toLetRec) (k)

      case letrec @ LetRec(_,_) if convertLetRecToLetsAndSets =>
        normalizeExp (letrec.toLetsAndSets) (k)

      case ListExp(Nil) => QuoteLit(SNil())
      case ListExp(hd :: tl) => 
        normalizeExp (ConsExp(hd,ListExp(tl))) (k)
          
        

      case App(fun,args) =>  
        normalizeLinearName (fun) (fun =>
          normalizeArguments (args) (args =>
            k(App(fun,args))))

      case _ => throw new Exception("Unhandled expression during A-Normalization: " + exp)
    }

  def normalizeName (exp : Exp) (k : Exp => Exp) : Exp = 
    normalizeExp (exp) (exp => 
      if (exp.isAtomic && exp.isDuplicable)
        k(exp)
      else
        Exp.let (exp) (k))

  def normalizeNames (exps : List[Exp]) (k : List[Exp] => Exp) : Exp = 
    if (exps.isEmpty)
      k(Nil)
    else
      normalizeName (exps.head) (e => 
        normalizeNames (exps.tail) (es =>
          k(e :: es)))


  def normalizeLinearName (exp : Exp) (k : Exp => Exp) : Exp = {
    // println("[["+exp+"]].isAtomic = " + exp.isAtomic) // DEBUG
    normalizeExp (exp) (exp => 
      if (exp.isAtomic) 
        k(exp)
      else 
        Exp.let (exp) (k))
  }
                      

  def normalizeLinearNames (exps : List[Exp]) (k : List[Exp] => Exp) : Exp = 
    if (exps.isEmpty)
      k(Nil)
    else
      normalizeLinearName (exps.head) (e => 
        normalizeLinearNames (exps.tail) (es =>
          k(e :: es)))


  def normalizeBody (body : Body) (k : Exp => Exp) : Body = body match {
    case Body(defs,exps) => {
      val normDefs = defs map normalize
      exps reverse match {
        case List(exp) => Body(normDefs,List(normalizeExp (exp) (k)))
        case last :: front => {
          Body(normDefs,(((normalizeExp (last) (k)) :: (front map normalize)) reverse))
        }
      }
    }
  }

  def normalizeArgs (argList : List[Argument]) (k : List[Argument] => Exp) : Exp = argList match {
    case Nil => k(Nil)
    case PosArgument(exp) :: tl => 
      normalizeLinearName (exp) (exp => 
        normalizeArgs (tl) (tl =>
          k(PosArgument(exp) :: tl)))
  }

  def normalizeArguments (args : Arguments) (k : Arguments => Exp) : Exp = args match {
    case ListArguments(argList) => 
      normalizeArgs (argList) (argList => 
        k(ListArguments(argList)))
  }


}


object ANormalizer {
  def apply (p : Program) : Exp = {
    val t = new ANormalizer
    val e = p.toExp
    // println ("e: " + e) // DEBUG
    t.normalize (e)
  }
}








/**
 * Assumes A-Normalization.
 */
class CPSConverter {

  def convert (exp : Exp) : Exp = {
    val answer = SName.from("answer")
    convertQExp (exp) (KLambda(ListFormals(List(PosFormal(answer))),ExpBody(Ref(answer))))
  }


  def convertExp (exp : Exp) (k : Exp => Exp) : Exp = {
    exp match {
      
      case _ : Ref => k(exp) 
      case _ : Lit => k(exp)
      case _ : Undefined => k(exp)
      case _ : Void => k(exp)
      
      case Lambda(formals,body) => {
        val cc = SName.gensym("cc")
        k(ULambda(KeywordFormal(SKeyword.from("cc"), cc) :: formals, 
                  convertQBody (body) (Ref(cc))))
      }

      case Let(Bindings(List(Binding(name,value))),body) =>
        convertQExp (value) (KLambda(ListFormals(List(PosFormal(name))),
                                     convertBody (body) (k)))

      case _ => {
        convertQExp (exp) (cont(k))
      }
    }
  }

  def convertExps (exps : List[Exp]) (k : List[Exp] => Exp) : Exp = {
    exps match {
      case Nil => k(Nil)
      case hd :: tl => 
        convertExp (hd) (hd =>
          convertExps (tl) (tl =>
            k(hd :: tl)))
    }
  }

  def convertQExp (exp : Exp) (q : Exp) : Exp = {
    exp match {
      case _ : Ref => new App(q,exp)
      case _ : Lit => new App(q,exp)
      case _ : Undefined => new App(q,exp)
      case _ : Void => new App(q,exp)

      case Lambda(formals,body) => {
        val cc = SName.gensym("cc")
        new App(q,ULambda(KeywordFormal(SKeyword.from("cc"), cc) :: formals,
                          convertQBody (body) (Ref(cc))))
      }

      case Let(Bindings(List(Binding(name,value))),body) =>
        convertQExp (value) (KLambda(ListFormals(List(PosFormal(name))),
                                     convertQBody (body) (q)))

      case If(condition,ifTrue,ifFalse) => {
        let (q) (q =>
          convertExp (condition) (condition =>
            If(condition,
               convertQExp (ifTrue) (q),
               convertQExp (ifFalse) (q))))
      }

      case SetBang(name,value) =>
        convertExp (value) (value =>
          Seq(SetBang(name,value),
              new App(q,Void())))

      case Begin(body) => 
        convertQBody (body) (q) match {
          case Body(List(),List(exp)) => exp
        }

      case App(f,args) => 
        convertExp (f) (f =>
          convertArguments (args) (args =>
            App(f,KeywordArgument(SKeyword.from("cc"), q) :: args)))
    }
  }

  def cont (k : Exp => Exp) : Exp = {
    val rv = SName.gensym("rv")
    KLambda(ListFormals(List(PosFormal(rv))),
            ExpBody(k(Ref(rv))))
  }

  def ignoreCont (body : Body) : Exp = {
    val rv = SName.gensym("_")
    KLambda(VarFormals(rv),
            body)
  }

  def let (exp : Exp) (k : Exp => Exp) : Exp = {
    exp match {
      case _ : Ref => k(exp)
      case _ : Lit => k(exp)
      case _ : Void => k(exp)
      case _ : Undefined => k(exp)

      case lam : Lambda => {
        val tmp = SName.gensym("$tmp")
        new App(KLambda(ListFormals(List(PosFormal(tmp))),ExpBody(k(Ref(tmp)))),
                exp)
      }
    }
  }

  def convertArguments (arguments : Arguments) (k : Arguments => Exp) : Exp = {
    arguments match {
      case ListArguments(List()) => k(ListArguments(List()))
      case ListArguments(hd :: tl) => 
        convertArgument (hd) (hd => 
          convertArguments (ListArguments(tl)) (tl => 
            k(hd :: tl)))
    }
  }

  def convertArgument (argument : Argument) (k : Argument => Exp) : Exp = {
    argument match {
      case PosArgument(exp) => 
        convertExp (exp) (exp => 
          k(PosArgument(exp)))
      case KeywordArgument(keyword,exp) =>
        convertExp (exp) (exp => 
          k(KeywordArgument(keyword,exp)))
    }
  }

  def convertBody (body : Body) (k : Exp => Exp) : Body = {
    body match {
      case ExpBody(exp) => ExpBody(convertExp (exp) (k))
    }
  }

  def convertQSeq (exps : List[Exp]) (q : Exp) : Exp = {
    exps match {
      case List() => new App(q,Void()) 
      case List(exp) => convertQExp (exp) (q)
      case hd :: tl => convertQExp (hd) (ignoreCont (ExpBody(convertQSeq (tl) (q))))
    }
  }

  def convertQBody (body : Body) (q : Exp) : Body = {
    body match {
      case Body(List(),exps) => ExpBody(convertQSeq (exps) (q))
      case ExpBody(exp) => ExpBody(convertQExp (exp) (q))
    }
  }

}




object CPSConverter {
  
  def apply (exp : Exp) : Exp = {
    val t = new CPSConverter
    t.convert (exp)
  }

}








/* Small-step abstract interpretation */


/**
 A state in a small-step abstract interpretation.

 The <code>flat</code> component of a state has a flat partial order;
 the <code>sharp</code> component of a state has a true partial order.
 */
case class State(_flat : Flat, _sharp : Sharp) {
  def flat = _flat

  def sharp_= (_sharp : Sharp) : State = 
    State(_flat,_sharp)

  def sharp = _sharp
}

trait Flat extends Ordered[Flat] {
  override def equals (that : Any) = that match {
    case that : Flat => (this compare that) == 0
    case _ => false
  }
}

case object StuckFlat extends Flat {
  def compare (that : Flat) = 
    that match {
      case StuckFlat => 0
      case _ => -1
    }
}


abstract class DeltaSharp {
  def isEmpty : Boolean ;

  def apply(sharp : Sharp) : Sharp ;
}

/*
case object NullDeltaSharp extends DeltaSharp {
  override isEmpty = true
  
  def apply(sharp : Sharp) : Sharp = sharp
}
*/


trait Sharp {
  def wt (that : Sharp) : Boolean ;

  def resetChangeLog : Sharp ;
  def changeLog : DeltaSharp ;
}









/* Flow analyses */

trait Addr extends Ordered[Addr] {

  def localCompare (that : Addr) : Int ;

  def compare (that : Addr) : Int = {
    val thisClass = this.getClass() 
    val thatClass = that.getClass()
    val cmp = thisClass.getName() compare thatClass.getName()
    if (cmp != 0)
      cmp
    else
      localCompare(that)
  }

}

trait ObjectLocation extends Value {
  def isProcedure = false
  def isObjectLocation = true
  def objectType : SName ;
}

case class ConsLocation(time : Time) extends ObjectLocation {
  def localCompare (that : Value) : Int = that match {
    case ConsLocation(thatTime) => time compare thatTime
  }

  val objectType = SName.from("cons")

  def toSourceLabel : Int = time match {
    case KTime(hd :: tl) => -hd + -10
    case _ => -1
  }
}

case class FieldAddr(baseAddr : ObjectLocation, field : SName) extends Addr {
  def localCompare (that : Addr) : Int = {
    that match {
      case FieldAddr(ba2,field2) => ComparisonUtilities.compare2 (baseAddr.asInstanceOf[Value],field) (ba2.asInstanceOf[Value],field2)
    }
  }
}



trait BEnv extends Ordered[BEnv] {
  def apply(name : SName) : Addr ;
  override def equals (that : Any) = that match {
    case thatBEnv : BEnv => (this compare thatBEnv) == 0
    case _ => false
  }
  def update(name : SName, addr : Addr) : BEnv ;
  def | (names : Iterable[SName]) : BEnv ;
}

trait Time extends Ordered[Time] {
  def succ (k : Int, call : Int) : Time ;
}

trait Store {
  def apply(addr : Addr) : D = (this get addr) match {
    case Some(d) => d
    case None => throw new Exception("Could not find " + addr)
  }
  def getOrElse(addr : Addr, default : D) : D = (this get addr) match {
    case Some(d) => d
    case None => default
  }
  def get(addr : Addr) : Option[D] ;


  def wt (that : Store) : Boolean ;

  /**
   Weak update.
   */
  def + (addr : Addr, d : D) : Store ;

  /**
   Strong update if safe; weak update otherwise.
   */
  def update (addr : Addr, d : D) : Store ;

  def toList : List[(Addr,D)] ;
}

trait Kont

trait Value extends Ordered[Value] {
  def isProcedure : Boolean ;
  def isObjectLocation : Boolean ;

  protected def localCompare(that : Value) : Int ;

  def compare (that : Value) : Int = {
    val thisClassName = this.getClass().getName()
    val thatClassName = that.getClass().getName()
    val cmp = thisClassName compare thatClassName
    if (cmp != 0)
      cmp
    else
      this.localCompare(that)
  }

  override def equals (that : Any) : Boolean = that match {
    case that : Value => (this compare that) == 0
    case _ => false
  }

  def toSourceLabel : Int ;
}

case class BooleanValue(val value : Boolean) extends Value {
  override def toString = if (value) { "#t" } else { "#f" }

  def localCompare (that : Value) : Int = that match {
    case BooleanValue(value2) => value compare value2
  }
  
  def isProcedure = false
  def isObjectLocation = false

  def toSourceLabel : Int = -2
}


case class PrimValue(val name : String) extends Value {
  def localCompare (that : Value) : Int = that match {
    case PrimValue(name2 : String) => name compare name2
  }

  def isProcedure = true
  def isObjectLocation = false

  def toSourceLabel : Int = -3
}


case class Clo(val lam : Lambda, val bEnv : BEnv) extends Value {
  def localCompare (that : Value) : Int = that match {
    case Clo(lam2,bEnv2) => ComparisonUtilities.compare2 (lam.asInstanceOf[Exp],bEnv) (lam2.asInstanceOf[Exp],bEnv2)
  }

  def isProcedure = true 
  def isObjectLocation = false

  def toSourceLabel : Int = lam.label
}

trait D {
  def + (value : Value) : D ;
  def join (that : D) : D ;
  def wt (that : D) : Boolean ;
  def toList : List[Value] ;
}






/* Generics components */


case class StoreUpdate (val isStrong : Boolean, val addr : Addr, val d : D) {
  def apply(sharp : Sharp) : Sharp = {
    sharp match {
      case StoreSharp(store) => 
        if (isStrong)
          new StoreSharp (store(addr) = d)
        else
          new StoreSharp (store + (addr,d))
    }
  }
}

case class StoreUpdateDeltaSharp (val changeLog : List[StoreUpdate]) extends DeltaSharp {
  def isEmpty = changeLog.isEmpty

  def apply (sharp : Sharp) : Sharp = 
    changeLog.foldLeft (sharp) ((sharp,update) => update(sharp))
}

class SentinelStore(val changeLog : List[StoreUpdate], val store : Store) extends Store {

  def this (store : Store) = this(List(), store)

  def get (addr : Addr) = store get addr
  def wt (that : Store) = that match {
    case thatStore : SentinelStore => store wt thatStore.store
    case _ => store wt that
  }

  def resetLog() = 
    new SentinelStore(List(), store)

  def + (addr : Addr, d : D) : SentinelStore = {
    (store get addr) match {
      case Some(d2) if (d wt d2) => this
      case _ => {
        new SentinelStore(StoreUpdate(false,addr,d) :: changeLog, store + (addr,d))
      }
    }
  }

  def update (addr : Addr, d : D) : SentinelStore = {
    (store get addr) match {
      case Some(d2) if (d wt d2) => this
      case _ => {
        new SentinelStore(StoreUpdate(true,addr,d) :: changeLog, store(addr) = d)
      }
    }
  }

  override def toString = store.toString

  def toList = store.toList
}


class MapStore(val map : SortedMap[Addr,D]) extends Store {
  def this () = this(TreeMap())

  def get(addr : Addr) : Option[D] = map get addr

  def wt (that : Store) = that match {
    case _ => throw new Exception("Unknown store type")
  }

  /**
   Weak update.
   */
  def + (addr : Addr, d : D) : MapStore = {
    map get addr match {
      case Some(existingD) => new MapStore(map + (addr -> (d join existingD)))
      case None => new MapStore(map + (addr -> d))
    }
  }

  /**
   A simple store does not contain enough information to determine whether a strong update is safe or not, so
   this operation always performs a weak update.
   */
  def update (addr : Addr, d : D) : Store = {
    // We don't have enough information to do a strong update, so we fall back to a weak update.
    this + (addr,d)
  }

  override def toString = "\n " +  (map mkString "\n ") 

  def toList : List[(Addr,D)] = map.toList
}


case class SortedSetD (set : SortedSet[Value]) extends D {
  def this () = this (TreeSet())

  def + (value : Value) : D = SortedSetD(set + value)
  
  def join (that : D) = SortedSetD(set ++ that.toList)

  def wt (that : D) : Boolean = {
    that match {
      case SortedSetD(set2) => set subsetOf set2
    }
  }

  def toList = set.toList

  override def toString = "{" + (set mkString ",") + "}"
}



case class StoreSharp(val store : Store) extends Sharp {

  def resetChangeLog = {
    if (store.isInstanceOf[SentinelStore])
      new StoreSharp(new SentinelStore(store.asInstanceOf[SentinelStore].store))
    else
      new StoreSharp(new SentinelStore(store))
  }
  
  def changeLog = {
    if (store.isInstanceOf[SentinelStore])
      StoreUpdateDeltaSharp(store.asInstanceOf[SentinelStore].changeLog)
    else
      throw new Exception()
  }

  def wt (that : Sharp) : Boolean = that match {
    case StoreSharp(thatStore) => store wt thatStore
    case _ => throw new Exception("Can't compare sharps!")
  }
}



case class KTime(val last : List[Int]) extends Time {
  def compare (that : Time) =  that match {
    case KTime(last2) => ComparisonUtilities.compareLists (last,last2)
  }
  def succ (k : Int, call : Int) : KTime = KTime((call :: last) take k)
}


case object UniTime extends Time {
  def compare (that : Time) = 
    if (this eq that)
      0
    else
      -1

  def succ (k : Int, call : Int) : Time = this
}


case class PrimAddr(val name : SName) extends Addr {
  def localCompare (that : Addr) : Int = that match {
    case PrimAddr(thatName) => this.name compare thatName
  }
}



/* RnRS primitives */
object RnRSPrimitives {

  def list = List("*","-","+","/",
                  "quotient","gcd","modulo","log",
                  "ceiling",
                  "<","=",">","<=",">=",
                  "odd?","even?","char?","symbol?","list?","pair?","null?","integer?","number?","boolean?","procedure?","string?",
                  "char-alphabetic?","char-numeric?","string<?",
                  "eq?","equal?","eqv?","char=?",
                  "string-ref","string-length", "string-append","number->string","list->string","symbol->string",
                  "string->symbol",
                  "char->integer",
                  "not",
                  "length",
                  "cons","car","cdr",
                  "newline","display",
                  "random",
                  "error")

}



/* mCFA */


case class FlatBind (name : SName, bEnv : BEnv) extends Addr {
  def localCompare (that : Addr) : Int = {
    that match {
      case FlatBind(thatName, thatBEnv : BEnv) =>
        ComparisonUtilities.compare2 (name,bEnv) (thatName,thatBEnv)
    }
  }
}

case class FlatBEnv (val labels : List[Int]) extends BEnv {
  def apply (name : SName) = FlatBind(name,this)
  def succ (m : Int, l : Int) = FlatBEnv((l::labels) take m)

  def compare (that : BEnv) = that match {
    case that : FlatBEnv => ComparisonUtilities.compareLists (this.labels, that.labels)
  }
  
  def update (name : SName, addr : Addr) : BEnv = throw new Exception("Cannot extend flat binding environments.")

  def | (names : Iterable[SName]) : BEnv = this
}


case class MapBind (val name : SName, val time : Time) extends Addr {
  def localCompare (that : Addr) : Int = that match {
    case MapBind(thatName,thatTime) => ComparisonUtilities.compare2 (this.name,this.time) (thatName,thatTime)
  }
}

case class MapBEnv (val map : SortedMap[SName,Addr]) extends BEnv {
  def apply (name : SName) = map(name)
  def succ (m : Int, l : Int) = throw new Exception("Not appropriate in this context.")
  
  def compare (that : BEnv) = that match {
    case MapBEnv(thatMap) => ComparisonUtilities.compareLists (this.map.toList,thatMap.toList)
  }
  
  def update (name : SName, addr : Addr) : BEnv = 
    MapBEnv(map + (name -> addr))

  def | (names : Iterable[SName]) : BEnv = MapBEnv(TreeMap[SName,Addr]() ++ (names map (n => (n,map(n)))))
    
}


case class CFlat(val exp : Exp, val bEnv : BEnv, val t : Time) extends Flat {
  def compare (that : Flat) = that match {
    case CFlat(exp2,bEnv2,t2) => ComparisonUtilities.compare3 (exp,bEnv,t) (exp2,bEnv2,t2)
  }
}



class Parameters(val keywords : SortedMap[SKeyword,D], val positionals : List[D]) {

  def this () = this(TreeMap(), List())

  def apply(keyword : SKeyword) : D = keywords(keyword)

  def apply(position : Int) : D = positionals(position)

  def update(keyword : SKeyword, d : D) : Parameters = 
    new Parameters(keywords + (keyword -> d), positionals)

  def :: (d : D) : Parameters =
    new Parameters(keywords, d :: positionals)
}




class KCFA_CPS(exp : Exp, bEnv0 : BEnv, t0 : Time, store0 : Store, botD : D) extends SmallStepAbstractInterpretation {

  var k : Int = 0

  val bEnv1 = RnRSPrimitives.list.foldRight (bEnv0) ((name,bEnv) => bEnv(SName.from(name)) = PrimAddr(SName.from(name)))

  val store1 = RnRSPrimitives.list.foldRight (store0) ((name,store) => store(bEnv1(SName.from(name))) = botD + PrimValue(name))
  
  def atomEval (bEnv : BEnv, store : Store) (exp : Exp) : D = exp match {
    case Lit(SBoolean(value)) => botD + BooleanValue(value)
    case _ : Lit => botD
    case Undefined() => botD
    case Ref(name) => {
      val addr = bEnv(name)
      (store get addr) match {
        case Some(d) => d
        case None => {
          throw new Exception("could not find address: " + addr)
        }
      }
    }
    case Void() => botD
    case lam : Lambda => {
      botD + Clo(lam,bEnv | lam.free)
    }
  }

  def inject (exp : Exp) : State = {
    State(CFlat(exp,bEnv1,t0),StoreSharp(store1))
  }

  lazy val initialState = inject(exp)

  def tick (call : Exp, t : Time) : Time = t.succ(k,call.label)

  def evalArgs (args : Arguments, bEnv : BEnv, store : Store) : Parameters = {
    args match {
      case ListArguments(arglist) => evalArgs (new Parameters()) (arglist,bEnv,store)
    }
  }

  def evalArgs (parameters : Parameters) (arglist : List[Argument], bEnv : BEnv, store : Store) : Parameters = {
    arglist match {
      case Nil => parameters
      case hd :: tl => {
        val p = evalArgs (parameters) (tl,bEnv,store)
        hd match {
          case PosArgument(exp) => (atomEval (bEnv,store) (exp)) :: p
          case KeywordArgument(kw,exp) => p(kw) = atomEval (bEnv,store) (exp)
        }
      }
    }
  }


  private def applyProcedure (args : Arguments, params : Parameters, store : Store, newTime : Time) (proc : Value) : List[State] = {
    proc match {
      
      case Clo(lam @ Lambda(VarFormals(name),ExpBody(call)),bEnv2) if !(call.free contains name)  => {
        List(State(CFlat(call,bEnv2,newTime),StoreSharp(store)))
      }
      
      case Clo(lam @ Lambda(formals,ExpBody(call)),bEnv2) if formals accepts args => {
        
        var newStore = store 
        var newBEnv = bEnv2
        
        // Bind positional arguments:
        for ((PosFormal(name),d) <- formals.positionals zip params.positionals) {
          // TODO: Un-hard-code the MapBind; factor into alloc
          newBEnv = (newBEnv(name) = MapBind(name,newTime))
          newStore += (newBEnv(name), d)
        }
        
        // Bind keyword arguments:
        for (KeywordFormal(keyword,name) <- formals.keywords) {
          newBEnv = (newBEnv(name) = MapBind(name,newTime))
          newStore += (newBEnv(name), params(keyword))
        }
        
        if (formals.positionals.length < params.positionals.length) {
          // Stuff the rest into a list.
          val remainder = params.positionals.drop(formals.positionals.length)
          throw new Exception(exp + "\nv.\n" + lam)
        }
        
        List(State(CFlat(call,newBEnv,newTime),StoreSharp(newStore)))
      }

      
      case PrimValue("*"|"+"|"-"|"/"|"quotient"|"gcd"|"modulo") => {
        val conts = params(SKeyword.from("cc"))
        val primParams = botD :: (new Parameters())
        for (cont <- conts.toList; succ <- applyProcedure (InternalPrimArguments,primParams,store,newTime) (cont)) yield {
          succ
        }
      }

      case PrimValue("ceiling"|"log"|"length"|"char->integer") => {
        val conts = params(SKeyword.from("cc"))
        val primParams = botD :: (new Parameters())
        for (cont <- conts.toList; succ <- applyProcedure (InternalPrimArguments,primParams,store,newTime) (cont)) yield {
          succ
        }
      }


      case PrimValue("string-append"|"number->string"|"string-ref"|"string-length"|"list->string"|"symbol->string") => {
        val conts = params(SKeyword.from("cc"))
        val primParams = botD :: (new Parameters())
        for (cont <- conts.toList; succ <- applyProcedure (InternalPrimArguments,primParams,store,newTime) (cont)) yield {
          succ
        }
      }

      case PrimValue("string->symbol") => {
        val conts = params(SKeyword.from("cc"))
        val primParams = botD :: (new Parameters())
        for (cont <- conts.toList; succ <- applyProcedure (InternalPrimArguments,primParams,store,newTime) (cont)) yield {
          succ
        }
      }

      case PrimValue("display"|"newline") => {
        val conts = params(SKeyword.from("cc"))
        val primParams = botD :: (new Parameters())
        for (cont <- conts.toList; succ <- applyProcedure (InternalPrimArguments,primParams,store,newTime) (cont)) yield {
          succ
        }
      }

      case PrimValue("random") => {
        val conts = params(SKeyword.from("cc"))
        val primParams = botD :: (new Parameters())
        for (cont <- conts.toList; succ <- applyProcedure (InternalPrimArguments,primParams,store,newTime) (cont)) yield {
          succ
        }
      }

      case PrimValue("cons") => {
        if (params.positionals.length != 2)
          List()
        else {
          val conts = params(SKeyword.from("cc"))
          val loc = ConsLocation(newTime)
          val primParams = (botD + loc) :: (new Parameters())
          val carD = params(0)
          var cdrD = params(1)
          val carAddr = FieldAddr(loc,SName.from("car"))
          val cdrAddr = FieldAddr(loc,SName.from("cdr"))
          var newStore = store
          newStore = (newStore(carAddr) = carD)
          newStore = (newStore(cdrAddr) = cdrD)
          for (cont <- conts.toList; succ <- applyProcedure (InternalPrimArguments,primParams,store,newTime) (cont)) yield {
            succ
          }
        }
      }

      case PrimValue("not"|"equal?"|"eqv?"|"eq?"|"odd?"|"even?"|"char?"|"char=?"|"char-alphabetic?"|"char-numeric?"|"string<?"|"boolean?"|"procedure?"|
                     "string?"|
                     "symbol?"|"pair?"|"list?"|"null?"|"number?"|"integer?"|"<"|"="|">"|"<="|">=") => {
        val conts = params(SKeyword.from("cc"))
        val primParams = (botD + BooleanValue(true) + BooleanValue(false)) :: (new Parameters())
        for (cont <- conts.toList; succ <- applyProcedure (InternalPrimArguments,primParams,store,newTime) (cont)) yield {
          succ
        }
      }

      case PrimValue(field @ ("car"|"cdr")) => {
        val conts = params(SKeyword.from("cc"))
        if (params.positionals.length == 1) {
          val cellLocs = params(0)
          val statess : List[List[State]] =
            for (cellLoc <- cellLocs.toList if cellLoc.isObjectLocation) yield {
              val loc = cellLoc.asInstanceOf[ObjectLocation]
              val fieldValue = store.getOrElse(FieldAddr(loc,SName.from("cons")),botD)
              val primParams = fieldValue :: (new Parameters())
              val states : List[State] = 
                for (cont <- conts.toList; 
                     succ <- applyProcedure (InternalPrimArguments,primParams,store,newTime) (cont)) yield {
                       succ
                     } 
              states
            }
          val states : List[State] = statess.flatMap(states => states)
          states
        } else {
          List()
        }
      }

      case PrimValue("error") => List()


    }
  }


  
  def next (state : State) : List[State] = {
    state match {
      
      case State(CFlat(exp @ App(f,args),bEnv,t),StoreSharp(store)) => {
        val procs = atomEval (bEnv,store) (f)
        val params = evalArgs (args,bEnv,store)
        val newTime = tick(exp,t)
        for (procValue <- procs.toList if procValue.isProcedure; 
             succ <- applyProcedure (args,params,store,newTime) (procValue)) yield {
          succ
        }
      }

      case State(CFlat(exp @ If(condition,ifTrue,ifFalse),bEnv,t),StoreSharp(store)) => {
        for (call <- List(ifTrue,ifFalse)) yield {
          val newTime = tick (exp,t)
          State(CFlat(call,bEnv,newTime),StoreSharp(store))
        }
      }

      case State(CFlat(exp @ Seq(SetBang(name,value),call),bEnv,t),StoreSharp(store)) => {
        val d = atomEval (bEnv,store) (value)

        val newStore = (store(bEnv(name)) = d)

        List(State(CFlat(call,bEnv,t),StoreSharp(newStore)))
      }

      // Non-call expression: Halt!
      case State(CFlat(ae @ (_ : Ref | _ : Lit | _ : Undefined),bEnv,t),StoreSharp(store)) => {
        System.out.println("Halting state; final value: " + atomEval (bEnv,store) (ae)) // DEBUG
        List()
      }

      case _ => throw new Exception("unhandled state: " + state)
    }
  }  

}




object Store {
  
  // TODO: The code below produces raw stats for comparing analyses.
  // The code will be scrapped and rewritten to produce useful
  // information after the PLDI 2010 deadline.

  type FlowMap = SortedMap[SName,SortedSet[Int]]

  def condense (store : Store) : SortedMap[SName,SortedSet[Int]] = {
    var map : FlowMap = TreeMap[SName,SortedSet[Int]]()

    
    val set : SortedSet[Int] = TreeSet[Int]()

    for ((addr,d) <- store.toList) {
      map =
        addr match {
          case FlatBind(name,_) => {
            (map get name) match {
              case Some(flowSet) => map + (name -> (flowSet ++ (d.toList map (_.toSourceLabel))))
              case None => map + (name -> (set ++ (d.toList map (_.toSourceLabel))))
            }
          }
          case MapBind(name,_) => {
            (map get name) match {
              case Some(flowSet) => map + (name -> (flowSet ++ (d.toList map (_.toSourceLabel))))
              case None => map + (name -> (set ++ (d.toList map (_.toSourceLabel))))
            }
          }
          case _ =>
            map
        }
    }

    map
  }

  def countInlinable (flowMap : FlowMap) : Int = {
    var inlinable = 0 

    for ((name,flows) <- flowMap) {
      if (flows.size == 1 && !(flows exists (n => n < 0))) {
        inlinable += 1
      }
    }
    
    inlinable
  }

  abstract class Score
  case object LeftWins extends Score
  case object RightWins extends Score
  case object Tie extends Score
  case object NeitherWins extends Score

  def crossAnalyze (flowMap1 : FlowMap, flowMap2 : FlowMap) {
    var scoreMap : SortedMap[SName,Score] = TreeMap[SName,Score]()
   
    for ((name,flows) <- flowMap1) {
      (flowMap1 get name,flowMap2 get name) match {
        case (Some(flows1),Some(flows2)) => {

          println("pflows["+name+"]: " +flows1)
          println("mflows["+name+"]: " +flows2)

          val lw = flows1 subsetOf flows2
          var rw = flows2 subsetOf flows1

          println("lw: " + lw)
          println("rw: " + rw)

          if (lw && rw)
            scoreMap = (scoreMap + (name -> Tie))
          else if (lw)
            scoreMap = (scoreMap + (name -> LeftWins))
          else if (rw)
            scoreMap = (scoreMap + (name -> RightWins))
          else
            scoreMap = (scoreMap + (name -> NeitherWins))
        }
        case (Some(_), None) => {
          scoreMap = (scoreMap + (name -> RightWins))
        }
        case (None,Some(_)) =>
          scoreMap = (scoreMap + (name -> LeftWins))
      }
    }


    for ((name,flows) <- flowMap2) {
      (flowMap1 get name,flowMap2 get name) match {
        case (Some(flows1),Some(flows2)) => {
          val lw = flows1 subsetOf flows2
          var rw = flows2 subsetOf flows1
          if (lw && rw)
            scoreMap = (scoreMap + (name -> Tie))
          else if (lw)
            scoreMap = (scoreMap + (name -> LeftWins))
          else if (rw)
            scoreMap = (scoreMap + (name -> RightWins))
          else
            scoreMap = (scoreMap + (name -> NeitherWins))
        }
        case (Some(_), None) => {
          scoreMap = (scoreMap + (name -> RightWins))
        }
        case (None,Some(_)) =>
          scoreMap = (scoreMap + (name -> LeftWins))
      }
    }

    var LW = 0
    var RW = 0
    var TIE = 0 
    var NEITHER = 0

    for ((name,score) <- scoreMap) {
      if (score == LeftWins)
        LW += 1
      else if (score == RightWins)
        RW += 1
      else if (score == Tie)
        TIE += 1
      else
        NEITHER += 1
    }
 
    println("LW:      " + LW + "\n ", 
            "RW:      " + RW + "\n ", 
            "TIE:     " + TIE + "\n ", 
            "NEITHER: " + NEITHER + "\n ")
  }
}






class MCFA_CPS(exp : Exp, bEnv0 : BEnv, t0 : Time, store0 : Store, botD : D) extends SmallStepAbstractInterpretation {

  var k : Int = 1
  var m : Int = 1 

  var flatPolicy : String = "k"

  val bEnv1 = bEnv0
  val store1 = RnRSPrimitives.list.foldRight (store0) ((name,store) => store(bEnv1(SName.from(name))) = botD + PrimValue(name))

  def atomEval (bEnv : BEnv, store : Store) (exp : Exp) : D = exp match {
    case Lit(SBoolean(value)) => botD + BooleanValue(value)
    case _ : Lit => botD
    case Undefined() => botD
    case Ref(name) => {
      val addr = bEnv(name)
      (store get addr) match {
        case Some(d) => d
        case None => {
          throw new Exception("could not find address: " + addr)
        }
      }
    }
    case Void() => botD
    case lam : Lambda => {
      botD + Clo(lam,bEnv)
    }
  }

  def inject (exp : Exp) : State = {
    State(CFlat(exp,bEnv0,t0),StoreSharp(store1))
  }

  lazy val initialState = inject(exp)


  def tick (call : Exp, t : Time) : Time = t.succ(k,call.label)

  def allocateBEnv (exp : Exp, current : BEnv, lam : Lambda, captured : BEnv, nextTime : Time) : BEnv = {
    flatPolicy match {
      case "m" => 
        if (lam.isInstanceOf[ULambda])
          current.asInstanceOf[FlatBEnv].succ(m,exp.label)
        else
          captured
      case "k" =>
        FlatBEnv(nextTime.asInstanceOf[KTime].last)
    }
  }


  def evalArgs (args : Arguments, bEnv : BEnv, store : Store) : Parameters = {
    args match {
      case ListArguments(arglist) => evalArgs (new Parameters()) (arglist,bEnv,store)
    }
  }

  def evalArgs (parameters : Parameters) (arglist : List[Argument], bEnv : BEnv, store : Store) : Parameters = {
    arglist match {
      case Nil => parameters
      case hd :: tl => {
        val p = evalArgs (parameters) (tl,bEnv,store)
        hd match {
          case PosArgument(exp) => (atomEval (bEnv,store) (exp)) :: p
          case KeywordArgument(kw,exp) => p(kw) = atomEval (bEnv,store) (exp)
        }
      }
    }
  }


  private def applyProcedure (allocBEnv : (Lambda,BEnv) => BEnv) (args : Arguments, params : Parameters, store : Store, newTime : Time) (proc : Value) : List[State] = {
    proc match {

      case PrimValue("*"|"+"|"-"|"/"|"quotient"|"gcd"|"modulo") => {
        val conts = params(SKeyword.from("cc"))
        val primParams = botD :: (new Parameters())
        for (cont <- conts.toList; succ <- applyProcedure (allocBEnv) (InternalPrimArguments,primParams,store,newTime) (cont)) yield {
          succ
        }
      }

      case PrimValue("ceiling"|"log"|"length"|"char->integer") => {
        val conts = params(SKeyword.from("cc"))
        val primParams = botD :: (new Parameters())
        for (cont <- conts.toList; succ <- applyProcedure (allocBEnv) (InternalPrimArguments,primParams,store,newTime) (cont)) yield {
          succ
        }
      }

      case PrimValue("string-append"|"number->string"|"string-length"|"string-ref"|"list->string"|"symbol->string") => {
        val conts = params(SKeyword.from("cc"))
        val primParams = botD :: (new Parameters())
        for (cont <- conts.toList; succ <- applyProcedure (allocBEnv) (InternalPrimArguments,primParams,store,newTime) (cont)) yield {
          succ
        }
      }

      case PrimValue("string->symbol") => {
        val conts = params(SKeyword.from("cc"))
        val primParams = botD :: (new Parameters())
        for (cont <- conts.toList; succ <- applyProcedure (allocBEnv) (InternalPrimArguments,primParams,store,newTime) (cont)) yield {
          succ
        }
      }

      case PrimValue("display"|"newline") => {
        val conts = params(SKeyword.from("cc"))
        val primParams = botD :: (new Parameters())
        for (cont <- conts.toList; succ <- applyProcedure (allocBEnv) (InternalPrimArguments,primParams,store,newTime) (cont)) yield {
          succ
        }
      }

      case PrimValue("random") => {
        val conts = params(SKeyword.from("cc"))
        val primParams = botD :: (new Parameters())
        for (cont <- conts.toList; succ <- applyProcedure (allocBEnv) (InternalPrimArguments,primParams,store,newTime) (cont)) yield {
          succ
        }
      }

      case PrimValue("cons") => {
        if (params.positionals.length != 2)
          List()
        else {
          val conts = params(SKeyword.from("cc"))
          val loc = ConsLocation(newTime)
          val primParams = (botD + loc) :: (new Parameters())
          val carD = params(0)
          var cdrD = params(1)
          val carAddr = FieldAddr(loc,SName.from("car"))
          val cdrAddr = FieldAddr(loc,SName.from("cdr"))
          var newStore = store
          newStore = (newStore(carAddr) = carD)
          newStore = (newStore(cdrAddr) = cdrD)
          for (cont <- conts.toList; succ <- applyProcedure (allocBEnv) (InternalPrimArguments,primParams,store,newTime) (cont)) yield {
            succ
          }
        }
      }

      // Anything that returns a Boolean:
      case PrimValue("not"|"equal?"|"eqv?"|"eq?"|"odd?"|"even?"|"char?"|"char=?"|"char-alphabetic?"|"char-numeric?"|"string<?"|"boolean?"|
                     "procedure?"|"string?"|
                     "symbol?"|"pair?"|"list?"|"null?"|"integer?"|"number?"|"<"|"="|">"|"<="|">=") => {
        val conts = params(SKeyword.from("cc"))
        val primParams = (botD + BooleanValue(true) + BooleanValue(false)) :: (new Parameters())
        for (cont <- conts.toList; succ <- applyProcedure (allocBEnv) (InternalPrimArguments,primParams,store,newTime) (cont)) yield {
          succ
        }
      }

      case PrimValue(field @ ("car"|"cdr")) => {
        val conts = params(SKeyword.from("cc"))
        if (params.positionals.length == 1) {
          val cellLocs = params(0)
          val statess : List[List[State]] =
            for (cellLoc <- cellLocs.toList if cellLoc.isObjectLocation) yield {
              val loc = cellLoc.asInstanceOf[ObjectLocation]
              val fieldValue = store.getOrElse(FieldAddr(loc,SName.from("cons")),botD)
              val primParams = fieldValue :: (new Parameters())
              val states : List[State] = 
                for (cont <- conts.toList; 
                     succ <- applyProcedure (allocBEnv) (InternalPrimArguments,primParams,store,newTime) (cont)) yield {
                       succ
                     } 
              states
            }
          val states : List[State] = statess.flatMap(states => states)
          states
        } else {
          List()
        }
      }

      case PrimValue("error") => List()
      
      case Clo(lam @ Lambda(VarFormals(name),ExpBody(call)),bEnv2) if !(call.free contains name)  => {
        val newBEnv = allocBEnv(lam,bEnv2) // allocateBEnv(exp,bEnv,lam,bEnv2,newTime)
        var newStore = store 
        if (newBEnv != bEnv2)
          for (x <- lam.free) {
            newStore += (newBEnv(x),store(bEnv2(x)))
          }
        List(State(CFlat(call,newBEnv,newTime),StoreSharp(newStore)))
      }
      
      
      case Clo(lam @ Lambda(formals,ExpBody(call)),bEnv2) if formals accepts args => {
        
        val newBEnv = allocBEnv(lam,bEnv2) // allocateBEnv(exp,bEnv,lam,bEnv2,newTime)
        
        var newStore = store 
        
        // Bind positional arguments:
        for ((PosFormal(name),d) <- formals.positionals zip params.positionals)  {
          newStore += (newBEnv(name), d)
        }
        
        // Bind keyword arguments:
        for (KeywordFormal(keyword,name) <- formals.keywords) {
          newStore += (newBEnv(name), params(keyword))
        }
        
        // Copy free variables from the old environment to the new:
        // println("[[" +lam+ "]].free = " + lam.free) // DEBUG 
        if (newBEnv != bEnv2) 
          for (x <- lam.free) {
            //println("Copying free variable: " + newBEnv(x) + " from " + bEnv2(x)) // DEBUG
            newStore += (newBEnv(x),store(bEnv2(x)))
          }
        
        if (formals.positionals.length < params.positionals.length) {
          // Stuff the rest into a list.
          val remainder = params.positionals.drop(formals.positionals.length)
          throw new Exception()
        }
        
        List(State(CFlat(call,newBEnv,newTime),StoreSharp(newStore)))
      }
      
      
      // case _ => throw new Exception("transition not complete")
    }    
  }



  
  def next (state : State) : List[State] = {
    state match {
      
      case State(CFlat(exp @ App(f,args),bEnv,t),StoreSharp(store)) => {
        val procs = atomEval (bEnv,store) (f)
        val params = evalArgs(args,bEnv,store)
        val newTime = tick(exp,t)
        def allocBEnv (lam : Lambda, bEnv2 : BEnv) = allocateBEnv(exp,bEnv,lam,bEnv2,newTime)
        for (procValue <- procs.toList if procValue.isProcedure; 
             succ <- applyProcedure (allocBEnv) (args,params,store,newTime) (procValue)) yield {
          succ
        }
      }

      case State(CFlat(exp @ If(condition,ifTrue,ifFalse),bEnv,t),StoreSharp(store)) => {
        for (call <- List(ifTrue,ifFalse)) yield {
          val newTime = tick (exp,t)
          State(CFlat(call,bEnv,newTime),StoreSharp(store))
        }
      }

      case State(CFlat(exp @ Seq(SetBang(name,value),call),bEnv,t),StoreSharp(store)) => {
        val d = atomEval (bEnv,store) (value)

        val newStore = (store(bEnv(name)) = d)

        List(State(CFlat(call,bEnv,t),StoreSharp(newStore)))
      }

      // Non-call expression: Halt!
      case State(CFlat(ae @ (_ : Ref | _ : Lit | _ : Undefined),bEnv,t),StoreSharp(store)) => {
        System.out.println("Halting state; final value: " + atomEval (bEnv,store) (ae)) // DEBUG
        List()
      }

      case _ => throw new Exception("unhandled state: " + state)
    }
  }

}





class CFAOptions {
  var file : String = null ;
  var k = 1
  var m = 1
  var printStates = false ;
  var flatPolicy = "m"
  var analysis = "flat"
}


object CFAOptions {

  def parse(args : List[String], opts : CFAOptions) : Unit = args match {
    case List() => {}
    case "--k" :: k :: rest => { 
      opts.k = Integer.parseInt(k)
      parse(rest,opts)
    }

    case "--m" :: m :: rest => { 
      opts.m = Integer.parseInt(m)
      parse(rest,opts)
    }

    case "--flat-policy" :: s :: rest => { 
      opts.flatPolicy = s
      parse(rest,opts)
    }

    case "--analysis" :: a :: rest => { 
      opts.analysis = a
      parse(rest,opts)
    }

    case "--print-states" :: "true" :: rest => {
      opts.printStates = true
      parse(rest,opts)
    }

    case "--print-states" :: "false" :: rest => {
      opts.printStates = false
      parse(rest,opts)
    }

    case fileName :: rest => {
      opts.file = fileName ;
      parse(rest,opts)
    }

  }

  def parse(args : Array[String]) : CFAOptions = {
    val opts = new CFAOptions
    parse(args.toList,opts)
    opts
  }

}




object RunCFA {

  def main (args : Array[String]) {

    val opts = CFAOptions.parse(args)

    if (opts.file == null) {
      System.err.println ("Please specify a filename.")
      return 
    }

    val filename = opts.file
    System.err.print("Parsing s-expressions...")
    val sexps = SExp.parseAllIn(filename)
    System.err.println("done")
    
    System.err.print("Bulding AST...")
    val ast = RnRSParser(sexps)
    System.err.println("done") 
    
    System.err.print("A-normalizing...")
    val anast = ANormalizer(ast)
    System.err.println("done") 
    
    System.err.print("CPS-converting...")
    val cpast = CPSConverter(anast)
    System.err.println("done") 


    System.out.println("Input program:")
    System.out.println(ast)
    System.out.println("\n")

    System.out.println("ANF program:")
    System.out.println(anast)
    System.out.println("\n")

    System.out.println("CPS program:")
    System.out.println(cpast)
    System.out.println("\n")

    System.out.println("Free variables:")    
    System.out.println(cpast.free)
    System.out.println("\n")

    opts.analysis match {
     case "full" => { 
       val CFA = new KCFA_CPS(cpast,new MapBEnv(TreeMap()), KTime(List()), new MapStore(), new SortedSetD())
       CFA.printStates = opts.printStates
       CFA.k = opts.k
       CFA.runWithGlobalSharp()
       val sharp = CFA.globalSharp
       val store = sharp.asInstanceOf[StoreSharp].store
       val flows = Store.condense (store)
       val inlinable = Store.countInlinable(flows)
       println ("inlinable: " + inlinable)
     }
     case "flat" => { 
       val CFA = new MCFA_CPS(cpast,new FlatBEnv(List()), KTime(List()), new MapStore(), new SortedSetD())
       CFA.printStates = opts.printStates
       CFA.k = opts.k
       CFA.m = opts.m
       CFA.flatPolicy = opts.flatPolicy
       CFA.runWithGlobalSharp()
       val sharp = CFA.globalSharp
       val store = sharp.asInstanceOf[StoreSharp].store
       val flows = Store.condense (store)
       val inlinable = Store.countInlinable(flows)
       println ("inlinable: " + inlinable)
     }
      
     case "compare-m-poly-1" => {
       val P1CFA = new MCFA_CPS(cpast,new FlatBEnv(List()), KTime(List()), new MapStore(), new SortedSetD())
       P1CFA.k = 1
       P1CFA.m = 1
       P1CFA.flatPolicy = "k"
       P1CFA.runWithGlobalSharp()
       val pSharp = P1CFA.globalSharp
       val pStore = pSharp.asInstanceOf[StoreSharp].store
       val pFlows = Store.condense (pStore)
       
       val M1CFA = new MCFA_CPS(cpast,new FlatBEnv(List()), KTime(List()), new MapStore(), new SortedSetD())
       M1CFA.k = 1
       M1CFA.m = 1
       M1CFA.flatPolicy = "m"
       M1CFA.runWithGlobalSharp()
       val mSharp = M1CFA.globalSharp
       val mStore = mSharp.asInstanceOf[StoreSharp].store
       val mFlows = Store.condense (mStore)
       
       Store.crossAnalyze(pFlows,mFlows)
     }
    }


    ()
  }

}



