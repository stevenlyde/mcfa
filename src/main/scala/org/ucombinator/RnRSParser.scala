package org.ucombinator

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
