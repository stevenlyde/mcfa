package org.ucombinator

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


