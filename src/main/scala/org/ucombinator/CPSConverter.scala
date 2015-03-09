package org.ucombinator


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

