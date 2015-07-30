package org.ucombinator

class Alphatiser {

  type Var = SName
  type Seen = collection.mutable.Set[Var]
  type Env = Map[Var, Var]

  def alphatise (p : Program) : Program = {
    val seen = collection.mutable.Set[Var]()
    val env = Map[Var, Var]()
    p match {
      case Program(defs) =>
        val newEnv = extendDefs(defs)(seen,env)
        Program(alphatiseDefs(defs)(seen,newEnv))
    }
  }


  def lookup(env: Env, v : Var): Var = {
    env.getOrElse(v, v)
  }


  def extend (name : SName) (seen : Seen, env : Env) : Env = {
    if (seen.contains(name)) {
      val n = SName.gensym(name.s)
      env + (name -> n)
    }
    else {
      seen += name
      env
    }
  }

  def extendDefs (defs : List[Def]) (seen : Seen, env : Env) : Env = {
    defs.foldLeft(env)((env, defn) => defn match {
      case ImplicitDef(exp) => env
      case VarDef(name, exp) => extend(name)(seen,env)
      case FunctionDef(name, formals, body) => extend(name)(seen,env)
    })
  }

  def extendFormals (fs : Formals) (seen : Seen, env : Env) : Env = {
    fs match {
      case ListFormals(formals) => extendFormalsList(formals)(seen,env)
      case MultiFormals(formals, rest) => extend(rest)(seen,extendFormalsList(formals)(seen,env))
      case VarFormals(rest) => extend(rest)(seen,env)
    }
  }

  def extendFormalsList (formals : List[Formal]) (seen : Seen, env : Env) : Env = {
    formals.foldLeft(env)((env, formal) => formal match {
      case KeywordFormal(keyword, name) => extend(name)(seen,env)
      case PosFormal(name) => extend(name)(seen,env)
    })
  }

  def extendBindings (bs : Bindings) (seen : Seen, env : Env) : Env = bs match {
    case Bindings(bindings) =>
      bindings.foldLeft(env)((env, binding) => binding match {
        case Binding(name, value) => extend(name)(seen,env)
      })
  }


  def alphatiseDefs (defs : List[Def]) (seen : Seen, env : Env) : List[Def] = {
    defs map (defn => alphatise(defn)(seen,env))
  }

  def alphatise (defn : Def) (seen : Seen, env : Env) : Def = defn match {
    case ImplicitDef(exp) =>
      ImplicitDef(alphatise(exp)(seen,env))
    case VarDef(name, exp) =>
      VarDef(lookup(env, name), alphatise(exp)(seen,env))
    case FunctionDef(name, formals, body) =>
      val newEnv = extendFormals(formals)(seen,env)
      FunctionDef(lookup(env, name), alphatise(formals)(seen,newEnv), alphatise(body)(seen,newEnv))
  }

  def alphatise (exp : Exp) (seen : Seen, env : Env) : Exp = exp match {
    case Ref(name) => Ref(lookup(env, name))
    case Lit(sexp) => exp
    case Undefined() => exp
    case Void() => exp

    case Lambda(formals, body) =>
      val newEnv = extendFormals(formals)(seen,env)
      Lambda(alphatise(formals)(seen,newEnv), alphatise(body)(seen,newEnv))

    case If(cond,ifTrue,ifFalse) =>
      If(alphatise(cond)(seen,env),alphatise(ifTrue)(seen,env),alphatise(ifFalse)(seen,env))
    case Cond(clauses) =>
      Cond(clauses map (clause => alphatise(clause)(seen,env)))
    case Or(exps) =>
      Or(exps map (exp => alphatise(exp)(seen,env)))
    case And(exps) =>
      And(exps map (exp => alphatise(exp)(seen,env)))

    case Let(bindings, body) =>
      val newEnv = extendBindings(bindings)(seen,env)
      val newBindings = bindings match {
        case Bindings(bs) =>
          Bindings(bs map {
            case Binding(name, value) => Binding(lookup(newEnv, name), alphatise(value)(seen, env))
          })
      }
      Let(newBindings, alphatise(body)(seen,newEnv))

    case LetStar(bindings, body) =>
      var valEnv = env
      val newBindings = bindings match {
        case Bindings(bs) =>
          Bindings(bs map {
            case Binding(name, value) =>
              val newValue = alphatise(value)(seen,valEnv)
              valEnv = extend(name)(seen,valEnv)
              Binding(lookup(valEnv, name), newValue)
          })
      }
      LetStar(newBindings, alphatise(body)(seen,valEnv))

    case LetRec(bindings, body) =>
      val newEnv = extendBindings(bindings)(seen,env)
      LetRec(alphatise(bindings)(seen,newEnv), alphatise(body)(seen,newEnv))

    case SetBang(name, value) =>
      SetBang(lookup(env, name), alphatise(value)(seen,env))

    case Begin(body) =>
      Begin(alphatise(body)(seen,env))

    case App(fun, ListArguments(args)) =>
      App(alphatise(fun)(seen,env), ListArguments(args map (arg => alphatise(arg)(seen,env))))
  }

  def alphatise (formals : Formals) (seen : Seen, env : Env) : Formals = formals match {
    case ListFormals(fs) => ListFormals(alphatiseFormalsList(fs)(seen,env))
    case MultiFormals(fs, rest) => MultiFormals(alphatiseFormalsList(fs)(seen,env), lookup(env, rest))
    case VarFormals(rest) => VarFormals(lookup(env, rest))
  }

  def alphatiseFormalsList (formals : List[Formal]) (seen : Seen, env : Env) : List[Formal] = {
    formals map (formal => alphatise(formal)(seen,env))
  }

  def alphatise (formal : Formal) (seen : Seen, env : Env) : Formal = formal match {
    case KeywordFormal(keyword, name) => KeywordFormal(keyword, lookup(env, name))
    case PosFormal(name) => PosFormal(lookup(env, name))
  }

  def alphatise (body : Body) (seen : Seen, env : Env) : Body = body match {
    case Body(defs, exps) =>
      val newEnv = extendDefs(defs)(seen,env)
      Body(alphatiseDefs(defs)(seen,newEnv), exps map (exp => alphatise(exp)(seen,newEnv)))
  }

  def alphatise (clause : CondClause) (seen : Seen, env : Env) : CondClause = clause match {
    case SelfCondClause(test) =>
      SelfCondClause(alphatise(test)(seen,env))
    case TestCondClause(test, exps) =>
      TestCondClause(alphatise(test)(seen,env), exps map (exp => alphatise(exp)(seen,env)))
    case ProcCondClause(test, proc) =>
      ProcCondClause(alphatise(test)(seen,env), alphatise(proc)(seen,env))
    case ElseCondClause(exps) =>
      ElseCondClause(exps map (exp => alphatise(exp)(seen,env)))
  }

  def alphatise (argument : Argument) (seen : Seen, env : Env) : Argument = argument match {
    case KeywordArgument(keyword, exp) => KeywordArgument(keyword, alphatise(exp)(seen,env))
    case PosArgument(exp) => PosArgument(alphatise(exp)(seen,env))
  }

  def alphatise (bindings : Bindings) (seen : Seen, env : Env) : Bindings = bindings match {
    case Bindings(bs) =>
      Bindings(bs map {
        case Binding(name, value) => Binding(lookup(env, name), alphatise(value)(seen, env))
      })
  }

}


object Alphatiser {
  def apply(p: Program): Program = {
    val t = new Alphatiser
    t.alphatise(p)
  }
}


