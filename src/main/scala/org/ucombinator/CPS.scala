package org.ucombinator

trait CFA extends SmallStepAbstractInterpretation {

  val botD : D

  var k : Int

  def allocateBEnv (exp : Exp, current : BEnv, nextTime : Time)
                   (lam : Lambda, captured : BEnv, store : Store) : (BEnv, Store)

  def extendBEnv (env : BEnv, name : SName, time : Time): BEnv


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

  private def applyProcedure (allocBEnv : (Lambda,BEnv,Store) => (BEnv,Store))
                             (args : Arguments, params : Parameters, store : Store, newTime : Time)
                             (proc : Value) : List[State] = {
    proc match {

      case Clo(lam @ Lambda(VarFormals(name),ExpBody(call)),bEnv2) if !(call.free contains name)  => {
        val (newBEnv, newStore) = allocBEnv(lam,bEnv2,store)
        List(State(CFlat(call,newBEnv,newTime),StoreSharp(newStore)))
      }

      case Clo(lam @ Lambda(formals,ExpBody(call)),bEnv2) if formals accepts args => {

        var (newBEnv, newStore) = allocBEnv(lam,bEnv2,store)

        // Bind positional arguments:
        for ((PosFormal(name),d) <- formals.positionals zip params.positionals) {
          newBEnv = extendBEnv(newBEnv, name, newTime)
          newStore += (newBEnv(name), d)
        }

        // Bind keyword arguments:
        for (KeywordFormal(keyword,name) <- formals.keywords) {
          newBEnv = extendBEnv(newBEnv, name, newTime)
          newStore += (newBEnv(name), params(keyword))
        }

        if (formals.positionals.length < params.positionals.length) {
          // Stuff the rest into a list.
          val remainder = params.positionals.drop(formals.positionals.length)
          throw new Exception()
        }

        List(State(CFlat(call,newBEnv,newTime),StoreSharp(newStore)))
      }

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

      case PrimValue("not"|"equal?"|"eqv?"|"eq?"|"odd?"|"even?"|"char?"|"char=?"|"char-alphabetic?"|"char-numeric?"|"string<?"|"boolean?"|"procedure?"|
                     "string?"|"symbol?"|"pair?"|"list?"|"null?"|"number?"|"integer?"|"<"|"="|">"|"<="|">=") => {
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

    }
  }



  def next (state : State) : List[State] = {
    state match {
      
      case State(CFlat(exp @ App(f,args),bEnv,t),StoreSharp(store)) => {
        val procs = atomEval (bEnv,store) (f)
        val params = evalArgs (args,bEnv,store)
        val newTime = tick(exp,t)
        val allocBEnv = allocateBEnv(exp,bEnv,newTime) _
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
