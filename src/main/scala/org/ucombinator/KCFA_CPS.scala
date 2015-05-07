package org.ucombinator

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

  def addrEval (bEnv : BEnv, time : Time) (exp : Exp) : Addr = exp match {
    case Ref(name) => bEnv(name)
    case _ => ParamAddr(exp.label, time)
  }

  def inject (exp : Exp) : State = {
    State(CFlat(exp,bEnv1,t0),StoreSharp(store1))
  }

  lazy val initialState = inject(exp)

  def tick (call : Exp, t : Time) : Time = t.succ(k,call.label)

  def evalArgs[A] (args : Arguments, eval : Exp => A)  : Parameters[A] = {
    args match {
      case ListArguments(arglist) => evalArgs (new Parameters()) (arglist, eval)
    }
  }

  def evalArgs[A] (parameters : Parameters[A]) (arglist : List[Argument], eval : Exp => A) : Parameters[A] = {
    arglist match {
      case Nil => parameters
      case hd :: tl => {
        val p = evalArgs (parameters) (tl, eval)
        hd match {
          case PosArgument(exp) => (eval(exp)) :: p
          case KeywordArgument(kw,exp) => p(kw) = eval(exp)
        }
      }
    }
  }


  private def applyProcedure (args : Arguments, params : Parameters[D], paddrs : Parameters[Addr], store : Store, newTime : Time) (proc : Value) : List[State] = {
    proc match {
      
      case Clo(lam @ Lambda(VarFormals(name),ExpBody(call)),bEnv2) if !(call.free contains name)  => {
        List(State(CFlat(call,bEnv2,newTime),StoreSharp(store)))
      }
      
      case Clo(lam @ Lambda(formals,ExpBody(call)),bEnv2) if formals accepts args => {
        
        var newStore = store 
        var newBEnv = bEnv2
        
        // Bind positional arguments:
        for ((PosFormal(name), d, paddr) <- (formals.positionals, params.positionals, paddrs.positionals).zipped) {
          // TODO: Un-hard-code the MapBind; factor into alloc
          val addr = MapBind(name,newTime)
          newBEnv = (newBEnv(name) = addr)
          newStore += (addr, d)
          newStore = newStore union (addr, paddr)
        }
        
        // Bind keyword arguments:
        for (KeywordFormal(keyword,name) <- formals.keywords) {
          val addr = MapBind(name,newTime)
          newBEnv = (newBEnv(name) = addr)
          newStore += (addr, params(keyword))
          newStore = newStore union (addr, paddrs(keyword))
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
        val primParams = botD :: (new Parameters[D]())
        val addrParams = ParamAddr(proc.toSourceLabel, newTime) :: (new Parameters[Addr]())
        for (cont <- conts.toList; succ <- applyProcedure (InternalPrimArguments,primParams,addrParams,store,newTime) (cont)) yield {
          succ
        }
      }

      case PrimValue("ceiling"|"log"|"length"|"char->integer") => {
        val conts = params(SKeyword.from("cc"))
        val primParams = botD :: (new Parameters[D]())
        val addrParams = ParamAddr(proc.toSourceLabel, newTime) :: (new Parameters[Addr]())
        for (cont <- conts.toList; succ <- applyProcedure (InternalPrimArguments,primParams,addrParams,store,newTime) (cont)) yield {
          succ
        }
      }


      case PrimValue("string-append"|"number->string"|"string-ref"|"string-length"|"list->string"|"symbol->string") => {
        val conts = params(SKeyword.from("cc"))
        val primParams = botD :: (new Parameters[D]())
        val addrParams = ParamAddr(proc.toSourceLabel, newTime) :: (new Parameters[Addr]())
        for (cont <- conts.toList; succ <- applyProcedure (InternalPrimArguments,primParams,addrParams,store,newTime) (cont)) yield {
          succ
        }
      }

      case PrimValue("string->symbol") => {
        val conts = params(SKeyword.from("cc"))
        val primParams = botD :: (new Parameters[D]())
        val addrParams = ParamAddr(proc.toSourceLabel, newTime) :: (new Parameters[Addr]())
        for (cont <- conts.toList; succ <- applyProcedure (InternalPrimArguments,primParams,addrParams,store,newTime) (cont)) yield {
          succ
        }
      }

      case PrimValue("display"|"newline") => {
        val conts = params(SKeyword.from("cc"))
        val primParams = botD :: (new Parameters[D]())
        val addrParams = ParamAddr(proc.toSourceLabel, newTime) :: (new Parameters[Addr]())
        for (cont <- conts.toList; succ <- applyProcedure (InternalPrimArguments,primParams,addrParams,store,newTime) (cont)) yield {
          succ
        }
      }

      case PrimValue("random") => {
        val conts = params(SKeyword.from("cc"))
        val primParams = botD :: (new Parameters[D]())
        val addrParams = ParamAddr(proc.toSourceLabel, newTime) :: (new Parameters[Addr]())
        for (cont <- conts.toList; succ <- applyProcedure (InternalPrimArguments,primParams,addrParams,store,newTime) (cont)) yield {
          succ
        }
      }

      case PrimValue("cons") => {
        if (params.positionals.length != 2)
          List()
        else {
          val conts = params(SKeyword.from("cc"))
          val loc = ConsLocation(newTime)
          val primParams = (botD + loc) :: (new Parameters[D]())
          val carD = params(0)
          var cdrD = params(1)
          val carAddr = FieldAddr(loc,SName.from("car"))
          val cdrAddr = FieldAddr(loc,SName.from("cdr"))
          var newStore = store
          newStore = (newStore(carAddr) = carD)
          newStore = (newStore(cdrAddr) = cdrD)
          val addrParams = ParamAddr(proc.toSourceLabel, newTime) :: (new Parameters[Addr]())
          for (cont <- conts.toList; succ <- applyProcedure (InternalPrimArguments,primParams,addrParams,store,newTime) (cont)) yield {
            succ
          }
        }
      }

      case PrimValue("not"|"equal?"|"eqv?"|"eq?"|"odd?"|"even?"|"char?"|"char=?"|"char-alphabetic?"|"char-numeric?"|"string<?"|"boolean?"|"procedure?"|
                     "string?"|
                     "symbol?"|"pair?"|"list?"|"null?"|"number?"|"integer?"|"<"|"="|">"|"<="|">=") => {
        val conts = params(SKeyword.from("cc"))
        val primParams = (botD + BooleanValue(true) + BooleanValue(false)) :: (new Parameters[D]())
        val addrParams = ParamAddr(proc.toSourceLabel, newTime) :: (new Parameters[Addr]())
        for (cont <- conts.toList; succ <- applyProcedure (InternalPrimArguments,primParams,addrParams,store,newTime) (cont)) yield {
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
              val primParams = fieldValue :: (new Parameters[D]())
              val addrParams = ParamAddr(proc.toSourceLabel, newTime) :: (new Parameters[Addr]())
              val states : List[State] = 
                for (cont <- conts.toList; 
                     succ <- applyProcedure (InternalPrimArguments,primParams,addrParams,store,newTime) (cont)) yield {
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

      case _ => List() // imprecision might force the arguments to not be accepted anymore

    }
  }


  
  def next (state : State) : List[State] = {
    state match {
      
      case State(CFlat(exp @ App(f,args),bEnv,t),StoreSharp(store)) => {
        val procs = atomEval (bEnv,store) (f)
        val params = evalArgs (args, atomEval(bEnv, store))
        val addrs = evalArgs (args, addrEval(bEnv, t))
        val newTime = tick(exp,t)
        for {
          procValue <- procs.toList if procValue.isProcedure
          succ <- applyProcedure (args,params,addrs,store,newTime) (procValue)
        } yield succ
      }

      case State(CFlat(exp @ If(condition,ifTrue,ifFalse),bEnv,t),StoreSharp(store)) => {
        for (call <- List(ifTrue,ifFalse)) yield {
          val newTime = tick (exp,t)
          State(CFlat(call,bEnv,newTime),StoreSharp(store))
        }
      }

      case State(CFlat(exp @ Seq(SetBang(name,value),call),bEnv,t),StoreSharp(store)) => {
        val d = atomEval (bEnv,store) (value)
        val da = addrEval (bEnv,t) (value)

        val a = bEnv(name)
        val newStore = (store(a) = d) union (a, da)

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
