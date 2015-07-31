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

  private def handleRestArgs (formals : Formals, params : Parameters, bEnv : BEnv, store : Store, newTime : Time): (BEnv, Store) = formals match {
    case MultiFormals(fs, r) =>
      val rest = params.positionals.drop(formals.positionals.length)

      var newBEnv = bEnv
      var newStore = store

      newBEnv = (newBEnv(r) = MapBind(r,newTime))

      val loc = ConsLocation(newTime)
      val carD = rest.foldLeft(botD)((a, d) => a join d)
      val cdrD = botD + loc
      val carAddr = FieldAddr(loc,SName.from("car"))
      val cdrAddr = FieldAddr(loc,SName.from("cdr"))
      newStore += (carAddr, carD)
      newStore += (cdrAddr, cdrD)
      newStore += (newBEnv(r), botD + loc)

      (newBEnv, newStore)

    case _ => (bEnv, store)
  }

  private def applyProcedure (args : Arguments, params : Parameters, store : Store, newTime : Time) (proc : Value) : List[State] = {
    proc match {
      
      case Clo(lam @ Lambda(VarFormals(name),ExpBody(call)),bEnv2) if !(call.free contains name)  => {
        List(State(CFlat(call,bEnv2,newTime),StoreSharp(store)))
      }

      case Clo(Lambda(formals,_),_) if !(formals accepts args) => List()
      
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

        val (tmpBEnv, tmpStore) = handleRestArgs(formals, params, newBEnv, newStore, newTime)
        newBEnv = tmpBEnv
        newStore = tmpStore

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
          for (cont <- conts.toList; succ <- applyProcedure (InternalPrimArguments,primParams,newStore,newTime) (cont)) yield {
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
              val fieldValue = store.getOrElse(FieldAddr(loc,SName.from(field)),botD)
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
        //System.out.println("Halting state; final value: " + atomEval (bEnv,store) (ae)) // DEBUG
        List()
      }

      case _ => throw new Exception("unhandled state: " + state)
    }
  }  

}
