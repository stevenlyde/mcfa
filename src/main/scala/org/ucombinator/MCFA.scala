package org.ucombinator

class MCFA(exp : Exp, bEnv0 : BEnv, t0 : Time, store0 : Store, val botD : D) extends CFA {

  var k : Int = 1
  var m : Int = 1

  var flatPolicy : String = "k"

  def allocateBEnv (exp : Exp, current : BEnv, nextTime : Time)
                   (lam : Lambda, captured : BEnv, store : Store) : (BEnv, Store) = {
    val newBEnv = flatPolicy match {
      case "m" =>
        if (lam.isInstanceOf[ULambda])
          current.asInstanceOf[FlatBEnv].succ(m,exp.label)
        else
          captured
      case "k" =>
        FlatBEnv(nextTime.asInstanceOf[KTime].last)
    }

    var newStore = store
    if (newBEnv != captured)
      for (x <- lam.free) {
        newStore += (newBEnv(x),store(captured(x)))
      }

    (newBEnv, newStore)
  }

  def extendBEnv (env : BEnv, name : SName, time : Time): BEnv = {
    env
  }

  lazy val initialState = inject(exp)

  def inject (exp : Exp) : State = {
    val bEnv1 = bEnv0
    val store1 = RnRSPrimitives.list.foldRight (store0) ((name,store) => store(bEnv1(SName.from(name))) = botD + PrimValue(name))
    State(CFlat(exp,bEnv1,t0),StoreSharp(store1))
  }

}
