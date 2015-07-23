package org.ucombinator

class KCFA(exp : Exp, bEnv0 : BEnv, t0 : Time, store0 : Store, val botD : D) extends CFA {

  var k : Int = 0

  def allocateBEnv (exp : Exp, current : BEnv, nextTime : Time)
                   (lam : Lambda, captured : BEnv, store : Store) : (BEnv, Store) = {
    (captured, store)
  }

  def extendBEnv (env : BEnv, name : SName, time : Time): BEnv = {
    env(name) = MapBind(name,time)
  }

  lazy val initialState = inject(exp)

  def inject (exp : Exp) : State = {
    val bEnv1 = RnRSPrimitives.list.foldRight (bEnv0) ((name,bEnv) => bEnv(SName.from(name)) = PrimAddr(SName.from(name)))
    val store1 = RnRSPrimitives.list.foldRight (store0) ((name,store) => store(bEnv1(SName.from(name))) = botD + PrimValue(name))
    State(CFlat(exp,bEnv1,t0),StoreSharp(store1))
  }

}
