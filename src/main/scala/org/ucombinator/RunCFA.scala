package org.ucombinator

import scala.collection.immutable.{TreeMap}

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

    ExpressionFold(cpast, 0, (_: Int, e) => e.label)

    opts.analysis match {
     case "full" => { 
       val CFA = new KCFA_CPS(cpast,new MapBEnv(TreeMap()), KTime(List()), new MapStore(), new SortedSetD())
       CFA.printStates = opts.printStates
       CFA.k = opts.k
       val assignment = PriorityAssignment(opts.ordering)
       val start = System.currentTimeMillis
       CFA.runWithGlobalSharp(assignment)
       val time = System.currentTimeMillis - start
       val sharp = CFA.globalSharp
       val store = sharp.asInstanceOf[StoreSharp].store
       if (opts.writeStore) {
         val writer = new StoreSymbolicExpressionWriter(store)
         writer.write(opts)
       }
       val flows = Store.condense (store)
       val inlinable = Store.countInlinable(flows)
       println ("inlinable: " + inlinable)
       println ("analysis time: " + time)
     }
     case "flat" => { 
       val CFA = new MCFA_CPS(cpast,new FlatBEnv(List()), KTime(List()), new MapStore(), new SortedSetD())
       CFA.printStates = opts.printStates
       CFA.k = opts.k
       CFA.m = opts.m
       CFA.flatPolicy = opts.flatPolicy
       val assignment = PriorityAssignment(opts.ordering)
       val start = System.currentTimeMillis
       CFA.runWithGlobalSharp(assignment)
       val time = System.currentTimeMillis - start
       val sharp = CFA.globalSharp
       val store = sharp.asInstanceOf[StoreSharp].store
       if (opts.writeStore) {
         val writer = new StoreSymbolicExpressionWriter(store)
         writer.write(opts)
       }
       val flows = Store.condense (store)
       val inlinable = Store.countInlinable(flows)
       println ("inlinable: " + inlinable)
       println ("analysis time: " + time)
     }
      
     case "compare-m-poly-1" => {
       val P1CFA = new MCFA_CPS(cpast,new FlatBEnv(List()), KTime(List()), new MapStore(), new SortedSetD())
       P1CFA.k = 1
       P1CFA.m = 1
       P1CFA.flatPolicy = "k"
       P1CFA.runWithGlobalSharp(PriorityAssignment(opts.ordering))
       val pSharp = P1CFA.globalSharp
       val pStore = pSharp.asInstanceOf[StoreSharp].store
       val pFlows = Store.condense (pStore)
       
       val M1CFA = new MCFA_CPS(cpast,new FlatBEnv(List()), KTime(List()), new MapStore(), new SortedSetD())
       M1CFA.k = 1
       M1CFA.m = 1
       M1CFA.flatPolicy = "m"
       M1CFA.runWithGlobalSharp(PriorityAssignment(opts.ordering))
       val mSharp = M1CFA.globalSharp
       val mStore = mSharp.asInstanceOf[StoreSharp].store
       val mFlows = Store.condense (mStore)
       
       Store.crossAnalyze(pFlows,mFlows)
     }
    }


    ()
  }

}
