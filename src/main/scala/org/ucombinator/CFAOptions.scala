package org.ucombinator

class CFAOptions {
  var file : String = null ;
  var k = 1
  var m = 1
  var printStates = false ;
  var flatPolicy = "m"
  var analysis = "flat"
  var ordering = "bfs"
}

object CFAOptions {

  def parse(args : List[String], opts : CFAOptions) : Unit = args match {
    case List() => {}
    case "--k" :: k :: rest => { 
      opts.k = Integer.parseInt(k)
      parse(rest,opts)
    }

    case "--m" :: m :: rest => { 
      opts.m = Integer.parseInt(m)
      parse(rest,opts)
    }

    case "--flat-policy" :: s :: rest => { 
      opts.flatPolicy = s
      parse(rest,opts)
    }

    case "--analysis" :: a :: rest => { 
      opts.analysis = a
      parse(rest,opts)
    }

    case "--ordering" :: o :: rest => {
      opts.ordering = o
      parse(rest,opts)
    }

    case "--print-states" :: "true" :: rest => {
      opts.printStates = true
      parse(rest,opts)
    }

    case "--print-states" :: "false" :: rest => {
      opts.printStates = false
      parse(rest,opts)
    }

    case fileName :: rest => {
      opts.file = fileName ;
      parse(rest,opts)
    }

  }

  def parse(args : Array[String]) : CFAOptions = {
    val opts = new CFAOptions
    parse(args.toList,opts)
    opts
  }

}
