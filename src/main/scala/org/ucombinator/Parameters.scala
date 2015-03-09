package org.ucombinator

import scala.collection.immutable.{SortedMap, TreeMap}

class Parameters(val keywords : SortedMap[SKeyword,D], val positionals : List[D]) {

  def this () = this(TreeMap(), List())

  def apply(keyword : SKeyword) : D = keywords(keyword)

  def apply(position : Int) : D = positionals(position)

  def update(keyword : SKeyword, d : D) : Parameters = 
    new Parameters(keywords + (keyword -> d), positionals)

  def :: (d : D) : Parameters =
    new Parameters(keywords, d :: positionals)
}
