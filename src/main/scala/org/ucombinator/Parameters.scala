package org.ucombinator

import scala.collection.immutable.{SortedMap, TreeMap}

class Parameters[A](val keywords : SortedMap[SKeyword,A], val positionals : List[A]) {

  def this () = this(TreeMap(), List())

  def apply(keyword : SKeyword) : A = keywords(keyword)

  def apply(position : Int) : A = positionals(position)

  def update(keyword : SKeyword, a : A) : Parameters[A] =
    new Parameters(keywords + (keyword -> a), positionals)

  def :: (a : A) : Parameters[A] =
    new Parameters(keywords, a :: positionals)
}
