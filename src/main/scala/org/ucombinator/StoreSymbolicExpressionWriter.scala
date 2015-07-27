package org.ucombinator

import java.io.{File, BufferedWriter, FileWriter}

class StoreSymbolicExpressionWriter(store: Store) {

  private def trimFileName(fileName: String): String = {
    val name = if (fileName == null || !fileName.contains(".") || fileName.startsWith(".")) {
      fileName
    }
    else {
      fileName.substring(0, fileName.indexOf("."))
    }

    if (name == null || !name.contains(File.separator) || name.endsWith(File.separator)) {
      name
    } else {
      name.substring(name.lastIndexOf(File.separator) + 1)
    }
  }

  def write(opts: CFAOptions): Unit = {
    val fileName = s"${opts.analysis}-${opts.k}-${trimFileName(opts.file)}.txt"
    write(fileName)
  }

  def write(fileName: String): Unit = {
    val parent = new File("stores")
    parent.mkdirs()
    val file = new File(parent, fileName)
    val writer = new BufferedWriter(new FileWriter(file))

    writer.write("(")
    writer.newLine()
    for ((a, d) <- store.toList) {
      writer.write(s"(${convert(a)} ${convert(d)})")
      writer.newLine()
    }
    writer.write(")")
    writer.newLine()

    writer.close()
  }

  def write(store: Store): Unit = {
  }

  def convert(a: Addr): String = a match {
    case MapBind(name, KTime(last)) => s"($name ${last.mkString(" ")})"
    case PrimAddr(name) => s"(prim $name)"
    case FieldAddr(ConsLocation(time), field) => s"(cons ${convert(time)} $field)"
  }

  def convert(d: D): String = d match {
    case SortedSetD(set) => s"(set ${set.toList.map(convert).mkString(" ")})"
  }

  def convert(value: Value): String = value match {
    case Clo(lam, bEnv) => s"(clo ${lam.label} ${convert(bEnv)})"
    case BooleanValue(_) => value.toString
    case PrimValue(name) => s"(prim $name)"
    case ConsLocation(time) => s"(cons ${convert(time)})"
  }

  def convert(bEnv: BEnv): String = bEnv match {
    case MapBEnv(map) =>
      val tmp = map.toList.map {
        case (name, addr) => s"($name ${convert(addr)})"
      }.mkString(" ")
      s"(env $tmp)"
  }

  def convert(time: Time): String = time match {
    case KTime(last) => last.mkString(" ")
  }

}
