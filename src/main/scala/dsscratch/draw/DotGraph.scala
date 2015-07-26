package dsscratch.draw

import dsscratch.components.{Channel, TwoChannel, MultiChannel}

object DotGraph {
  def drawChs(cs: Seq[Channel]): String = {
    "digraph G {\n" +
      "  concentrate=true;\n" +
      cs.map({
        case TwoChannel(p0, p1, _) => "  " + p0 + " -> " + p1 + ";\n"
        case MultiChannel(ps, _) => ""
        case _ => ""
      }).mkString +
    "}"
  }

  def drawStrings(ss: Seq[String]): String = {
    "digraph G {\n" +
      "  concentrate=true;\n" +
      ss.mkString +
    "}"
  }
}
