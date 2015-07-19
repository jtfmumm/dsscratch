package dsscratch.draw

import dsscratch.components.{Channel, TwoChannel, MultiChannel}

object DotGraph {
  def draw(chs: Seq[Channel]): String = {
    "digraph G {\n" +
      "  concentrate=true;\n" +
      chs.map({
        case TwoChannel(p0, p1, _) => "  " + p0 + " -> " + p1 + ";\n"
        case MultiChannel(ps, _) => ""
        case _ => ""
      }).mkString +
    "}"
  }
}
