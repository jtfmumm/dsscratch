package dsscratch.modules.test

trait TestCode {
  val name = "\\$.+\\$".r
  override def toString: String = name.findFirstMatchIn(this.getClass.getName).getOrElse("").toString
}

object TestCodes {
  object SUCCESS extends TestCode
  object FAILURE extends TestCode
  object NO_VAL extends TestCode
}
