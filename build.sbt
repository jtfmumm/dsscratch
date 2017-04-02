lazy val commonSettings = Seq(
  organization := "com.jtfmumm",
  version := "0.2.0",
  scalaVersion := "2.12.1"
)


lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "Distributed Systems Scratchpad"
  )
