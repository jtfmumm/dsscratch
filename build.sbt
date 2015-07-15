lazy val randomific = "com.jtfmumm" %% "randomific" % "0.1.1"

lazy val commonSettings = Seq(
  organization := "com.jtfmumm",
  version := "0.1.0"
  // scalaVersion := "2.11.4"
)


lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "Distributed Systems Scratchpad",
    libraryDependencies += randomific
  )