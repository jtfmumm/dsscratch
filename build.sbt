name := "fitzroy"

lazy val randomific = "com.jtfmumm" %% "randomific" % "0.1.1"

lazy val commonSettings = Seq(
  organization := "com.jtfmumm",
  version := "0.2.0"
)

lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "fitzroy",
    libraryDependencies += randomific,
    publishMavenStyle := true,
    publishTo := {
      val nexus = "https://oss.sonatype.org/"
      if (isSnapshot.value)
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases"  at nexus + "service/local/staging/deploy/maven2")
    },
    publishArtifact in Test := false,
        pomExtra := (
      <url>http://github.com/jtfmumm/fitzroy</url>
        <licenses>
          <license>
            <name>MIT</name>
            <url>http://opensource.org/licenses/GPL-3.0</url>
            <distribution>repo</distribution>
          </license>
        </licenses>
        <scm>
          <url>git@github.com:jtfmumm/fitzroy.git</url>
          <connection>scm:git:git@github.com:jtfmumm/fitzroy.git</connection>
        </scm>
        <developers>
          <developer>
            <id>jtfmumm</id>
            <name>John Mumm</name>
            <url>http://jtfmumm.com</url>
          </developer>
        </developers>)
  )
