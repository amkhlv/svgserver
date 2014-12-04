name := """svgserver"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.1"

libraryDependencies ++= Seq(
  jdbc,
  anorm,
  cache,
  ws
)

scalacOptions in (Compile, doc) <++= baseDirectory map { d =>
  Seq("-doc-root-content", d / "rootdoc.txt" getPath)
}

scalacOptions ++= Seq("-feature")

