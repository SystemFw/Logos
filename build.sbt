lazy val root = (project in file(".")).
  settings(
    commonSettings,
    consoleSettings,
    compilerOptions,
    typeSystemEnhancements,
    dependencies,
    tests
  )

lazy val commonSettings = Seq(
  name := "stm",
  scalaVersion := "2.11.12",
  crossScalaVersions := Seq("2.11.12", "2.12.5")
)

val consoleSettings = Seq(
 initialCommands := s"import stm._",
  scalacOptions in (Compile, console) -= "-Ywarn-unused-import"
)

lazy val compilerOptions =
  scalacOptions ++= Seq(
    "-unchecked",
    "-deprecation",
    "-encoding",
    "utf8",
    "-target:jvm-1.8",
    "-feature",
    "-language:implicitConversions",
    "-language:higherKinds",
    "-language:existentials",
    "-Ypartial-unification",
    "-Ywarn-unused-import",
    "-Ywarn-value-discard"
  )

lazy val typeSystemEnhancements =
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")

def dep(org: String)(version: String)(modules: String*) =
    Seq(modules:_*) map { name =>
      org %% name % version
    }

lazy val dependencies = {
  val cats = dep("org.typelevel")("1.1.0")(
    "cats-core",
    "cats-macros",
    "cats-kernel",
    "cats-free"
  )

  val catsEffect =
    dep("org.typelevel")("1.0.0-RC2")("cats-effect")

  val deps =
    libraryDependencies ++= Seq(
      cats,
      catsEffect
    ).flatten

  Seq(deps)
}

lazy val tests = {
  val dependencies = {
    val specs2 = dep("org.specs2")("4.0.2")(
      "specs2-core",
      "specs2-scalacheck"
    )

    val mixed = Seq(
      "org.scalacheck" %% "scalacheck" % "1.13.4"
    )

    libraryDependencies ++= Seq(
      specs2,
      mixed
    ).flatten.map(_ % "test")
  }

  val frameworks =
    testFrameworks := Seq(TestFrameworks.Specs2)

  Seq(dependencies, frameworks)
}
