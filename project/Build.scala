import bintray.Plugin.bintraySettings
import bintray.Keys._
import sbt.Keys._
import sbt._
import sbtassembly.Plugin.AssemblyKeys._
import sbtassembly.Plugin._
import com.typesafe.sbt.SbtScalariform
import com.typesafe.sbt.SbtScalariform._

object BuildSettings {
  val Version = "0.4.0-SNAPSHOT"

  val buildSettings = Defaults.coreDefaultSettings ++ (
    if (Version.endsWith("-SNAPSHOT"))
      Seq(
        publishTo := Some("Artifactory Realm" at "http://oss.jfrog.org/artifactory/oss-snapshot-local"),
        credentials := Credentials(Path.userHome / ".bintray" / ".artifactory") :: Nil
      )
    else bintraySettings ++
      Seq(
        bintrayOrganization in bintray := Some("softwaremill"),
        repository in bintray := "softwaremill")
    ) ++ Seq(
    organization := "com.softwaremill.supler",
    version := Version,
    scalaVersion := "2.11.6",
    scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-language:existentials", "-language:higherKinds"),
    publishMavenStyle := true,
    publishArtifact in Test := false,
    pomExtra := <scm>
      <url>git@github.com:softwaremill/supler.git</url>
      <connection>scm:git:git@github.com:softwaremill/supler.git</connection>
    </scm>
      <developers>
        <developer>
          <id>szimano</id>
          <name>Tomasz Szymanski</name>
          <url>http://www.szimano.org</url>
        </developer>
        <developer>
          <id>adamw</id>
          <name>Adam Warski</name>
          <url>http://www.warski.org</url>
        </developer>
      </developers>,
    parallelExecution := false,
    homepage := Some(new java.net.URL("https://github.com/softwaremill/supler")),
    licenses := ("Apache-2.0", new java.net.URL("http://www.apache.org/licenses/LICENSE-2.0.txt")) :: Nil
  )

  lazy val formatSettings = SbtScalariform.scalariformSettings ++ Seq(
    ScalariformKeys.preferences in Compile := formattingPreferences,
    ScalariformKeys.preferences in Test := formattingPreferences)

  import scalariform.formatter.preferences._
  def formattingPreferences =
    FormattingPreferences()
      .setPreference(RewriteArrowSymbols, false)
      .setPreference(AlignParameters, true)
      .setPreference(AlignSingleLineCaseStatements, true)
      .setPreference(DoubleIndentClassDeclaration, true)
      .setPreference(IndentSpaces, 2)

}

object Dependencies {
  val scalaTest     = "org.scalatest"     %% "scalatest"      % "2.1.6"   % "test"
  val json4sNative  = "org.json4s"        %% "json4s-native"  % "3.2.10"
  val playJson      = "com.typesafe.play" %% "play-json"      % "2.3.8"
  val akka          = "com.typesafe.akka" %% "akka-actor"     % "2.3.4"
  val jodaTime      = "joda-time"          % "joda-time"      % "2.5"
  val jodaConvert   = "org.joda"           % "joda-convert"   % "1.7"

  val sprayVersion  = "1.3.1"
  val sprayCan      = "io.spray"          %% "spray-can"      % sprayVersion
  val sprayRouting  = "io.spray"          %% "spray-routing"  % sprayVersion
  val sprayHttpx    = "io.spray"          %% "spray-httpx"    % sprayVersion
}

object SuplerBuild extends Build {

  import BuildSettings._
  import Dependencies._

  lazy val root: Project = Project(
    "root",
    file("."),
    settings = buildSettings ++ Seq(publishArtifact := false)
  ) aggregate(supler, suplerjs, examples)

  lazy val makeVersionSh = taskKey[Seq[File]]("Creates .run.central.synchro.sh file.")

  lazy val supler: Project = Project(
    "supler",
    file("supler"),
    settings = buildSettings ++ Seq(
      libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-compiler" % _),
      libraryDependencies ++= Seq(json4sNative, scalaTest, playJson),
      makeVersionSh := {
        val pf = new java.io.File(".run.central.synchro.sh")
        val content = s"""|#!/bin/bash
                         |PROJECT_VERSION=${version.value} /bin/bash .central.synchro.sh
                      """.stripMargin
        IO.write(pf, content)
        Seq(pf)
      })
  )

  lazy val createAndCopySuplerJs = taskKey[Unit]("Create and copy the supler js files.")

  lazy val examples: Project = Project(
    "examples",
    file("examples"),
    settings = buildSettings ++ assemblySettings ++ Seq(
      libraryDependencies ++= Seq(akka, sprayCan, sprayRouting, sprayHttpx, jodaTime, jodaConvert),
      jarName in assembly := "supler-example.jar",
      mainClass in assembly := Some("org.demo.DemoServer"),
      createAndCopySuplerJs := {
        val suplerJsDir = baseDirectory.value / ".." / "supler-js"

        println("Running grunt")
        Process(List("grunt", "ts"), suplerJsDir.getCanonicalFile).!

        val suplerJsSource = suplerJsDir / "target" / "supler.js"
        val suplerJsTarget = (classDirectory in Compile).value / "supler.js"
        println(s"Copying supler.js to resources from $suplerJsSource to $suplerJsTarget")
        IO.copy(List((suplerJsSource, suplerJsTarget)))
      },
      assembly <<= assembly.dependsOn(createAndCopySuplerJs),
      publishArtifact := false)
  ).settings(formatSettings: _*) dependsOn (supler)

  private def haltOnCmdResultError(result: Int) {
    if (result != 0) throw new Exception("Build failed.")
  }

  val updateNpm = baseDirectory map { bd =>
    println("Updating NPM dependencies in " + bd)
    haltOnCmdResultError(Process("npm install", bd)!)
    println("NPM dependencies updated")
  }

  def gruntTask(taskName: String) = (baseDirectory, streams) map { (bd, s) =>
    val localGruntCommand = "./node_modules/.bin/grunt " + taskName
    def buildGrunt() = {
      Process(localGruntCommand, bd).!
    }
    println("Building with Grunt.js : " + taskName)
    haltOnCmdResultError(buildGrunt())
  } dependsOn updateNpm

  lazy val suplerjs: Project = Project(
    "supler-js",
    file("supler-js"),
    settings = buildSettings ++ Seq(
      test in Test <<= gruntTask("test") dependsOn (test in Test in supler))
  )
}
