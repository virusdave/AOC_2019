import sbt._
import Keys._

object Dependencies {
  private object Sources {
    // My personal libraries
    val SlickScratchDb = ProjectRef(uri("https://github.com/virusdave/slick-scratch.git"), "db")
    val SlickScratchDbAll = ProjectRef(uri("https://github.com/virusdave/slick-scratch.git"), "db_all")
  }

  private object LibraryVersions {
    val Ammonite = "1.4.4"
    val Cats = "1.5.0-RC1"
    val Enumeratum = "1.5.13"
    val Postgres = "42.2.5"
    val Shapeless = "2.3.3"
    val Slick = "3.2.3"
    val Slickless = "0.3.3"
    val SlickPG = "0.16.3"

    // Test libs
    val ScalaCheck = "1.14.0"
    val ScalaTest = "3.0.5"
  }

  private object Libraries {
    val Ammonite = "com.lihaoyi" % "ammonite" % LibraryVersions.Ammonite % "test" cross CrossVersion.full
    val Breeze = "org.scalanlp" %% "breeze" % "1.0"
    val Cats = "org.typelevel" %% "cats-core" % LibraryVersions.Cats
    val Enumeratum = "com.beachape" %% "enumeratum" % LibraryVersions.Enumeratum
    val EnumeratumPlayJson = "com.beachape" %% "enumeratum-play-json" % LibraryVersions.Enumeratum
    val Postgres = "org.postgresql" % "postgresql" % LibraryVersions.Postgres
    val Shapeless = "com.chuusai" %% "shapeless" % LibraryVersions.Shapeless
    val Slick = "com.typesafe.slick" %% "slick" % LibraryVersions.Slick
    val Slickless = "io.underscore" %% "slickless" % LibraryVersions.Slickless
    val SlickPg = "com.github.tminglei" %% "slick-pg" % LibraryVersions.SlickPG
    val SlickPgPlayJson = "com.github.tminglei" %% "slick-pg_play-json" % LibraryVersions.SlickPG

    // Test libs
    val ScalaTest  = "org.scalatest" %% "scalatest" % LibraryVersions.ScalaTest % Test
    val ScalaCheck = "org.scalacheck" %% "scalacheck" % LibraryVersions.ScalaCheck % Test
  }

  private object Packages {
    val AmmoniteAll = Set[ModuleID](
      Libraries.Ammonite,
      "com.lihaoyi" %% "ammonite-ops" % LibraryVersions.Ammonite,
    )

    val Enumeratum = Set[ModuleID](
      Libraries.Enumeratum,
      Libraries.EnumeratumPlayJson
    )

    val Slick = Set[ModuleID](
      Libraries.Slick,
      "com.typesafe.slick" %% "slick-codegen" % LibraryVersions.Slick,
      "com.typesafe.slick" %% "slick-hikaricp" % LibraryVersions.Slick,
    )

    val SlickPgAll = Set[ModuleID](
      Libraries.Postgres,
      Libraries.SlickPg,
      Libraries.SlickPgPlayJson,
    )
  }

  val commonDependencies: Set[ModuleID] = Set(
    Libraries.Cats,
    //Libraries.Postgres,
    //Libraries.Shapeless,
    //Libraries.Slickless,

    //Libraries.ScalaCheck,
    //Libraries.ScalaTest,
  ) ++ Set(
    Packages.AmmoniteAll,
    Packages.Enumeratum,
    Packages.Slick,
    Packages.SlickPgAll,
  ).flatten

  val day1Dependencies: Set[ModuleID] =
    commonDependencies ++
      Set(
      )

  val day2Dependencies: Set[ModuleID] =
    commonDependencies ++
      Set(
      )

  val day3Dependencies: Set[ModuleID] =
    commonDependencies ++
      Set(
        Libraries.Breeze,
      )

  val allDependencies: Set[ModuleID] =
    commonDependencies ++
      Set(
      )

  val rootDependencies: Set[ModuleID] =
    commonDependencies ++
      Set(
      )
}
