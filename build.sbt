import Dependencies._
import Settings._

scalacOptions += "-Ypartial-unification"

lazy val day1 = (project in file("day1"))
  .settings(Settings.settings: _*)
  .settings(Settings.day1Settings: _*)
  .settings(libraryDependencies ++= day1Dependencies.toSeq)

lazy val day2 = (project in file("day2"))
  .settings(Settings.settings: _*)
  .settings(Settings.day2Settings: _*)
  .settings(libraryDependencies ++= day2Dependencies.toSeq)

lazy val day3 = (project in file("day3"))
  .settings(Settings.settings: _*)
  .settings(Settings.day3Settings: _*)
  .settings(libraryDependencies ++= day3Dependencies.toSeq)

lazy val day4 = (project in file("day4"))
  .settings(Settings.settings: _*)
  .settings(Settings.day4Settings: _*)
  .settings(libraryDependencies ++= day4Dependencies.toSeq)

lazy val all = (project in file("all"))
  .settings(Settings.settings: _*)
  .settings(Settings.allSettings: _*)
  .dependsOn(
    day1,
    day2,
    day3,
    day4,
  )
  .configs(Test)

lazy val root = (project in file("."))
  .aggregate(
    day1,
    day2,
    day3,
    day4,

    all,
  )
  .settings(libraryDependencies ++= rootDependencies.toSeq)
