import Dependencies._

scalacOptions += "-Ypartial-unification"

lazy val common = (project in file("common"))
  .settings(Settings.settings: _*)
  .settings(Settings.day1Settings: _*)
  .settings(libraryDependencies ++= commonDependencies.toSeq)

lazy val day1 = (project in file("day1"))
  .settings(Settings.settings: _*)
  .settings(Settings.day1Settings: _*)
  .settings(libraryDependencies ++= day1Dependencies.toSeq)
  .dependsOn(common)

lazy val day2 = (project in file("day2"))
  .settings(Settings.settings: _*)
  .settings(Settings.day2Settings: _*)
  .settings(libraryDependencies ++= day2Dependencies.toSeq)
  .dependsOn(common)

lazy val day3 = (project in file("day3"))
  .settings(Settings.settings: _*)
  .settings(Settings.day3Settings: _*)
  .settings(libraryDependencies ++= day3Dependencies.toSeq)
  .dependsOn(common)

lazy val day4 = (project in file("day4"))
  .settings(Settings.settings: _*)
  .settings(Settings.day4Settings: _*)
  .settings(libraryDependencies ++= day4Dependencies.toSeq)
  .dependsOn(common)

lazy val day5 = (project in file("day5"))
  .settings(Settings.settings: _*)
  .settings(Settings.day5Settings: _*)
  .settings(libraryDependencies ++= day5Dependencies.toSeq)
  .dependsOn(common)

lazy val day6 = (project in file("day6"))
  .settings(Settings.settings: _*)
  .settings(Settings.day6Settings: _*)
  .settings(libraryDependencies ++= day6Dependencies.toSeq)
  .dependsOn(common)

lazy val day7 = (project in file("day7"))
  .settings(Settings.settings: _*)
  .settings(Settings.day7Settings: _*)
  .settings(libraryDependencies ++= day7Dependencies.toSeq)
  .dependsOn(common)

lazy val day8 = (project in file("day8"))
  .settings(Settings.settings: _*)
  .settings(Settings.day8Settings: _*)
  .settings(libraryDependencies ++= day8Dependencies.toSeq)
  .dependsOn(common)

lazy val day9 = (project in file("day9"))
  .settings(Settings.settings: _*)
  .settings(Settings.day9Settings: _*)
  .settings(libraryDependencies ++= day9Dependencies.toSeq)
  .dependsOn(common)

lazy val dayN = (project in file("dayN"))
  .settings(Settings.settings: _*)
  .settings(Settings.dayNSettings: _*)
  .settings(libraryDependencies ++= dayNDependencies.toSeq)
  .dependsOn(common)

lazy val all = (project in file("all"))
  .settings(Settings.settings: _*)
  .settings(Settings.allSettings: _*)
  .dependsOn(
    day1,
    day2,
    day3,
    day4,
    day5,
    day6,
    day7,
    day8,
    day9,
    dayN,
  )
  .configs(Test)

lazy val root = (project in file("."))
  .aggregate(
    day1,
    day2,
    day3,
    day4,
    day5,
    day6,
    day7,
    day8,
    day9,
    dayN,

    all,
  )
  .settings(Settings.settings: _*)
  .settings(Settings.allSettings: _*)
  .settings(libraryDependencies ++= rootDependencies.toSeq)
