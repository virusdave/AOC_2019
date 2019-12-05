package codekata2019.day4

object Day4 {
  // Convert the input into a sequence of signed integers by splitting on newlines, trimming whitespace,
  // and removing empty entries.
  val inputRange = in.split("-").map(_.toInt)

  object Part1 {
    val solution =
      (inputRange(0) to inputRange(1)).map(_.toString.toList)
        .count { x =>
          // has at least 2 consecutive identical digits
          x.sliding(2).exists(l => l(0) == l(1)) &&
          // All consecutive digits are nondecreasing.  Takes advantage of the fact the 'toString(digit)' is
          // a monotone (order preserving) map.
          x.sliding(2).forall(l => l(0) <= l(1))
        }

  }

  object Part2 {
    val solution =
      (inputRange(0) to inputRange(1)).map(_.toString)
        .filter { x =>
          // All consecutive digits are nondecreasing.  Takes advantage of the fact the 'toString(digit)' is
          // a monotone (order preserving) map.
          x.toList.sliding(2).forall(l => l(0) <= l(1))
        }
        .map { x =>
          // From each string, remove all triply-or-more repeated characters
          "(.)\\1{2,}".r.replaceAllIn(x, "")
        }
        .count { x =>
          // has at least 2 consecutive identical digits of the remaining digits
          x.length >= 2 &&
          x.toList.sliding(2).exists(l => l(0) == l(1))
        }

  }


  lazy val in: String = ""
}
