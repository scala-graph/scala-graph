object Version {
  val scala_2_9 = false
  val compiler = if (scala_2_9) "2.9.2" else "2.10.0"
  val compilerIsRC = false

  private val isSnapshot = false
  private def snapshot = if (isSnapshot) "-SNAPSHOT" else ""
  private val major = "1.6"
  val all         = major + ".0" + snapshot
  val core        = major + ".1" + snapshot
  val constrained = major + ".0" + snapshot
  val dot         = major + ".0" + snapshot
  val json        = major + ".0" + snapshot
  val misc        = major + ".0" + snapshot
}