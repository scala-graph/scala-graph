object Version {
  val scala_2_10 = true
  val compiler = if (scala_2_10) "2.10.2" else "2.11.0"
  val compilerIsRC = false

  private val isSnapshot = false
  private def snapshot = if (isSnapshot) "-SNAPSHOT" else ""
  private val major = "1.7"
  val all         = major + ".0" + snapshot
  val core        = major + ".0" + snapshot
  val constrained = major + ".0" + snapshot
  val dot         = major + ".0" + snapshot
  val json        = major + ".0" + snapshot
  val misc        = major + ".0" + snapshot
}