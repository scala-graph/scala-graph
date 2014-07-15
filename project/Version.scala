object Version {
  val compiler = "2.11.1"
  val compiler_2 = "2.10.3"
  val compilerIsRC = false

  private val isSnapshot = false
  private def snapshot = if (isSnapshot) "-SNAPSHOT" else ""
  private val major = "1.9"
  val all         = major + ".0" + snapshot
  val core        = major + ".0" + snapshot
  val constrained = major + ".0" + snapshot
  val dot         = major + ".0" + snapshot
  val json        = major + ".2" + snapshot
  val test        = major + ".0" + snapshot
  val misc        = major + ".0" + snapshot
}