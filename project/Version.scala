object Version {
  val scala_2_10 = true
  val compiler = if (scala_2_10) "2.10.3" else "2.11.0"
  val compilerIsRC = false

  private val isSnapshot = true
  private def snapshot = if (isSnapshot) "-SNAPSHOT" else ""
  private val major = "1.7"
  val all         = major + ".3" + snapshot
  val core        = major + ".3" + snapshot
  val constrained = major + ".3" + snapshot
  val dot         = major + ".3" + snapshot
  val json        = major + ".3" + snapshot
  val misc        = major + ".3" + snapshot
}