object Version {
  val compiler_2_11 = "2.11.11"
  val compiler_2_12 = "2.12.2"

  private val isSnapshot = false
  private def snapshot = if (isSnapshot) "-SNAPSHOT" else ""
  private val major = "1.12"
  val all         = s"$major.0$snapshot"
  val core        = s"$major.0$snapshot"
  val constrained = s"$major.0$snapshot"
  val dot         = s"$major.5$snapshot"
  val json        = s"$major.0$snapshot"
  val misc        = s"$major.0$snapshot"
}