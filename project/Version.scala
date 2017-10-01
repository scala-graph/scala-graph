object Version {
  val compiler_2_11 = "2.11.11"
  val compiler_2_12 = "2.12.3"

  private val isSnapshot = false
  private def snapshot = if (isSnapshot) "-SNAPSHOT" else ""
  private val major = "1.12"
  val all         = s"$major.1$snapshot"
  val core        = s"$major.1$snapshot"
  val constrained = s"$major.1$snapshot"
  val dot         = s"$major.1$snapshot"
  val json        = s"$major.1$snapshot"
  val misc        = s"$major.1$snapshot"
}