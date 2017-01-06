object Version {
  val compiler_2_10 = "2.10.6"
  val compiler_2_11 = "2.11.8"
  val compiler_2_12 = "2.12.0"

  private val isSnapshot = false
  private def snapshot = if (isSnapshot) "-SNAPSHOT" else ""
  private val major = "1.11"
  val all         = s"$major.0$snapshot"
  val core        = s"$major.4$snapshot"
  val constrained = s"$major.0$snapshot"
  val dot         = s"$major.0$snapshot"
  val json        = s"$major.0$snapshot"
  val misc        = s"$major.0$snapshot"
}