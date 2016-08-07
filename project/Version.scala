object Version {
  val compiler = "2.11.7"
  val compiler_2 = "2.10.5"

  private val isSnapshot = false
  private def snapshot = if (isSnapshot) "-SNAPSHOT" else ""
  private val major = "1.11"
  val all         = s"$major.0$snapshot"
  val core        = s"$major.2$snapshot"
  val constrained = s"$major.0$snapshot"
  val dot         = s"$major.0$snapshot"
  val json        = s"$major.0$snapshot"
  val misc        = s"$major.0$snapshot"
}