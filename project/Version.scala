object Version {
  val compiler = "2.11.7"
  val compiler_2 = "2.10.5"
  val compilerIsRC = false

  private val isSnapshot = true
  private def snapshot = if (isSnapshot) "-SNAPSHOT" else ""
  private val major = "1.10"
  val all         = s"$major.0$snapshot"
  val core        = s"$major.0$snapshot"
  val constrained = s"$major.0$snapshot"
  val dot         =   s"1.10.1$snapshot"
  val json        = s"$major.0$snapshot"
  val misc        = s"$major.0$snapshot"
}