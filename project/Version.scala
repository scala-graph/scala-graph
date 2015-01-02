object Version {
  val compiler = "2.11.4"
  val compiler_2 = "2.10.3"
  val compilerIsRC = false

  private val isSnapshot = false
  private def snapshot = if (isSnapshot) "-SNAPSHOT" else ""
  private val major = "1.9"
  val all         = s"$major.0$snapshot"
  val core        = s"$major.1$snapshot"
  val constrained = s"$major.0$snapshot"
  val dot         =   s"1.10.0$snapshot"
  val json        = s"$major.2$snapshot"
  val test        = s"$major.0$snapshot"
  val misc        = s"$major.0$snapshot"
}