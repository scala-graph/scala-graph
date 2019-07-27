object Version {
  val compiler_2_11 = "2.11.12"
  val compiler_2_12 = "2.12.8"

  private val isSnapshot = false
  private def snapshot   = if (isSnapshot) "-SNAPSHOT" else ""
  private val major      = "1.13"
  val highest            = s"$major.0$snapshot"
  val core               = s"$major.0$snapshot"
  val constrained        = s"$major.0$snapshot"
  val dot                = s"$major.0$snapshot"
  val json               = s"$major.0$snapshot"
  val misc               = s"$major.0$snapshot"
}
