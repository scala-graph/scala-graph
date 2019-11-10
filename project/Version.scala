object Version {
  val compiler_2_12 = "2.12.10"
  val compiler_2_13 = "2.13.1"

  private val isSnapshot = false
  private def snapshot   = if (isSnapshot) "-SNAPSHOT" else ""
  private val major      = "1.13"
  val highest            = s"$major.1$snapshot"
  val core               = s"$major.1$snapshot"
  val constrained        = s"$major.0$snapshot"
  val dot                = s"1.12.1$snapshot"
  val json               = s"1.12.1$snapshot"
  val misc               = s"$major.0$snapshot"
}
