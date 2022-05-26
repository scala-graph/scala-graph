object Version {
  val compiler_2_12 = "2.12.15"
  val compiler_2_13 = "2.13.8"

  private val isSnapshot = false
  private def snapshot   = if (isSnapshot) "-SNAPSHOT" else ""
  private val major      = "1.13"
  val highest            = s"$major.5$snapshot"
  val core               = s"$major.5$snapshot"
  val constrained        = s"$major.2$snapshot"
  val dot                = s"$major.0$snapshot"
  val json               = s"$major.0$snapshot"
}
