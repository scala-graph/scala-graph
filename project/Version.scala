object Version {
  val compiler_2_11 = "2.11.12"
  val compiler_2_12 = "2.12.8"

  private val isSnapshot = true
  private def snapshot   = if (isSnapshot) "-SNAPSHOT" else ""
  private val major      = "1.13"
  val highest            = s"$major.1$snapshot"
  val core               = s"$major.1$snapshot"
  val constrained        = s"$major.1$snapshot"
  val dot                = s"$major.1$snapshot"
  val json               = s"$major.1$snapshot"
  val misc               = s"$major.1$snapshot"
}
