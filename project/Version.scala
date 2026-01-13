object Version {
  val compiler_2_13 = "2.13.18"
  val compiler_3    = "3.7.4"

  private val isSnapshot = false
  private def snapshot   = if (isSnapshot) "-SNAPSHOT" else ""

  private val major               = 2
  private val minor               = 0
  private def version(patch: Int) = s"$major.$minor.$patch$snapshot"

  val highest  = version(3)
  val core     = version(3)
  val dot      = version(0)
  val jsonLift = version(3)
  val jsoniter = version(3)
}
