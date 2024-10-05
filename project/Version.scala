object Version {
  val compiler_2_13 = "2.13.15"
  val compiler_3    = "3.5.1"

  val compiler_3_fallback = "3.3.0"

  private val isSnapshot = false
  private def snapshot   = if (isSnapshot) "-SNAPSHOT" else ""

  private val major               = 2
  private val minor               = 0
  private def version(patch: Int) = s"$major.$minor.$patch$snapshot"

  val highest = version(2)
  val core    = version(2)
  val dot     = version(0)
  val json    = version(0)
}
