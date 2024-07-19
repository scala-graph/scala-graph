object Version {
  val compiler_2_13 = "2.13.14"
  val compiler_3    = "3.3.0"

  private val isSnapshot = false
  private def snapshot   = if (isSnapshot) "-SNAPSHOT" else ""

  private val major               = 2
  private val minor               = 0
  private def version(patch: Int) = s"$major.$minor.$patch$snapshot"

  val highest     = version(1)
  val core        = version(1)
  val gen         = version(1)
  val constrained = version(0)
  val dot         = version(0)
  val json        = version(0)
}
