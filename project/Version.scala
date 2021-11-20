object Version {
  val compiler_2_13 = "2.13.6"

  private val isSnapshot = false
  private def snapshot   = if (isSnapshot) "-SNAPSHOT" else ""

  private val major = 2
  private val minor = 0
  private def version(patch: Int) = s"$major.$minor.$patch$snapshot"

  val highest     = version(0)
  val core        = version(0)
  val constrained = version(0)
  val dot         = version(0)
  val json        = version(0)
}
