import sbt._
import Keys._

/* Adapted version of Paul's suggestion on org.improving */
trait Sonatype {
  def projectUrl: String
  def licenseDistribution = "repo"
  def licenseName: String
  def licenseUrl: String
  def developerId: String
  def developerName: String
  def developerUrl: String
  def scmUrl        = "https://github.com/scala-graph/scala-graph"
  def scmConnection = "scm:git:git@github.com:scala-graph/scala-graph.git"

  protected def isSnapshot(s: String) = s.trim endsWith "SNAPSHOT"
  protected val nexus                 = "https://oss.sonatype.org/"
  protected val ossSnapshots          = "Sonatype OSS Snapshots" at nexus + "content/repositories/snapshots/"
  protected val ossStaging            = "Sonatype OSS Staging" at nexus + "service/local/staging/deploy/maven2/"

  protected def generatePomExtra(scalaVersion: String): xml.NodeSeq =
    <url>{ projectUrl }
    </url>
    <licenses><license>
        <name>{ licenseName }</name>
        <url>{ licenseUrl }</url>
        <distribution>{ licenseDistribution }</distribution>
    </license></licenses>
    <scm>
      <url>{ scmUrl }</url>
      <connection>{ scmConnection }</connection>
    </scm>
    <developers><developer>
      <id>{ developerId }</id>
      <name>{ developerName }</name>
      <url>{ developerUrl }</url>
    </developer></developers>

  def settings: Seq[Setting[_]] = Seq(
    publishMavenStyle := true,
    publishTo := version((v: String) => Some(if (isSnapshot(v)) ossSnapshots else ossStaging)).value,
    publishArtifact in Test := false,
    pomIncludeRepository := (_ => false),
    pomExtra := (scalaVersion)(generatePomExtra).value
  )
}
