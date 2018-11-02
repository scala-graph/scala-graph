package custom.flight

import language.implicitConversions

/** This object encapsulates the node and other helper types needed for
  * the flight route example. The nodes of such a graph will be `Airport`s,
  * the edges `Flight`s using `Datetime` and `Duration`.
  */
/* ------------------------------------------------- node type */
case class Airport(val code: String) {
  override def toString = code
}

/* ----------------------------------------- edge type helpers */
abstract class HHMM(hour: Int, min: Int) {
  final val MAX_MIN = 60
  assert(hour >= 0 && hour < 100 && min >= 0 && min < MAX_MIN)
  protected def fmt(i: Int) = "%02d" format i
  def toInt                 = hour * MAX_MIN + min
  override def toString     = fmt(hour) + ":" + fmt(min)
}

/** Represents a time of day as "hh:mm" with 0 <= hh < 24 and 0 <= mm < 60. */
case class DayTime(val oClock: Int, val min: Int) extends HHMM(oClock, min) {
  assert(oClock < 24)
}
final class DayTimeAssoc(oClock: Int) {
  def o(min: Int) = new DayTime(oClock, min)
}

/** Represents a duration "hh:mm" with 0 <= hh < 100 and 0 <= mm < 100. */
case class Duration(val hour: Int, val min: Int) extends HHMM(hour, min) {
  override def toString = fmt(hour) + "h" + fmt(min) + "'"
}
final class DurationAssoc(hours: Int) {
  def h(mins: Int) = new Duration(hours, mins)
}

object Helper {
  implicit def intToDuration(hours: Int) = new DurationAssoc(hours)
  implicit def intToDayTime(oClock: Int) = new DayTimeAssoc(oClock)
}
