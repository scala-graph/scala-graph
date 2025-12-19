package scalax.util

import scala.annotation.elidable
import scala.annotation.elidable.ASSERTION

@elidable(ASSERTION)
transparent inline def assert(inline cond: Boolean): Unit =
  if !cond then throw new AssertionError

@elidable(ASSERTION)
transparent inline def assert(inline cond: Boolean, inline msg: => String): Unit =
  if !cond then throw new AssertionError(msg)
