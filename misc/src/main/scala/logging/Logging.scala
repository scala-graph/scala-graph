package logging

import scala.language.implicitConversions

import org.slf4j.LoggerFactory
import org.slf4j.Logger

trait Logging {
  lazy val log = LoggerFactory.getLogger(getClass)
  
  def trace(msg: => String) =                   { if(log.isTraceEnabled) log.trace(msg) }
  def trace(msg: => String, e: Throwable) =     { if(log.isTraceEnabled) log.trace(msg, e) }
  def trace(msg: => String, o: Any, os: Any*) = { if(log.isTraceEnabled) log.trace(msg, o, os) }
  
  def debug(msg: => String) = 					{ if(log.isDebugEnabled) log.debug(msg) }
  def debug(msg: => String, e: Throwable) = 	{ if(log.isDebugEnabled) log.debug(msg, e) }
  def debug(msg: => String, o: Any, os: Any*) = { if(log.isDebugEnabled) log.debug(msg, o, os) }
                                                                         
  def info(msg: => String) = 					{ if(log.isInfoEnabled)  log.info(msg) }
  def info(msg: => String, e: Throwable) = 		{ if(log.isInfoEnabled)  log.info(msg, e) }
  def info(msg: => String, o: Any, os: Any*) = 	{ if(log.isInfoEnabled)  log.info(msg, o, os) }
                                                                         
  def warn(msg: => String) = 					{ if(log.isWarnEnabled)  log.warn(msg) }
  def warn(msg: => String, e: Throwable) = 		{ if(log.isWarnEnabled)  log.warn(msg, e) }
  def warn(msg: => String, o: Any, os: Any*) = 	{ if(log.isWarnEnabled)  log.warn(msg, o, os) }
                                                                         
  def error(msg: => String) = 					{ if(log.isErrorEnabled) log.error(msg) }
  def error(msg: => String, e: Throwable) = 	{ if(log.isErrorEnabled) log.error(msg, e) }
  def error(msg: => String, o: Any, os: Any*) = { if(log.isErrorEnabled) log.error(msg, o, os) }
}

object Logging {
	implicit def logging2Logger(anything: Logging): Logger = anything.log
}