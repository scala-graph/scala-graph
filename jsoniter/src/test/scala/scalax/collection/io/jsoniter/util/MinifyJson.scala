package scalax.collection.io.jsoniter.util

extension (json: String)
  def minified: String =
    val sb       = new StringBuilder
    var inString = false
    var escape   = false
    for (c <- json)
      if inString then
        sb += c
        escape = c == '\\' && !escape
        if c == '"' && !escape then inString = false
      else
        c match
          case ' ' | '\n' | '\r' | '\t' => // skip
          case '"'                      =>
            sb += c
            inString = true
          case _ =>
            sb += c
    sb.toString()
