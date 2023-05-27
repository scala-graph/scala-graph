package scalax.collection.io.json

import net.liftweb.json.JsonAST.JValue
import net.liftweb.json.MappingException

/** Lift-JSON `Serializers` that convert from/to unnamed JSON values and edge data containers
  * (parameters) covering all predefined edge types. They allow to save space as default
  * Lift-JSON serializers would require the JSON text to contain JFields for every node and
  * edge.
  */
package object serializer {
  def couldNotConvertException(j: JValue, clazz: Class[_]) =
    new MappingException("Could not convert " + j + " to " + clazz.getName)
}
