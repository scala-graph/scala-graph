package scalax.collection.io.json.descriptor

/**
 * This package contains predefined edge descriptors for all basic/predefined edge types.   
 * They may be used when defining `json.descriptor.Descriptor`s for convenience. For instance,
 * {{{
 * UnDi.descriptor[String](Some(new EdgeSerializer))
 * }}}
 * is a shorthand for
 * {{{
 * new EdgeDescriptor[String,UnDiEdge,UnDiEdge.type](UnDiEdge, Some(new EdgeSerializer)) 
 * }}}
 * where predefined edge types named <edgeType>Edge map to predefined descriptors
 * named <edgeType>.
 * 
 * @author Peter Empen
 */
package object predefined {
}
