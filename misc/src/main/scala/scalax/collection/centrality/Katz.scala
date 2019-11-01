package scalax.collection.centrality

import math.{ceil, min}

import scalax.collection.GraphPredef._
import scalax.collection.Graph

/** Calculation of node centrality based on Katz centrality.
 */
object Katz {
     
  implicit class Centrality[N, E[X] <: EdgeLikeIn[X]](val g: Graph[N, E]) {
  
    /** Calculates the centrality of each node contained in `nodes`.
     */
    def centralities[G <: Graph[N, E] with Singleton]
                    (nodes: G#NodeSetT = g.nodes.asInstanceOf[G#NodeSetT],
                     maxDepth: Int = 0,
                     attenuationFactor: Float = .5f)
        : Map[G#NodeT, Float] = {
      
      assert(nodes.headOption map (_.containingGraph eq g) getOrElse true)

      val degrees: Map[G#NodeT, Int] = nodes.map((n: G#NodeT) => (n, n.degree)).toMap
          
      object Factor {
        private val factors = {
          var i = 0
          var lastFactor = 1f
          Array.fill(g.order) {
            val thisFactor = lastFactor 
            lastFactor *= attenuationFactor
            thisFactor
          }
        }
        def apply(index: Int) = factors(index)
      }

      val weightBuilder = Map.newBuilder[G#NodeT, Float]
      nodes.asInstanceOf[g.NodeSetT] foreach { n =>
        import g.ExtendedNodeVisitor

        var weight = 0f
        n.innerNodeTraverser.withMaxDepth(maxDepth) foreach {
          ExtendedNodeVisitor((node, count, depth, informer) => {
            weight += degrees(node.asInstanceOf[G#NodeT]) * Factor(depth)
          }) 
        }
        
        weightBuilder += ((n.asInstanceOf[G#NodeT], weight))
      }
      weightBuilder.result
    }
  }

  def centralityMapOrdering[N, E[X] <: EdgeLikeIn[X], G <: Graph[N, E] with Singleton]
     (centralities: Map[G#NodeT, Float]): Ordering[(G#NodeT, Float)] =
    new Ordering[(G#NodeT, Float)] {
      def compare(x: (G#NodeT, Float), y: (G#NodeT, Float)) = x._2 compare y._2
    }

  type ProjectionNodeCentrality[N, E[X] <: EdgeLikeIn[X]] = (Graph[N,E]#NodeT, Float)

  def centralityProjectionMapOrdering[N, E[X] <: EdgeLikeIn[X]]
     (centralities: Map[_ <: Graph[N,E]#NodeT, Float]): Ordering[ProjectionNodeCentrality[N,E]] =
    new Ordering[ProjectionNodeCentrality[N,E]] {
      def compare(x: ProjectionNodeCentrality[N,E], y: ProjectionNodeCentrality[N,E]) =
        x._2  compare y._2
    }
}