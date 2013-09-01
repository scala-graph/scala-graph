package scalax.collection.centrality

//import scala.collection.mutable.
import scala.math.{ceil, min}

import scalax.collection.GraphPredef.{any2EdgeAssoc, EdgeLikeIn}
import scalax.collection.Graph

/** Calculation of node centrality based on Katz centrality.
 */
object Katz {
  implicit class Centrality[N, E[X] <: EdgeLikeIn[X]](val g: Graph[N, E]) {
  
    // Minimum 0.5, increasing with higher order
    private def defaultAttenuation(order: Int): Float =
      1f - (1f / (ceil(order.toFloat / 10).toFloat + 1))

    /** Calculates the centrality of each node contained in `nodes`.
     */
    def centralities(nodes: g.NodeSet = g.nodes,
                     maxDepth: Int = 0)
                    (implicit attenuationFactor: Int => Float = defaultAttenuation)
        : Map[g.NodeT, Float] = {
     
      val factor: Float = attenuationFactor(nodes.size) 
      val degrees: Map[g.NodeT, Int] = {
        val b = Map.newBuilder[g.NodeT, Int]
        nodes foreach (n => b += ((n, n.degree)))
        b.result
      }
      object Factor {
        private val limit = min(g.order, 5000)
        private val factors = {
          var i = 0
          var lastFactor = 1f
          Array.fill(limit) {
            val thisFactor = lastFactor 
            lastFactor *= factor
            thisFactor
          }
        }
        private val minFactor = factors(limit - 1)
        def apply(index: Int) =
          if (index < limit) factors(index) else minFactor
      }

      val weightBuilder = Map.newBuilder[g.NodeT, Float]
      nodes foreach { n =>
        import scalax.collection.GraphTraversal.VisitorReturn._
        import scalax.collection.GraphTraversalImpl._
        import g.ExtendedNodeVisitor

        var weight = 0f
        def visitor = ExtendedNodeVisitor((node, count, depth, informer) =>
          if (depth > maxDepth) Cancel
          else {
            weight += degrees(node) * Factor(depth)
            Continue
          }
        )
        n.traverse()(nodeVisitor = visitor)
        weightBuilder += ((n, weight))
      }
      weightBuilder.result
    }
  }
  
  implicit class CentralityOrdering[N, E[X] <: EdgeLikeIn[X]]
      (val centralities: Map[_ <: Graph[N,E]#NodeT, Float]) {
    
    type NodeCentrality[N, E[X] <: EdgeLikeIn[X]] = (Graph[N,E]#NodeT, Float)
    
    def ordering = new scala.Ordering[NodeCentrality[N,E]] {
      def compare(x: NodeCentrality[N,E], y: NodeCentrality[N,E]) =
        x._2  compare y._2
    }
  }
}