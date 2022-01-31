package scalax.collection.centrality

import scalax.collection.GraphPredef._
import scalax.collection.Graph

/** Calculation of node centrality based on Katz centrality.
  */
class Katz[N, E[+X] <: EdgeLikeIn[X], G[N, E[+X] <: EdgeLikeIn[X]] <: Graph[N, E]](val g: G[N, E]) {

    /** Calculates the centrality of each node contained in `nodes`.
      */
    def centralities(
        maxDepth: Int = 0,
        attenuationFactor: Float = .5f
    ): Map[g.NodeT, Float] = {
      val nodes = g.nodes
      val degrees: Map[g.NodeT, Int] = nodes.map((n: g.NodeT) => (n, n.degree)).toMap

      object Factor {
        private val factors = {
          var lastFactor = 1f
          Array.fill(g.order) {
            val thisFactor = lastFactor
            lastFactor *= attenuationFactor
            thisFactor
          }
        }
        def apply(index: Int): Float = factors(index)
      }

      val weightBuilder = Map.newBuilder[g.NodeT, Float]
      nodes foreach { n =>
        import g.ExtendedNodeVisitor
        var weight = 0f
        n.innerNodeTraverser.withMaxDepth(maxDepth) foreach {
          ExtendedNodeVisitor { (node, _, depth, _) =>
            weight += degrees(node) * Factor(depth)
          }
        }
        weightBuilder += n -> weight
      }
      weightBuilder.result
    }

    def mapOrdering: Ordering[(g.NodeT, Float)] =
      (x: (g.NodeT, Float), y: (g.NodeT, Float)) => x._2 compare y._2
  }
