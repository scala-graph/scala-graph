package scalax.collection.connectivity

import scala.collection.{mutable, Map}
import scala.language.{higherKinds, implicitConversions}

import logging.Logging
import scalax.collection.GraphPredef.EdgeLikeIn
import scalax.collection.Graph

/**
 * @author Vasco Figueira
 */
final class GraphTrees[N, E[X] <: EdgeLikeIn[X]](val g: Graph[N,E]) extends Logging {

  /*
   * Think what return type should be. May return a modified instance of the graph 
   * without the vertices, or a simple tree-like structure based on the predecessorOf
   * map. Highly dependent on what would be handy for other algorithms that use these
   * tree-generating ones.
   */
  def breadthFirstSearchTree(outerNode: N): Unit = {
    val source = g get outerNode
    val visited: mutable.Map[g.NodeT,Boolean] = mutable.Map(source -> false)
    var predecessorOf: Map[g.NodeT, g.NodeT] = Map()
    var distances: Map[g.NodeT, Int] = Map(source -> 0)

    def loop(frontier: mutable.Queue[g.NodeT]): Unit = {
      if (!frontier.isEmpty) {
	      log.debug("loop: frontier is " + frontier)
	      val pred = frontier.dequeue()
	      pred.diSuccessors.foreach { s =>
	        visited.get(s) match {
	        case None => {
	            log.debug("node " + s + "... visited.")
	            visited += (s -> true)
	            distances += (s -> (distances(pred) + 1))
	            predecessorOf += (s -> pred)
	            frontier.enqueue(s)
	          }
	        case _ => {log.debug("node " + s + "... skipped.")}
	        }
	      }
	      log.debug(pred + " is now black")
	      visited(pred) = true
	      loop(frontier)
      }
    }
    loop(mutable.Queue(source))

    log.debug(distances.mkString("; "))
    log.debug(predecessorOf.mkString("; "))
  }

  def depthFirstSearchTree(): Unit = {
    val visited: mutable.Map[g.NodeT,Boolean] = mutable.Map()
    val predecessorOf: mutable.Map[g.NodeT, g.NodeT] = mutable.Map()
    
    // discovery and finish times for each node
    var time = 0;
    val discovery: mutable.Map[g.NodeT,Int] = mutable.Map()
    val finish: mutable.Map[g.NodeT,Int] = mutable.Map()
    
    def dfsVisit(n: g.NodeT): Unit = {
      log.debug("dfsVisit: visiting " + n)
      visited(n) = false
      time += 1
      discovery(n) = time
      for(s <- n.diSuccessors if visited.get(s) eq None) {
        predecessorOf(s) = n
        dfsVisit(s)
      }
      visited(n) = true
      time += 1
      finish(n) = time
      log.debug("done with " + n)
    }
    
    for (n <- g.nodes if (visited.get(n) == None)) dfsVisit(n)

    log.debug(visited.mkString("; "))
    val times = for((n,d) <- discovery) yield (n, d, finish(n))
    log.debug(times.mkString("; "))
    log.debug(predecessorOf.mkString("; "))
  }
  
}

object GraphTrees {

  implicit def graphToTrees[N, E[X] <: EdgeLikeIn[X]](g: Graph[N,E]) =
    new GraphTrees[N,E](g)
}
