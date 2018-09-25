package scalax.collection

import java.awt.Color
import java.io.File
import java.nio.file.{Path, Paths, Files}

import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.util.Try

import org.gephi.layout.plugin.forceAtlas2.ForceAtlas2
import org.gephi.preview.types.DependantColor
import org.gephi.filters.api.FilterController
import org.gephi.filters.api.Range
import org.gephi.filters.plugin.graph.DegreeRangeBuilder.DegreeRangeFilter
import org.gephi.graph.api.GraphController
import org.gephi.graph.api.GraphModel
import org.gephi.io.importer.api._
import org.gephi.io.exporter.api.ExportController
import org.gephi.io.processor.plugin.DefaultProcessor
import org.gephi.preview.api.PreviewController
import org.gephi.preview.api.PreviewModel
import org.gephi.preview.api.PreviewProperty
import org.gephi.preview.types.EdgeColor
import org.gephi.project.api.ProjectController
import org.gephi.project.api.Workspace
import org.openide.util.Lookup

import scalax.collection.GraphPredef.EdgeLikeIn

trait Drawable {

  private def assertedLookup[T <: AnyRef : ClassTag](clazz: Class[T]): T = {
    val l: T = Lookup.getDefault.lookup(clazz)
    assert(l ne null, "Lookup for class " + clazz.getName + " failed")
    l
  }

  /** Draw graph image and write it to the given path and file name.
    *
    * @param g    the graph to output
    * @param path folder the image file is to be written to
    * @param name file name including an extension
    * @tparam N   type of node
    * @tparam E   type of edge
    */
  def makeImage[N, E[X] <: EdgeLikeIn[X]](g: Graph[N, E], path: String, name: String): Try[File] = {

    //Init a project - and therefore a workspace
    val pc: ProjectController = assertedLookup(classOf[ProjectController])
    pc.newProject()
    val workspace: Workspace = pc.getCurrentWorkspace

    //Get models and controllers for this new workspace
    val graphModel: GraphModel = assertedLookup(classOf[GraphController]).getGraphModel
    val model: PreviewModel = assertedLookup(classOf[PreviewController]).getModel
    val importController: ImportController = assertedLookup(classOf[ImportController])
    val filterController: FilterController = assertedLookup(classOf[FilterController])

    toContainer(g).map(container => {
      //Append imported data to GraphAPI
      importController.process(container, new DefaultProcessor, workspace)

      //See if graph is well imported
      val graph = graphModel.getDirectedGraph
      //    println("Nodes: " + graph.getNodeCount)
      //    println("Edges: " + graph.getEdgeCount)

      //Filter
      val degreeFilter = new DegreeRangeFilter
      degreeFilter.init(graph)
      degreeFilter.setRange(new Range(1, Integer.MAX_VALUE)) //Remove nodes with degree < 1

      val query = filterController.createQuery(degreeFilter)
      val view = filterController.filter(query)
      graphModel.setVisibleView(view) //Set the filter result as the visible view

      //    //See visible graph stats
      //    val graphVisible = graphModel.getUndirectedGraphVisible
      //    println("Nodes: " + graphVisible.getNodeCount)
      //    println("Edges: " + graphVisible.getEdgeCount)

      val layout = new ForceAtlas2(null)
      layout.setGraphModel(graphModel)
      layout.resetPropertiesValues()
      layout.setAdjustSizes(true)
      layout.setScalingRatio(100.0)
      layout.setOutboundAttractionDistribution(true)
      layout.initAlgo()
      var i = 0
      while (i < 1000 && layout.canAlgo) {
        layout.goAlgo()
        i += 1
      }
      layout.endAlgo()

      //Preview
      model.getProperties.putValue(PreviewProperty.SHOW_NODE_LABELS, true)
      model.getProperties.putValue(PreviewProperty.SHOW_EDGE_LABELS, true)
      model.getProperties.putValue(PreviewProperty.ARROW_SIZE, 100.0f)
      model.getProperties.putValue(PreviewProperty.NODE_OPACITY, 10.5f)
      model.getProperties.putValue(PreviewProperty.NODE_BORDER_COLOR, new DependantColor(Color.BLUE))
      model.getProperties.putValue(PreviewProperty.NODE_BORDER_WIDTH, 2.0f)
      model.getProperties.putValue(PreviewProperty.EDGE_CURVED, false)
      model.getProperties.putValue(PreviewProperty.EDGE_COLOR, new EdgeColor(Color.GRAY))
      model.getProperties.putValue(PreviewProperty.EDGE_THICKNESS, 0.1f)
      model.getProperties.putValue(PreviewProperty.NODE_LABEL_FONT, model.getProperties.getFontValue(PreviewProperty.NODE_LABEL_FONT).deriveFont(8))
      model.getProperties.putValue(PreviewProperty.NODE_LABEL_PROPORTIONAL_SIZE, false)

      //Export
      val ec: ExportController = assertedLookup(classOf[ExportController])
      val folderPath: Path = Paths.get(path)
      if (!Files.exists(folderPath)) Files.createDirectory(folderPath)
      val file = new File(path + name)
      ec.exportFile(file)
      file
    })
  }

  /**
    * convert graph into a drawable Gephi container
    *
    * @param g graph
    * @tparam N type of node
    * @tparam E type of edge
    * @return container
    */
  def toContainer[N, E[X] <: EdgeLikeIn[X]](g: Graph[N, E]): Try[Container] = Try {

    val c: Container = assertedLookup(classOf[Container.Factory]).newContainer
    val container: ContainerLoader = c.getLoader

    // real nodes
    val g_nodes: List[g.NodeT] = g.nodes.toList
    val nodes: List[NodeDraft] = g_nodes.map(g_node => {
      val n: NodeDraft = container.factory.newNodeDraft
      n.setLabel(g_node.toString)
      n
    })
    nodes.foreach(container.addNode)

    // separate edges into standard and hyper
    val g_edges: g.EdgeSet = g.edges
    val isWeighted = g_edges.exists(_.weight != 1.0)
    val (hyp_edges, std_edges) = g_edges.toList.partition(_.size > 2)

    // add extra "fake" node for each hyper edge (connecting >2 nodes)
    val fake_nodes = hyp_edges.indices.map(_ => {
      val n: NodeDraft = container.factory.newNodeDraft
      n.setLabel("")
      n.setSize(0.05f)
      n
    })
    fake_nodes.foreach(container.addNode)

    def getLabel(g_edge: g.EdgeT): String = {
      var label: List[String] = List()
      if (isWeighted) label = label :+ g_edge.weight.toString
      if (g_edge.isLabeled) label = label :+ g_edge.label.toString
      label.mkString(" - ")
    }

    def getDirection(g_edge: g.EdgeT): EdgeDirection =
      if (g_edge.isDirected) EdgeDirection.DIRECTED
      else EdgeDirection.UNDIRECTED

    def getNodeDraft(g_node: g.NodeT): NodeDraft = nodes(g_nodes.indexOf(g_node))

    std_edges.foreach(g_edge => {
      val e: EdgeDraft = container.factory.newEdgeDraft
      e.setDirection(getDirection(g_edge))
      e.setLabel(getLabel(g_edge))
      val s: NodeDraft = getNodeDraft(g_edge._1)
      val t: NodeDraft = getNodeDraft(g_edge._2)
      if (g_edge.to == g_edge._1) {
        e.setSource(t)
        e.setTarget(s)
      } else {
        e.setSource(s)
        e.setTarget(t)
      }
      container.addEdge(e)
    })

    hyp_edges.indices.foreach(i => {
      val ns: List[g.NodeT] = hyp_edges(i).map(g_node => g_node).toList
      // add undirected edge from first node to fake node
      val u: EdgeDraft = container.factory.newEdgeDraft
      u.setDirection(EdgeDirection.UNDIRECTED)
      u.setLabel(getLabel(hyp_edges(i)))
      val f: NodeDraft = fake_nodes(i)
      u.setSource(getNodeDraft(ns.head))
      u.setTarget(f)
      container.addEdge(u)
      // add unlabeled edges from fake node to other nodes
      ns.tail.foreach(g_node => {
        val e: EdgeDraft = container.factory.newEdgeDraft
        e.setDirection(getDirection(hyp_edges(i)))
        e.setSource(f)
        e.setTarget(getNodeDraft(g_node))
        container.addEdge(e)
      })
    })

    c
  }

}