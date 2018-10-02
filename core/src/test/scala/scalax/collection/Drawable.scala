package scalax.collection

import java.awt.Color
import java.io.File
import java.nio.file.{Path, Paths, Files}
import java.util.logging.Level
import java.util.logging.LogManager

import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.util.Try

import org.gephi.layout.plugin.forceAtlas2.ForceAtlas2
import org.gephi.preview.types.DependantColor
import org.gephi.filters.api.FilterController
import org.gephi.filters.api.Range
import org.gephi.filters.plugin.graph.DegreeRangeBuilder.DegreeRangeFilter
import org.gephi.graph.api.{GraphController, GraphModel, GraphView}
import org.gephi.io.importer.api._
import org.gephi.io.exporter.api.ExportController
import org.gephi.io.processor.plugin.DefaultProcessor
import org.gephi.preview.api.PreviewController
import org.gephi.preview.api.PreviewProperty._
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
    * @tparam N type of node
    * @tparam E type of edge
    */
  def makeImage[N, E[X] <: EdgeLikeIn[X]](g: Graph[N, E], path: String, name: String): Try[File] = {

    def initWorkspace: Workspace = {
      val pc: ProjectController = assertedLookup(classOf[ProjectController])
      pc.newProject()
      pc.getCurrentWorkspace
    }

    def appendToWorkspace(container: Container): Unit = {
      val importController: ImportController = assertedLookup(classOf[ImportController])
      val rootLogger = LogManager.getLogManager.getLogger("")
      val lvl = rootLogger.getLevel
      rootLogger.setLevel(Level.WARNING)
      importController.process(container, new DefaultProcessor, initWorkspace)
      rootLogger.setLevel(lvl)
    }

    def adjustLayout(gm: GraphModel, iterations: Int): Unit = {
      val layout = new ForceAtlas2(null)
      layout.setGraphModel(gm)
      layout.resetPropertiesValues()
      layout.setAdjustSizes(true)
      layout.setScalingRatio(100.0)
      layout.setOutboundAttractionDistribution(true)
      layout.initAlgo()
      var i = 0
      while (i < iterations && layout.canAlgo) {
        layout.goAlgo()
        i += 1
      }
      layout.endAlgo()
    }

    def createFile: File = {
      val folderPath: Path = Paths.get(path)
      if (!Files.exists(folderPath)) Files.createDirectory(folderPath)
      new File(path + name)
    }

    toContainer(g).map(container => {

      appendToWorkspace(container)

      val graphModel: GraphModel = assertedLookup(classOf[GraphController]).getGraphModel

      def getFilteredView: GraphView = {
        val filterController: FilterController = assertedLookup(classOf[FilterController])
        val degreeFilter = new DegreeRangeFilter
        degreeFilter.init(graphModel.getDirectedGraph)
        degreeFilter.setRange(new Range(1, Integer.MAX_VALUE)) //Remove nodes with degree < 1
        val query = filterController.createQuery(degreeFilter)
        filterController.filter(query)
      }

      graphModel.setVisibleView(getFilteredView)

      adjustLayout(graphModel, 1000)

      val properties = assertedLookup(classOf[PreviewController]).getModel.getProperties
      properties.putValue(SHOW_NODE_LABELS, true)
      properties.putValue(SHOW_EDGE_LABELS, true)
      properties.putValue(ARROW_SIZE, 100.0f)
      properties.putValue(NODE_OPACITY, 10.5f)
      properties.putValue(NODE_BORDER_COLOR, new DependantColor(Color.BLUE))
      properties.putValue(NODE_BORDER_WIDTH, 2.0f)
      properties.putValue(EDGE_CURVED, false)
      properties.putValue(EDGE_COLOR, new EdgeColor(Color.GRAY))
      properties.putValue(EDGE_THICKNESS, 0.1f)
      properties.putValue(NODE_LABEL_FONT, properties.getFontValue(NODE_LABEL_FONT).deriveFont(8))
      properties.putValue(NODE_LABEL_PROPORTIONAL_SIZE, false)

      val ec: ExportController = assertedLookup(classOf[ExportController])
      val file = createFile
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