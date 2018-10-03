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

  implicit final class EdgeD(e: EdgeDraft) {

    def setEdge(source: NodeDraft, target: NodeDraft, direction: EdgeDirection, label: String = ""): Unit = {
      e.setSource(source)
      e.setTarget(target)
      e.setDirection(direction)
      e.setLabel(label)
    }

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

    val container: Container = assertedLookup(classOf[Container.Factory]).newContainer
    val loader: ContainerLoader = container.getLoader

    def addEdge(source: NodeDraft, target: NodeDraft, direction: EdgeDirection, label: String = "", invert: Boolean = false): Unit = {
      val e: EdgeDraft = loader.factory.newEdgeDraft
      if (!invert)
        e.setEdge(source, target, direction, label)
      else
        e.setEdge(target, source, direction, label)
      loader.addEdge(e)
    }

    // real nodes
    val g_nodes: List[g.NodeT] = g.nodes.toList
    val nodes: List[NodeDraft] = g_nodes.map(g_node => {
      val n: NodeDraft = loader.factory.newNodeDraft
      n.setLabel(g_node.toString)
      n
    })
    nodes.foreach(loader.addNode)

    // separate edges into standard and hyper
    val g_edges: g.EdgeSet = g.edges
    val isWeighted = g_edges.exists(_.weight != 1.0)
    val (hyp_edges, std_edges) = g_edges.toList.partition(_.size > 2)

    // add extra "fake" node for each hyper edge (connecting >2 nodes)
    val fake_nodes = hyp_edges.indices.map(_ => {
      val n: NodeDraft = loader.factory.newNodeDraft
      n.setLabel("")
      n.setSize(0.05f)
      n
    })
    fake_nodes.foreach(loader.addNode)

    implicit final class EdgeG(g_edge: g.EdgeT) {

      def getDirection: EdgeDirection =
        if (g_edge.isDirected) EdgeDirection.DIRECTED
        else EdgeDirection.UNDIRECTED

      def getLabel: String = {
        var label: List[String] = List()
        if (isWeighted) label = label :+ g_edge.weight.toString
        if (g_edge.isLabeled) label = label :+ g_edge.label.toString
        label.mkString(" - ")
      }

    }

    implicit final class NodeG(g_node: g.NodeT) {

      def toNodeDraft: NodeDraft = nodes(g_nodes.indexOf(g_node))

    }

    std_edges.foreach(g_edge => addEdge(
      source = g_edge._1.toNodeDraft,
      target = g_edge._2.toNodeDraft,
      direction = g_edge.getDirection,
      label = g_edge.getLabel,
      invert = g_edge.to == g_edge._1
    ))

    hyp_edges.indices.foreach(i => {
      val ns: List[g.NodeT] = hyp_edges(i).map(g_node => g_node).toList
      val fake_node: NodeDraft = fake_nodes(i)
      addEdge(
        source = ns.head.toNodeDraft,
        target = fake_node,
        direction = EdgeDirection.UNDIRECTED
      )
      ns.tail.foreach(g_node => addEdge(
        source = fake_node,
        target = g_node.toNodeDraft,
        direction = hyp_edges(i).getDirection
      ))
    })

    container
  }

}