package scalax.collection.visualization

import java.awt.Color
import java.io.File
import java.nio.file.{Files, Path, Paths}
import java.util.logging.{Level, LogManager}

import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.util.Try

import org.gephi.filters.api.{FilterController, Range}
import org.gephi.filters.plugin.graph.DegreeRangeBuilder.DegreeRangeFilter
import org.gephi.graph.api.{GraphController, GraphModel, GraphView}
import org.gephi.io.exporter.api.ExportController
import org.gephi.io.importer.api._
import org.gephi.io.processor.plugin.DefaultProcessor
import org.gephi.layout.plugin.forceAtlas2.ForceAtlas2
import org.gephi.preview.api.PreviewController
import org.gephi.preview.api.PreviewProperty._
import org.gephi.preview.types.{DependantColor, EdgeColor}
import org.gephi.project.api.{ProjectController, Workspace}
import org.openide.util.Lookup

import scalax.collection.Graph
import scalax.collection.GraphPredef.EdgeLikeIn

/** Facilitates drawing any graph as an image.
  */
trait Drawable {

  import Drawable._

  private def assertedLookup[T <: AnyRef: ClassTag](clazz: Class[T]): T = {
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
      val rootLogger                         = LogManager.getLogManager.getLogger("")
      val lvl                                = rootLogger.getLevel
      rootLogger.setLevel(Level.WARNING)
      importController.process(container, new DefaultProcessor, initWorkspace)
      rootLogger.setLevel(lvl)
    }

    def filteredView(gm: GraphModel): GraphView = {
      val filterController: FilterController = assertedLookup(classOf[FilterController])
      val degreeFilter                       = new DegreeRangeFilter
      degreeFilter.init(gm.getDirectedGraph)
      degreeFilter.setRange(new Range(1, Integer.MAX_VALUE)) //Remove nodes with degree < 1
      filterController.filter(filterController.createQuery(degreeFilter))
    }

    def adjustLayout(gm: GraphModel, iterations: Int): Unit = {
      val layout = new ForceAtlas2(null)
      layout.setGraphModel(gm)
      layout.resetPropertiesValues()
      layout.setAdjustSizes(true)
      layout.setScalingRatio(100.0)
      layout.setOutboundAttractionDistribution(true)

      layout.initAlgo()
      0 to iterations forall { _ =>
        if (layout.canAlgo) {
          layout.goAlgo()
          true
        } else false
      }
      layout.endAlgo()
    }

    def setProperties(): Unit = {
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
    }

    def createFile: File = {
      val folderPath: Path = Paths.get(path)
      if (!Files.exists(folderPath)) Files.createDirectory(folderPath)
      new File(path + name)
    }

    toContainer(g).map { container =>
      appendToWorkspace(container)

      val graphModel: GraphModel = assertedLookup(classOf[GraphController]).getGraphModel
      graphModel.setVisibleView(filteredView(graphModel))
      adjustLayout(graphModel, 1000)
      setProperties()

      val file = createFile
      assertedLookup(classOf[ExportController]).exportFile(file)
      file
    }
  }

  /** convert graph into a drawable Gephi container
    *
    * @param g graph
    * @tparam N type of node
    * @tparam E type of edge
    * @return container
    */
  def toContainer[N, E[X] <: EdgeLikeIn[X]](g: Graph[N, E]): Try[Container] = Try {

    val container: Container    = assertedLookup(classOf[Container.Factory]).newContainer
    val loader: ContainerLoader = container.getLoader

    def addNode(lbl: String = "", size: Option[Float] = None): NodeDraft = {
      val n: NodeDraft = loader.factory.newNodeDraft
      n.setNode(lbl, size)
      loader.addNode(n)
      n
    }

    def addEdge(src: NodeDraft, trg: NodeDraft, dir: EdgeDirection, lbl: String = "", invert: Boolean = false): Unit = {
      val e: EdgeDraft = loader.factory.newEdgeDraft
      if (!invert)
        e.setEdge(src, trg, dir, lbl)
      else
        e.setEdge(trg, src, dir, lbl)
      loader.addEdge(e)
    }

    val isWeighted                          = g.edges.exists(_.weight != 1.0)
    val nodeDrafts: Map[g.NodeT, NodeDraft] = g.nodes.map(n => n -> addNode(lbl = n.toString))(collection.breakOut)

    implicit final class EdgeG(edge: g.EdgeT) {

      def getDirection: EdgeDirection =
        if (edge.isDirected) EdgeDirection.DIRECTED
        else EdgeDirection.UNDIRECTED

      def getLabel: String = {
        var label: List[String] = List()
        if (isWeighted) label = label :+ edge.weight.toString
        if (edge.isLabeled) label = label :+ edge.label.toString
        label.mkString(" - ")
      }
    }

    implicit final class NodeG(node: g.NodeT) {

      def asNodeDraft: NodeDraft = nodeDrafts(node)
    }

    var alreadyCounted: Set[(g.Node, g.Node)] = Set()

    g.edges.foreach { edge =>
      def fakeNode: NodeDraft = addNode(size = Some(0.05f))

      if (edge.nonHyperEdge) {
        val (node1, node2) = (edge._1, edge._2)
        val isInverted     = edge.to == node1
        val isMultiEdge    = !edge.isLooping && node1.connectionsWith(node2).size > 1

        def addSimple(): Unit =
          addEdge(
            src = node1.asNodeDraft,
            trg = node2.asNodeDraft,
            dir = edge.getDirection,
            lbl = edge.getLabel,
            invert = isInverted
          )

        if (!isMultiEdge)
          addSimple()
        else {
          if (!alreadyCounted.contains((node1, node2)) && !alreadyCounted.contains((node2, node1))) {
            alreadyCounted = alreadyCounted + ((node1, node2))
            addSimple()
          } else {
            val fake = fakeNode
            addEdge(
              src = (if (isInverted) node2 else node1).asNodeDraft,
              trg = fake,
              dir = EdgeDirection.UNDIRECTED
            )
            addEdge(
              src = fake,
              trg = (if (isInverted) node1 else node2).asNodeDraft,
              dir = edge.getDirection,
              lbl = edge.getLabel
            )
          }
        }
      } else {
        val realNodes = edge.nodes.toIterator
        val fake      = fakeNode
        addEdge(
          src = realNodes.next.asNodeDraft,
          trg = fake,
          dir = EdgeDirection.UNDIRECTED
        )
        realNodes.foreach(
          node =>
            addEdge(
              src = fake,
              trg = node.asNodeDraft,
              dir = edge.getDirection
          ))
      }
    }

    container
  }
}

private object Drawable {

  implicit final class EdgeD(e: EdgeDraft) {

    def setEdge(src: NodeDraft, trg: NodeDraft, dir: EdgeDirection, lbl: String = ""): Unit = {
      e.setSource(src)
      e.setTarget(trg)
      e.setDirection(dir)
      e.setLabel(lbl)
    }
  }

  implicit final class NodeD(n: NodeDraft) {

    def setNode(label: String = "", size: Option[Float] = None): Unit = {
      n.setLabel(label)
      if (size.isDefined) n.setSize(size.get)
    }
  }

}
