package scalax.collection
package mutable

import GraphPredef.EdgeLikeIn
import GraphEdge.  {  HyperEdgeCompanion,                     EdgeCompanion}
import edge. WBase.{ WHyperEdgeCompanion,  WHyperEdgeBound,  WEdgeCompanion,  WEdgeBound}
import edge. LBase.{ LHyperEdgeCompanion,  LHyperEdgeBound,  LEdgeCompanion,  LEdgeBound}
import edge.WLBase.{WLHyperEdgeCompanion, WLHyperEdgeBound, WLEdgeCompanion, WLEdgeBound}

/**
 * This trait contains mutating edge addition methods that don't require an outer edge.
 * These methods are kept separately from `mutable.GraphLike` solely for convenience.
 * 
 * @define AUTOADD Node arguments not yet contained in this graph will be added to the node set.
 * @define NODE1 The first outer node to be incident with the edge to be created.
 *               In case of a directed edge, this becomes the source.  
 * @define NODE2 The second outer node to be incident with the edge to be created.
 *               In case of a directed edge, this becomes the target.  
 * @define NODEN The third and possibly more inner or outer nodes to be incident with the
 *               hyperedge to be created.
 *               In case of a directed edge, these become targets.
 * @define WEIGHT The number the weighted edge to be created should have as its weight.
 * @define LABEL  The value the labeled edge to be created should have as its label.               
 * @define EDGEFACTORY An edge companion who's `from` is to serve as the edge factory. 
 * @define EDGEADDED `true` if a new edge has been created. `false` if no new edge could be
 *         created because there exists a corresponding equaling edge.
 * @define HEDGEADDED `true` if a new hyperedge has been created. `false` if no new hyperedge could be
 *         created because there exists a corresponding equaling hyperedge.
 * @define THIS  This mutable graph containing the edge that has been created or found.
 * @define THISH This mutable graph containing the hyperedge that has been created or found.
 *         to be created `equals` to an already existing hyperedge.
 * @define EDGE The new edge or the corresponding contained edge if the edge
 *         to be created `equals` to an already existing edge.
 * @define HEDGE The new hyperedge or the corresponding contained hyperedge if the hyperedge
 *         to be created `equals` to an already existing hyperedge.
 * @author Peter Empen
 */
trait EdgeOps[N,
              E[X] <: EdgeLikeIn[X],
             +This[X, Y[X]<:EdgeLikeIn[X]] <: GraphLike[X,Y,This] with Graph[X,Y]]
{ selfGraph: GraphLike[N,E,This] =>

  trait InnerNodeOps
  { this: NodeT =>
    /**
     * Creates a new inner edge between this node and `that` using `edgeFactory`
     * and adds it to the edge set of this graph. 
     * $AUTOADD
     * Same as calling `addAndGetEdge(this, that)`. 
     * 
     * @param that $NODE2
     * @param edgeFactory $EDGEFACTORY
     * @return $EDGEADDED
     */
    @inline final
    def connectWith (that: N)(implicit edgeFactory: EdgeCompanion[E]) =
      selfGraph addAndGetEdge (this, that)
    /** Synonym to `connectWith` */
    @inline final
    def +~          (that: N)(implicit edgeFactory: EdgeCompanion[E]) =
      connectWith(that)
    /**
     * Creates a new inner hyperedge between this node and `that` plus `those` 
     * using `edgeFactory` and adds it to the edge set of this graph.
     * $AUTOADD
     * Same as calling `addAndGetEdge(this, that, those)` on a `Graph` instance. 
     * 
     * @param that  $NODE2
     * @param those $NODEN
     * @param edgeFactory $EDGEFACTORY 
     * @return $HEDGEADDED
     */
    @inline final
    def connectWith(that: N, those: N*)
                   (implicit edgeFactory: HyperEdgeCompanion[E]) =
      selfGraph addAndGetEdge (this, that, those: _*)
    /** Synonym to `connectWith` */
    @inline final
    def +~         (that: N, those: N*)
                   (implicit edgeFactory: HyperEdgeCompanion[E]) =
      connectWith(that, those: _*)
    // ---------------------------------------------------------------------- W
    /**
     * Creates a new weighted inner edge between this node and `that`
     * using `edgeFactory` and adds it to the edge set of this graph. 
     * $AUTOADD
     * Same as `addAndGetWEdge(this, that)(weight)` on a `Graph` instance.
     * 
     * @param that   $NODE2
     * @param weight $WEIGHT
     * @param edgeFactory $EDGEFACTORY
     * @return $EDGEADDED
     */
    @inline final
    def connectWithW[EE[X] <: E[X] with EdgeLikeIn[X] with WEdgeBound[_,EE]]
                     (that: N)
                     (weight: Long)
                     (implicit edgeFactory: WEdgeCompanion[EE]) =
      selfGraph.addAndGetWEdge[EE](this, that)(weight)
    /** Synonym to `connectWithW(that)(weight)`. */
    @inline final
    def +~% [EE[X] <: E[X] with EdgeLikeIn[X] with WEdgeBound[_,EE]]
             (that: N)
             (weight: Long)
             (implicit edgeFactory: WEdgeCompanion[EE]) =
      connectWithW[EE](that)(weight)
    /**
     * Creates a new weighted inner hyperedge between this node and `that` plus `those` 
     * using `edgeFactory` and adds it to the edge set of this graph.
     * $AUTOADD
     * Same as `addAndGetWEdge(this, that, those)(weight)` on a `Graph` instance. 
     * 
     * @param that   $NODE2
     * @param those  $NODEN
     * @param weight $WEIGHT
     * @param edgeFactory $EDGEFACTORY 
     * @return $HEDGEADDED
     */
    @inline final
    def connectWithW[EE[X] <: E[X] with EdgeLikeIn[X] with WEdgeBound[_,EE]]
                      (that: N, those: N*)
                      (weight: Long)
                      (implicit edgeFactory: WHyperEdgeCompanion[EE]) =
      selfGraph.addAndGetWEdge[EE](this, that, those: _*)(weight)
    /** Synonym to `connectWithW(that, those)(weight)`. */
    @inline final
    def +~% [EE[X] <: E[X] with EdgeLikeIn[X] with WEdgeBound[_,EE]]
             (that: N, those: N*)
             (weight: Long)
             (implicit edgeFactory: WHyperEdgeCompanion[EE]) =
      connectWithW[EE](that, those: _*)(weight)
    // ---------------------------------------------------------------------- L
    /**
     * Creates a new labeled inner edge between this node and `that`
     * using `edgeFactory` and adds it to the edge set of this graph. 
     * $AUTOADD
     * Same as `addAndGetLEdge(this, that)(label)` on a `Graph` instance.
     * 
     * @param that   $NODE2
     * @param label  $LABEL
     * @param edgeFactory $EDGEFACTORY
     * @return $EDGEADDED
     */
    @inline final
    def connectWithL[EE[X] <: E[X] with EdgeLikeIn[X] with LEdgeBound[_,EE], L]
                     (that: N)
                     (label: L)
                     (implicit edgeFactory: LEdgeCompanion[EE]) =
      selfGraph.addAndGetLEdge[EE,L](this, that)(label)
    /** Synonym to `connectWithL(that)(label)`. */
    @inline final
    def +~+ [EE[X] <: E[X] with EdgeLikeIn[X] with LEdgeBound[_,EE], L]
             (that: N)
             (label: L)
             (implicit edgeFactory: LEdgeCompanion[EE]) =
      connectWithL[EE,L](that)(label)
    /**
     * Creates a new labeled inner hyperedge between this node and `that` plus `those` 
     * using `edgeFactory` and adds it to the edge set of this graph.
     * $AUTOADD
     * Same as `addAndGetLEdge(this, that, those)(label)` on a `Graph` instance. 
     * 
     * @param that   $NODE2
     * @param those  $NODEN
     * @param label  $LABEL
     * @param edgeFactory $EDGEFACTORY 
     * @return $HEDGEADDED
     */
    @inline final
    def connectWithL[EE[X] <: E[X] with EdgeLikeIn[X] with LEdgeBound[_,EE], L]
                      (that: N, those: N*)
                      (label: L)
                      (implicit edgeFactory: LHyperEdgeCompanion[EE]) =
      selfGraph.addAndGetLEdge[EE,L](this, that, those: _*)(label)
    /** Synonym to `connectWithL(that, those)(label)`. */
    @inline final
    def +~+ [EE[X] <: E[X] with EdgeLikeIn[X] with LEdgeBound[_,EE], L]
             (that: N, those: N*)
             (label: L)
             (implicit edgeFactory: LHyperEdgeCompanion[EE]) =
      connectWithL[EE,L](that, those: _*)(label)
    // --------------------------------------------------------------------- WL
    /**
     * Creates a new weighted and labeled inner edge between this node and `that`
     * using `edgeFactory` and adds it to the edge set of this graph. 
     * $AUTOADD
     * Same as `addAndGetWLEdge(this, that)(weight, label)` on a `Graph` instance.
     * 
     * @param that   $NODE2
     * @param weight $WEIGHT
     * @param label  $LABEL
     * @param edgeFactory $EDGEFACTORY
     * @return $EDGEADDED
     */
    @inline final
    def connectWithWL[EE[X] <: E[X] with EdgeLikeIn[X] with WLEdgeBound[_,EE], L]
                     (that: N)
                     (weight: Long, label: L)
                     (implicit edgeFactory: WLEdgeCompanion[EE]) =
      selfGraph.addAndGetWLEdge[EE,L](this, that)(weight, label)
    /** Synonym to `connectWithWL(that)(weight, label)`. */
    @inline final
    def +~%+ [EE[X] <: E[X] with EdgeLikeIn[X] with WLEdgeBound[_,EE], L]
             (that: N)
             (weight: Long, label: L)
             (implicit edgeFactory: WLEdgeCompanion[EE]) =
      connectWithWL[EE,L](that)(weight, label)
    /**
     * Creates a new weighted and labeled inner hyperedge between this node and `that`
     * plus `those` using `edgeFactory` and adds it to the edge set of this graph.
     * $AUTOADD
     * Same as `addAndGetWLEdge(this, that, those)(weight, label)` on a `Graph` instance. 
     * 
     * @param that   $NODE2
     * @param those  $NODEN
     * @param weight $WEIGHT
     * @param label  $LABEL
     * @param edgeFactory $EDGEFACTORY 
     * @return $HEDGEADDED
     */
    @inline final
    def connectWithWL[EE[X] <: E[X] with EdgeLikeIn[X] with WLEdgeBound[_,EE], L]
                      (that: N, those: N*)
                      (weight: Long, label: L)
                      (implicit edgeFactory: WLHyperEdgeCompanion[EE]) =
      selfGraph.addAndGetWLEdge[EE,L](this, that, those: _*)(weight, label)
    /** Synonym to `connectWithWL(that, those)(weight, label)`. */
    @inline final
    def +~%+ [EE[X] <: E[X] with EdgeLikeIn[X] with WLEdgeBound[_,EE], L]
             (that: N, those: N*)
             (weight: Long, label: L)
             (implicit edgeFactory: WLHyperEdgeCompanion[EE]) =
      connectWithWL[EE,L](that, those: _*)(weight, label)
  }
  // ------------------------------------------------------------------------------- Edge
  /**
   * Creates a new inner edge between `node_1` and `node_2` using `edgeFactory`
   * and adds it to the edge set of this graph.
   * $AUTOADD
   * 
   * @param node_1      $NODE1
   * @param node_2      $NODE2
   * @param edgeFactory $EDGEFACTORY 
   * @return            $EDGEADDED
   */
  @inline final
  def addEdge (node_1: N, node_2: N)(implicit edgeFactory: EdgeCompanion[E]) =
    edges add nodesToEdgeCont(edgeFactory, node_1, node_2)
  /**
   * Same as `addEdge(node_1, node_2)` except for the returned result.
   * @return $THIS
   */
  @inline final
  def +~= (node_1: N, node_2: N)(implicit edgeFactory: EdgeCompanion[E]): this.type = {
    addEdge(node_1, node_2)
    this
  }
  /**
   * Same as `addEdge(node_1, node_2)` except for the returned result.
   * @return $EDGE
   */
  @inline final
  def addAndGetEdge (node_1: N, node_2: N)(implicit edgeFactory: EdgeCompanion[E]) = {
    val e = nodesToEdgeCont(edgeFactory, node_1, node_2)
    if  (edges add e) e
    else edges find (_ == e) get
  }
  /**
   * Creates a new inner hyperedge between `node_1`, `node_2` and `nodes`
   * using `edgeFactory` and adds it to the edge set of this graph.
   * $AUTOADD
   * 
   * @param node_1      $NODE1
   * @param node_2      $NODE2
   * @param nodes       $NODEN
   * @param edgeFactory $EDGEFACTORY 
   * @return            $HEDGEADDED
   */
  @inline final
  def addEdge(node_1: N, node_2: N, nodes: N*)
             (implicit edgeFactory: HyperEdgeCompanion[E]) =
    edges add nodesToEdgeCont(edgeFactory, node_1, node_2, nodes: _*)
  /**
   * Same as `addEdge(node_1, node_2, nodes)` except for the returned result.
   * @return $THISH
   */
  @inline final
  def +~=     (node_1: N, node_2: N, nodes: N*)
              (implicit edgeFactory: HyperEdgeCompanion[E]): this.type = {
    addEdge(node_1, node_2, nodes: _*)
    this
  }
  /**
   * Same as `addEdge(node_1, node_2, nodes)` except for the returned result.
   * @return $HEDGE
   */
  def addAndGetEdge (node_1: N, node_2: N, nodes: N*)
                    (implicit edgeFactory: HyperEdgeCompanion[E]) = {
    val e = nodesToEdgeCont(edgeFactory, node_1, node_2, nodes: _*)
    if  (edges add e) e
    else edges find (_ == e) get
  }
  // ------------------------------------------------------------------------------ WEdge
  /**
   * Creates a new weighted inner edge between `node_1` and `node_2`
   * using `edgeFactory` and adds it to the edge set of this graph.
   * $AUTOADD
   * 
   * @param node_1      $NODE1
   * @param node_2      $NODE2
   * @param weight      $WEIGHT
   * @param edgeFactory $EDGEFACTORY 
   * @return            $EDGEADDED
   */
  @inline final
  def addWEdge[EE[X] <: E[X] with EdgeLikeIn[X] with WEdgeBound[_,EE]]
              (node_1: N, node_2: N)
              (weight: Long)
              (implicit edgeFactory: WEdgeCompanion[EE]) =
    edges add nodesToWEdgeCont(edgeFactory, weight, node_1, node_2)
  /**
   * Same as `addWEdge(node_1, node_2)(weight)` except for the returned result.
   * @return $THIS
   */
  @inline final
  def +~%= [EE[X] <: E[X] with EdgeLikeIn[X] with WEdgeBound[_,EE]]
           (node_1: N, node_2: N)
           (weight: Long)
           (implicit edgeFactory: WEdgeCompanion[EE]): this.type = {
    addWEdge(node_1, node_2)(weight)
    this
  }
  /**
   * Same as `addWEdge(node_1, node_2)(weight)` except for the returned result.
   * @return $EDGE
   */
  @inline final
  def addAndGetWEdge[EE[X] <: E[X] with EdgeLikeIn[X] with WEdgeBound[_,EE]]
                    (node_1: N, node_2: N)
                    (weight: Long)
                    (implicit edgeFactory: WEdgeCompanion[EE]) = {
    val e = nodesToWEdgeCont(edgeFactory, weight, node_1, node_2)
    if  (edges add e) e
    else edges find (_ == e) get
  }
  /**
   * Creates a new weighted inner hyperedge between `node_1`, `node_2` and `nodes`
   * using `edgeFactory` and adds it to the edge set of this graph.
   * $AUTOADD
   * 
   * @param node_1      $NODE1
   * @param node_2      $NODE2
   * @param nodes       $NODEN
   * @param weight      $WEIGHT
   * @param edgeFactory $EDGEFACTORY 
   * @return            $HEDGEADDED
   */
  @inline final
  def addWEdge[EE[X] <: E[X] with EdgeLikeIn[X] with WHyperEdgeBound[_,EE]]
              (node_1: N, node_2: N, nodes: N*)
              (weight: Long)
              (implicit edgeFactory: WHyperEdgeCompanion[EE]) =
    edges add nodesToWEdgeCont(edgeFactory, weight, node_1, node_2, nodes: _*)
  /**
   * Same as `addWEdge(node_1, node_2, nodes)(weight)` except for the returned result.
   * @return $THISH
   */
  @inline final
  def +~%=    [EE[X] <: E[X] with EdgeLikeIn[X] with WHyperEdgeBound[_,EE]]
              (node_1: N, node_2: N, nodes: N*)
              (weight: Long)
              (implicit edgeFactory: WHyperEdgeCompanion[EE]): this.type = {
    addWEdge(node_1, node_2, nodes: _*)(weight)
    this
  }
  /**
   * Same as `addWEdge(node_1, node_2, nodes)(weight)` except for the returned result.
   * @return $HEDGE
   */
  def addAndGetWEdge[EE[X] <: E[X] with EdgeLikeIn[X] with WHyperEdgeBound[_,EE]]
                    (node_1: N, node_2: N, nodes: N*)
                    (weight: Long)
                    (implicit edgeFactory: WHyperEdgeCompanion[EE]) = {
    val e = nodesToWEdgeCont(edgeFactory, weight, node_1, node_2, nodes: _*)
    if  (edges add e) e
    else edges find (_ == e) get
  }
  // ------------------------------------------------------------------------------ LEdge
  /**
   * Creates a new labeled inner edge between `node_1` and `node_2`
   * using `edgeFactory` and adds it to the edge set of this graph.
   * $AUTOADD
   * 
   * @param node_1      $NODE1
   * @param node_2      $NODE2
   * @param label       $LABEL
   * @param edgeFactory $EDGEFACTORY 
   * @return            $EDGEADDED
   */
  @inline final
  def addLEdge[EE[X] <: E[X] with EdgeLikeIn[X] with LEdgeBound[_,EE], L]
              (node_1: N, node_2: N)
              (label: L)
              (implicit edgeFactory: LEdgeCompanion[EE]) =
    edges add nodesToLEdgeCont(edgeFactory, label, node_1, node_2)
  /**
   * Same as `addLEdge(node_1, node_2)(label)` except for the returned result.
   * @return $THIS
   */
  @inline final
  def +~+= [EE[X] <: E[X] with EdgeLikeIn[X] with LEdgeBound[_,EE], L]
           (node_1: N, node_2: N)
           (label: L)
           (implicit edgeFactory: LEdgeCompanion[EE]): this.type = {
    addLEdge(node_1, node_2)(label)
    this
  }
  /**
   * Same as `addLEdge(node_1, node_2)(label)` except for the returned result.
   * @return $EDGE
   */
  @inline final
  def addAndGetLEdge[EE[X] <: E[X] with EdgeLikeIn[X] with LEdgeBound[_,EE], L]
                    (node_1: N, node_2: N)
                    (label: L)
                    (implicit edgeFactory: LEdgeCompanion[EE]) = {
    val e = nodesToLEdgeCont(edgeFactory, label, node_1, node_2)
    if  (edges add e) e
    else edges find (_ == e) get
  }
  /**
   * Creates a new weighted and labeled inner hyperedge between `node_1`, `node_2` and `nodes`
   * using `edgeFactory` and adds it to the edge set of this graph.
   * $AUTOADD
   * 
   * @param node_1      $NODE1
   * @param node_2      $NODE2
   * @param nodes       $NODEN
   * @param label       $LABEL
   * @param edgeFactory $EDGEFACTORY 
   * @return            $HEDGEADDED
   */
  @inline final
  def addLEdge[EE[X] <: E[X] with EdgeLikeIn[X] with LHyperEdgeBound[_,EE], L]
              (node_1: N, node_2: N, nodes: N*)
              (label: L)
              (implicit edgeFactory: LHyperEdgeCompanion[EE]) =
    edges add nodesToLEdgeCont(edgeFactory, label, node_1, node_2, nodes: _*)
  /**
   * Same as `addLEdge(node_1, node_2, nodes)(label)` except for the returned result.
   * @return $THISH
   */
  @inline final
  def +~+=    [EE[X] <: E[X] with EdgeLikeIn[X] with LHyperEdgeBound[_,EE], L]
              (node_1: N, node_2: N, nodes: N*)
              (label: L)
              (implicit edgeFactory: LHyperEdgeCompanion[EE]): this.type = {
    addLEdge(node_1, node_2, nodes: _*)(label)
    this
  }
  /**
   * Same as `addLEdge(node_1, node_2, nodes)(label)` except for the returned result.
   * @return $HEDGE
   */
  def addAndGetLEdge[EE[X] <: E[X] with EdgeLikeIn[X] with LHyperEdgeBound[_,EE], L]
                    (node_1: N, node_2: N, nodes: N*)
                    (label: L)
                    (implicit edgeFactory: LHyperEdgeCompanion[EE]) = {
    val e = nodesToLEdgeCont(edgeFactory, label, node_1, node_2, nodes: _*)
    if  (edges add e) e
    else edges find (_ == e) get
  }
  // ----------------------------------------------------------------------------- WLEdge
  /**
   * Creates a new weighted and labeled inner edge between `node_1` and `node_2`
   * using `edgeFactory` and adds it to the edge set of this graph.
   * $AUTOADD
   * 
   * @param node_1      $NODE1
   * @param node_2      $NODE2
   * @param weight      $WEIGHT
   * @param label       $LABEL
   * @param edgeFactory $EDGEFACTORY 
   * @return            $EDGEADDED
   */
  @inline final
  def addWLEdge[EE[X] <: E[X] with EdgeLikeIn[X] with WLEdgeBound[_,EE], L]
              (node_1: N, node_2: N)
              (weight: Long, label: L)
              (implicit edgeFactory: WLEdgeCompanion[EE]) =
    edges add nodesToWLEdgeCont(edgeFactory, weight, label, node_1, node_2)
  /**
   * Same as `addWLEdge(node_1, node_2)(weight, label)` except for the returned result.
   * @return $THIS
   */
  @inline final
  def +~%+= [EE[X] <: E[X] with EdgeLikeIn[X] with WLEdgeBound[_,EE], L]
           (node_1: N, node_2: N)
           (weight: Long, label: L)
           (implicit edgeFactory: WLEdgeCompanion[EE]): this.type = {
    addWLEdge(node_1, node_2)(weight, label)
    this
  }
  /**
   * Same as `addWLEdge(node_1, node_2)(weight, label)` except for the returned result.
   * @return $EDGE
   */
  @inline final
  def addAndGetWLEdge[EE[X] <: E[X] with EdgeLikeIn[X] with WLEdgeBound[_,EE], L]
                    (node_1: N, node_2: N)
                    (weight: Long, label: L)
                    (implicit edgeFactory: WLEdgeCompanion[EE]) = {
    val e = nodesToWLEdgeCont(edgeFactory, weight, label, node_1, node_2)
    if  (edges add e) e
    else edges find (_ == e) get
  }
  /**
   * Creates a new weighted and labeled inner hyperedge between `node_1`, `node_2` and `nodes`
   * using `edgeFactory` and adds it to the edge set of this graph.
   * $AUTOADD
   * 
   * @param node_1      $NODE1
   * @param node_2      $NODE2
   * @param nodes       $NODEN
   * @param weight      $WEIGHT
   * @param label       $LABEL
   * @param edgeFactory $EDGEFACTORY 
   * @return            $HEDGEADDED
   */
  @inline final
  def addWLEdge[EE[X] <: E[X] with EdgeLikeIn[X] with WLHyperEdgeBound[_,EE], L]
              (node_1: N, node_2: N, nodes: N*)
              (weight: Long, label: L)
              (implicit edgeFactory: WLHyperEdgeCompanion[EE]) =
    edges add nodesToWLEdgeCont(edgeFactory, weight, label, node_1, node_2, nodes: _*)
  /**
   * Same as `addWLEdge(node_1, node_2, nodes)(weight, label)` except for the returned result.
   * @return $THISH
   */
  @inline final
  def +~%+=    [EE[X] <: E[X] with EdgeLikeIn[X] with WLHyperEdgeBound[_,EE], L]
              (node_1: N, node_2: N, nodes: N*)
              (weight: Long, label: L)
              (implicit edgeFactory: WLHyperEdgeCompanion[EE]): this.type = {
    addWLEdge(node_1, node_2, nodes: _*)(weight, label)
    this
  }
  /**
   * Same as `addWLEdge(node_1, node_2, nodes)(weight, label)` except for the returned result.
   * @return $HEDGE
   */
  def addAndGetWLEdge[EE[X] <: E[X] with EdgeLikeIn[X] with WLHyperEdgeBound[_,EE], L]
                    (node_1: N, node_2: N, nodes: N*)
                    (weight: Long, label: L)
                    (implicit edgeFactory: WLHyperEdgeCompanion[EE]) = {
    val e = nodesToWLEdgeCont(edgeFactory, weight, label, node_1, node_2, nodes: _*)
    if  (edges add e) e
    else edges find (_ == e) get
  }
  // ------------------------------------------------------------------ nodesTo<edge>Cont
  @inline final protected
  def nodesToEdgeCont  (edgeFactory: EdgeCompanion[E],
                        node_1:      N,
                        node_2:      N) =
    newEdge(EdgeAux.nodesToEdgeCont(edgeFactory, node_1, node_2))
  @inline final protected
  def nodesToEdgeCont  (edgeFactory: HyperEdgeCompanion[E],
                        node_1:      N,
                        node_2:      N,
                        nodes:       N*) =
    newEdge(EdgeAux.nodesToEdgeCont(edgeFactory, node_1, node_2, nodes: _*))
  @inline final protected
  def nodesToWEdgeCont [EE[X] <: E[X] with EdgeLikeIn[X] with WEdgeBound[_,EE]]
                       (edgeFactory: WEdgeCompanion[EE],
                        weight:      Long,
                        node_1:      N,
                        node_2:      N) =
    newEdge(EdgeAux.nodesToWEdgeCont(edgeFactory, weight, node_1, node_2))
  @inline final protected
  def nodesToWEdgeCont [EE[X] <: E[X] with EdgeLikeIn[X] with WHyperEdgeBound[_,EE]]
                       (edgeFactory: WHyperEdgeCompanion[EE],
                        weight:      Long,
                        node_1:      N,
                        node_2:      N,
                        nodes:       N*) =
    newEdge(EdgeAux.nodesToWEdgeCont(edgeFactory, weight, node_1, node_2, nodes: _*))
  @inline final protected
  def nodesToLEdgeCont [EE[X] <: E[X] with EdgeLikeIn[X] with LEdgeBound[_,EE], L]
                       (edgeFactory: LEdgeCompanion[EE],
                        label:       L,
                        node_1:      N,
                        node_2:      N) =
    newEdge(EdgeAux.nodesToLEdgeCont(edgeFactory, label, node_1, node_2))
  @inline final protected
  def nodesToLEdgeCont [EE[X] <: E[X] with EdgeLikeIn[X] with LHyperEdgeBound[_,EE], L]
                       (edgeFactory: LHyperEdgeCompanion[EE],
                        label:  L,
                        node_1:      N,
                        node_2:      N,
                        nodes:       N*) =
    newEdge(EdgeAux.nodesToLEdgeCont(edgeFactory, label, node_1, node_2, nodes: _*))
  @inline final protected
  def nodesToWLEdgeCont [EE[X] <: E[X] with EdgeLikeIn[X] with WLEdgeBound[_,EE], L]
                       (edgeFactory: WLEdgeCompanion[EE],
                        weight:      Long,
                        label:       L,
                        node_1:      N,
                        node_2:      N) =
    newEdge(EdgeAux.nodesToWLEdgeCont(edgeFactory, weight, label, node_1, node_2))
  @inline final protected
  def nodesToWLEdgeCont [EE[X] <: E[X] with EdgeLikeIn[X] with WLHyperEdgeBound[_,EE], L]
                       (edgeFactory: WLHyperEdgeCompanion[EE],
                        weight:      Long,
                        label:       L,
                        node_1:      N,
                        node_2:      N,
                        nodes:       N*) =
    newEdge(EdgeAux.nodesToWLEdgeCont(edgeFactory, weight, label, node_1, node_2, nodes: _*))
}