package scalax.collection.edges;/**
 * Represents a generic unlabeled directed edge.
 */
@SerialVersionUID(55)
        final case class DiEdge[+N](source: N, target: N) extends AbstractGenericDiEdge[N, DiEdge] {
                validate()
                def map[NN](node_1: NN, node_2: NN): DiEdge[NN] = copy[NN](node_1, node_2)
                }
