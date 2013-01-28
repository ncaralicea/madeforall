package org.madeforall.graph.mst.prim

/**
 * @author Nicolae Caralicea
 * @version 1.0, 26/01/2013
 */
case class Node(name: String)
case class Edge[T <% Ordered[T]](xNodeName: String, yNodeName: String, cost: T)

case class UndirectedGraph[T <% Ordered[T]](nodes: List[Node], edgetList: List[Edge[T]]) {
  case class NodesEdge[T <% Ordered[T]](xNode: Node, yNode: Node, cost: T)

  def computeMinimumSpanningTreeUsingPrim(): UndirectedGraph[T] =
    computeMinimumSpanningTree(nodes.tail, UndirectedGraph(List(nodes.head), Nil))

  private def computeMinimumSpanningTree(leftNodes: List[Node], mst: UndirectedGraph[T]): UndirectedGraph[T] = {
    leftNodes match {
      case Nil => mst
      case _ =>
        val minCostEdge = minCost(cost(leftNodes, mst)) // xNode belongs to leftNodes, and yNode belongs to mst
        if (minCostEdge == None) throw new Error("disconnected graph")
        val updatedLeftNodes = leftNodes diff List(minCostEdge.get.xNode)
        val updatedMst = UndirectedGraph(
              minCostEdge.get.xNode :: mst.nodes,
              toEdge(minCostEdge.get) :: mst.edgetList)
        computeMinimumSpanningTree(updatedLeftNodes, updatedMst)
    }
  }

  private def toEdge[T <% Ordered[T]](nodesEdge: NodesEdge[T]): Edge[T] =
    Edge(nodesEdge.xNode.name, nodesEdge.yNode.name, nodesEdge.cost)

  private def cost(outsideNodes: List[Node], graph: UndirectedGraph[T]): List[NodesEdge[T]] =
    for {
      node <- outsideNodes
      minCostEdgeNodeToGraph <- minCostEdgeOfNodeToGraph(node, graph.nodes)
    } yield minCostEdgeNodeToGraph

  private def minCost[T <% Ordered[T]](nodesEdgeList: List[NodesEdge[T]]): Option[NodesEdge[T]] = {
    val min: Option[NodesEdge[T]] = None
    nodesEdgeList.foldLeft(min)((comp, itm) =>
      if (comp != None) {
        if (comp.get.cost > itm.cost) Some(itm) else comp
      } else {
        Some(itm)
      })
  }

  private def cost(nodeA: Node, nodeB: Node): Option[T] = {
    val costItem = edgetList.filter(item => {
      (item.xNodeName == nodeA.name && item.yNodeName == nodeB.name) ||
        (item.yNodeName == nodeA.name && item.xNodeName == nodeB.name)
    })
    if (costItem != Nil) Some(costItem.head.cost) else None
  }

  private def minCostEdgeOfNodeToGraph(node: Node, graph: List[Node]): Option[NodesEdge[T]] = {
    val edges = for {
      n <- graph
      cost <- cost(n, node)
    } yield NodesEdge(node, n, cost)
    minCost(edges)
  }
}