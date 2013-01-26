package org.madeforall.graph.mst.prim

import scala.Option.option2Iterable


/**
 * @author Nicolae Caralicea
 * @version 1.0, 26/01/2013
 */
case class Node(name: String)
case class Edge(xNodeName: String, yNodeName: String, cost: Int)

case class UndirectedGraph(nodes: List[Node], edgetList: List[Edge]) {
  case class NodesEdge(xNode: Node, yNode: Node, cost: Int)

  def computeMinimumSpanningTreeUsingPrim(): UndirectedGraph =
    computeMinimumSpanningTree(nodes.tail, UndirectedGraph(List(nodes.head), Nil))

  private def computeMinimumSpanningTree(leftNodes: List[Node], mst: UndirectedGraph): UndirectedGraph = {
    leftNodes match {
      case Nil => mst
      case _ =>
        val minCostEdge = minCost(cost(leftNodes, mst)) // xNode belongs to leftNodes, and yNode belongs to mst
        if(minCostEdge == None) throw new Error("disconnected graph")
        val updatedLeftNodes =
          if (minCostEdge != None) leftNodes diff List(minCostEdge.get.xNode) else leftNodes
        val updatedMst =
          if (minCostEdge != None)
            UndirectedGraph(
              minCostEdge.get.xNode :: mst.nodes,
              toEdge(minCostEdge.get) :: mst.edgetList)
          else mst
        computeMinimumSpanningTree(updatedLeftNodes, updatedMst)
    }
  }
  
  private def toEdge(nodesEdge: NodesEdge): Edge =
    Edge(nodesEdge.xNode.name, nodesEdge.yNode.name, nodesEdge.cost)

  private def cost(outsideNodes: List[Node], graph: UndirectedGraph): List[NodesEdge] =
    for {
      node <- outsideNodes
      minCostEdgeNodeToGraph <- minCostEdgeOfNodeToGraph(node, graph.nodes)
    } yield minCostEdgeNodeToGraph

  private def minCost(nodesEdgeList: List[NodesEdge]): Option[NodesEdge] = {
    val min: Option[NodesEdge] = None
    nodesEdgeList.foldLeft(min)((comp, itm) =>
      if (comp != None) {
        if (comp.get.cost > itm.cost) Some(itm) else comp
      } else {
        Some(itm)
      })
  }

  private def cost(nodeA: Node, nodeB: Node): Option[Int] = {
    val costItem = edgetList.filter(item => {
      (item.xNodeName == nodeA.name && item.yNodeName == nodeB.name) ||
        (item.yNodeName == nodeA.name && item.xNodeName == nodeB.name)
    })
    if (costItem != Nil) Some(costItem.head.cost) else None
  }

  private def minCostEdgeOfNodeToGraph(node: Node, graph: List[Node]): Option[NodesEdge] = {
    val edges = for {
      n <- graph
      cost <- cost(n, node)
    } yield NodesEdge(node, n, cost)
    minCost(edges)
  }
}