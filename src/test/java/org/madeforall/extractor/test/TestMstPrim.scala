package org.madeforall.extractor.test

import org.scalatest._
import org.scalatest.matchers._
import org.madeforall.graph.mst.prim._
/**
 * @author Nicolae Caralicea
 * @version 1.0, 26/01/2013
 */
class TestMst extends FlatSpec with ShouldMatchers {
    
  "The minimal spanning resulted whn using the Prim's alghorith" should "be like" in {
    val graph: UndirectedGraph[Double] = UndirectedGraph(
      List(
        Node("A"), Node("B"), Node("C"), Node("D"),
        Node("E"), Node("F"), Node("G"), Node("H"),
        Node("M"), Node("N")),
      List(
        Edge("A", "B", 9.0),
        Edge("A", "F", 6),
        Edge("A", "G", 3),
        Edge("B", "G", 9),
        Edge("B", "M", 8),
        Edge("B", "C", 18),

        Edge("C", "M", 10),
        Edge("C", "N", 3),
        Edge("C", "D", 4),
        Edge("D", "N", 1),
        Edge("D", "E", 4),
        Edge("E", "F", 9),
        Edge("E", "H", 9),

        Edge("E", "M", 7),
        Edge("F", "G", 4),
        Edge("F", "H", 2),
        Edge("H", "G", 2),
        Edge("H", "M", 8),
        Edge("M", "N", 9),
        Edge("M", "G", 9),
        Edge("N", "E", 5)))
    assert(graph.computeMinimumSpanningTreeUsingPrim === UndirectedGraph(
        List(Node("B"), Node("C"), Node("N"), Node("D"), Node("E"), Node("M"), Node("F"), Node("H"), Node("G"), Node("A")),
        List(Edge("B","M",8), Edge("C","N",3), Edge("N","D",1), Edge("D","E",4), Edge("E","M",7), Edge("M","H",8), Edge("F","H",2), 
            Edge("H","G",2), Edge("G","A",3))))
  }
    
}
