/*
 * Copyright 2014 Dr. Thomas Richert
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package granthi

import granthi.Neo4jCommons._
import org.anormcypher._

import scala.reflect.runtime.universe._

/**
 * All stuff to ask the Neo4j database for nodes and edges.
 */
object AskNeo4jFor {

  /**
   * Finds Granthi nodes by type that match some properties. If the properties map is empty,
   * it returns all Granthi nodes of type T.
   *
   * @param properties A possible empty map of properties
   * @param typeTag Implicit parameter for resolving type by reflection
   * @tparam T Type of Granthi node
   * @return A possible empty stream of result nodes
   */
  def nodes[T <: GranthiNode[T]](properties: Map[String, Any])(implicit typeTag: TypeTag[T]): Stream[T] =
    Cypher(s"MATCH (n:${symbolOf[T].name} {${buildCypherPropsString(properties, CreateOrFind)}}) RETURN n")()
      .map(row => row[NeoNode]("n"))
      .map(node => newGranthiNodeByType(node.id, node.props))

  /**
   * Finds all Granthi nodes by type.
   *
   * @param typeTag Implicit parameter for resolving type by reflection
   * @tparam T Type of Granthi node
   * @return A possible empty stream of result nodes
   */
  def allNodes[T <: GranthiNode[T]](implicit typeTag: TypeTag[T]): Stream[T] = nodes(Map.empty)

  /**
   * Finds Granthi edges by type that matching some properties. Because an edge
   * needs the information about the nodes that it connects, it finds start node
   * and end node to. Thus its necessary to assign types of both nodes.
   *
   * @param properties A possible empty map of properties
   * @param direction The direction of the relation
   * @param typeTagT Implicit parameter for resolving type T by reflection
   * @param typeTagN Implicit parameter for resolving type N by reflection
   * @param typeTagM Implicit parameter for resolving type M by reflection
   * @tparam T Type of Granthi edge
   * @tparam N Type of start node
   * @tparam M Type of end node
   * @return A possible empty stream of result edges
   */
  def edges[T <: GranthiEdge[T, N, M], N <: GranthiNode[N], M <: GranthiNode[M]](properties: Map[String, Any], direction: Direction)
                                                                                (implicit typeTagT: TypeTag[T],
                                                                                 typeTagN: TypeTag[N],
                                                                                 typeTagM: TypeTag[M]): Stream[T] = {
    val relation = Directions.addDirToCypherRelationship(s"-[r:${symbolOf[T].name} {${buildCypherPropsString(properties, CreateOrFind)}}]-", direction)
    Cypher(s"MATCH (n:${symbolOf[N].name})$relation(m:${symbolOf[M].name}) RETURN n, r, m")()
      .map(row => (row[NeoNode]("n"), row[NeoRelationship]("r"), row[NeoNode]("m")))
      .map(r => newGranthiEdgeByType(r._2.id,
        newGranthiNodeByType[N](r._1.id, r._1.props),
        newGranthiNodeByType[M](r._3.id, r._3.props),
         r._2.props, AnormCypherType))
  }

  /**
   * Finds Granthi edges by type that matching some properties and same type
   * of start node and end node.
   *
   * @param properties A possible empty map of properties
   * @param direction The direction of the relation
   * @param typeTagT Implicit parameter for resolving type T by reflection
   * @param typeTagN Implicit parameter for resolving type N by reflection
   * @tparam T Type of Granthi edge
   * @tparam N Type of both end nodes of the edge
   * @return A possible empty stream of result edges
   */
  def edges[T <: GranthiEdge[T, N, N], N <: GranthiNode[N]](properties: Map[String, Any], direction: Direction)
                                                           (implicit typeTagT: TypeTag[T], typeTagN: TypeTag[N]): Stream[T] =
    edges[T, N, N](properties, direction)

  /**
   * Finds all graph edges by type. Because an edge
   * needs the information about the nodes that it connects, it finds start node
   * and end node to. Thus its necessary to assign types of both nodes.
   *
   * @param direction The direction of the relation
   * @param typeTagT Implicit parameter for resolving type T by reflection
   * @param typeTagN Implicit parameter for resolving type N by reflection
   * @param typeTagM Implicit parameter for resolving type M by reflection
   * @tparam T Type of Granthi edge
   * @tparam N Type of start node
   * @tparam M Type of end node
   * @return A possible empty stream of result edges
   */
  def allEdges[T <: GranthiEdge[T, N, M], N <: GranthiNode[N], M <: GranthiNode[M]](direction: Direction)
                                                                                   (implicit typeTagT: TypeTag[T],
                                                                                    typeTagN: TypeTag[N],
                                                                                    typeTagM: TypeTag[M]): Stream[T] =
    edges[T, N, M](Map.empty, direction)

  /**
   * Finds all graph edges by type. Both end nodes have same type.
   *
   * @param direction The direction of the relation
   * @param typeTagT Implicit parameter for resolving type T by reflection
   * @param typeTagN Implicit parameter for resolving type N by reflection
   * @tparam T Type of Granthi edge
   * @tparam N Type of both end nodes of the edge
   * @return  A possible empty stream of result edges
   */
  def allEdges[T <: GranthiEdge[T, N, N], N <: GranthiNode[N]](direction: Direction)
                                                              (implicit typeTagT: TypeTag[T], typeTagN: TypeTag[N]): Stream[T] =
    allEdges[T, N, N](direction)

  /**
   * Finds all incident edges of a node.
   * 
   * @param node The Granthi node
   * @param dir The direction of the relation
   * @tparam N Type of Granthi node
   * @return A possible empty stream of result edges
   */
  def allIncidentEdges[N <: GranthiNode[N]](node: N, dir: Direction): Stream[GranthiEdge[_, _, _]] =
    findIncidentEdges(node, Directions.addDirToCypherRelationship("-[r]-", dir))

  /**
   * Finds all incident edges of a node that belongs to certain relation.
   *
   * @param node The Granthi node
   * @param dir The direction of the relation
   * @param classOfEdge The Class that represents the relation edge
   * @tparam N Type of Granthi node
   * @tparam T Type of the Granthi edge that represents the relation
   * @return A possible empty stream of result edges
   */
  def incidentEdgesForRel[N <: GranthiNode[N], T <: GranthiEdge[T, N, _]](node: N, dir: Direction,
                                                                          classOfEdge: Class[T]): Stream[GranthiEdge[_, _, _]] =
    findIncidentEdges(node, Directions.addDirToCypherRelationship(s"-[r:${classOfEdge.getSimpleName}]-", dir))

  /* Common algorithm for finding incident edges */
  private def findIncidentEdges[N <: GranthiNode[N]](node: N, relation: String): Stream[GranthiEdge[_, _, _]] =
    node.graphId match {
      case Some(id) =>
        Cypher(s"MATCH (n:${node.getTypeName})$relation(m) WHERE id(n) = $id RETURN m, r")()
          .map(row => (row[NeoNode]("m"), row[NeoRelationship]("r")))
          .map(r => newGranthiEdgeByName(r._2.id, node, newGranthiNodeByName(r._1.id, r._1.props), r._2.props))
      case None => throw new GranthiException(s"Cannot find incident edges: Missing the graph id for node $node")
    }
  
  /**
   * Finds all adjacent nodes for a node.
   *
   * @param node The Granthi node
   * @param dir The direction of the relation
   * @tparam N Type of Granthi node
   * @return A possible empty stream of result nodes
   */
  def allAdjacentNodes[N <: GranthiNode[N]](node: N, dir: Direction): Stream[GranthiNode[_]] =
    findAdjacentNodes(node, Directions.addDirToCypherRelationship ("--", dir))

  /**
   * Finds all adjacent nodes for a node that belongs to certain relation.
   *
   * @param node The Granthi node
   * @param dir The direction of the relation
   * @param classOfEdge The Class that represents the relation edge
   * @tparam N Type of Granthi node
   * @tparam T Type of the Granthi edge that represents the relation
   * @return  A possible empty stream of result nodes
   */
  def adjacentNodesForRel[N <: GranthiNode[N], T <: GranthiEdge[T, N, _]](node: N, dir: Direction, classOfEdge: Class[T]): Stream[GranthiNode[_]] =
    findAdjacentNodes(node, Directions.addDirToCypherRelationship(s"-[r:${classOfEdge.getSimpleName}]-", dir))

  /* Common algorithm for finding adjacent edges */
  private def findAdjacentNodes[N <: GranthiNode[N]](node: N, relation: String): Stream[GranthiNode[_]] =
    node.graphId match {
      case Some(id) =>
        Cypher(s"MATCH (n:${node.getTypeName})$relation(m) WHERE id(n) = $id RETURN m")()
          .map(row => row[NeoNode]("m"))
          .map(r => newGranthiNodeByName(r.id, r.props))
      case None => throw new GranthiException(s"Cannot find adjacent nodes: Missing the graph id for node $node")
    }
}
