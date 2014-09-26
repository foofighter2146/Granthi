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

import scala.reflect._
import scala.reflect.runtime.universe._

/**
 * Neo4jNodes provides persistence operations for Granthi nodes inside
 * a Neo4j database.
 */
object Neo4jNodes {

  /**
   * Adds a new node to the Neo4j database and sets the graph id,
   * which is given from the database while creating the node.
   *
   * @param node The Granthi node to add
   * @param typeTag Implicit parameter for resolving type by reflection
   * @param classTag Implicit parameter for resolving class by reflection
   * @tparam T The certain Scala type of Granthi node
   */
  def add[T <: GranthiNode[T]](node: T)(implicit typeTag: TypeTag[T], classTag: ClassTag[T]) {
    val props = fieldsAsCypherPropsString(node, CreateOrFind)
    node.graphId match {
      case None => node.graphId = Some(
        Cypher(s"CREATE (n:${symbolOf[T].name} {${if (!props.isEmpty) s"$props," else ""} ${granthiTypeToProperty(node)}}) RETURN id(n) as nodeid")()
          .head[Long]("nodeid")
      )
      case Some(_) => throw new GranthiException(s"Adding of node '$node' failed because it has yet a graph id")
    }
  }

  /**
   * Syntactic sugar for adding a Granthi node into Neo4j database.
   * 
   * @param node The Granthi node to add
   * @param typeTag Implicit parameter for resolving type by reflection
   * @param classTag Implicit parameter for resolving class by reflection
   * @tparam T The certain Scala type of Granthi node
   */
  def +[T <: GranthiNode[T]](node: T)(implicit typeTag: TypeTag[T], classTag: ClassTag[T]) {
    add(node)
  }

  /**
   * Updates the properties of a node in a Neo4j database by refreshing values of
   * existing properties and adding new properties if field signature changes.
   *
   * @param node The Granthi node to update
   * @param typeTag Implicit parameter for resolving type by reflection
   * @param classTag Implicit parameter for resolving class by reflection
   * @tparam T The certain Scala type of Granthi node
   */
  def update[T <: GranthiNode[T]](node: T)(implicit typeTag: TypeTag[T], classTag: ClassTag[T]) {
    node.graphId match {
      case Some(id) => Cypher(s"MATCH (n:${symbolOf[T].name}) WHERE id(n) = $id SET ${fieldsAsCypherPropsString(node, Update)}").execute()
      case None => throw new GranthiException(s"Updating node failed: Missing the graph id for node '$node'")
    }
  }

  /**
   * Syntactic sugar for updating the properties of a Neo4j node.
   *
   * @param node The Granthi node to update
   * @param typeTag Implicit parameter for resolving type by reflection
   * @param classTag Implicit parameter for resolving class by reflection
   * @tparam T The certain Scala type of Granthi node
   */
  def ^[T <: GranthiNode[T]](node: T)(implicit typeTag: TypeTag[T], classTag: ClassTag[T]) {
    update(node)
  }

  /**
   * Adds a node to Neo4j database or updates it, if it already exists in database.
   *
   * @param node The Granthi node to add or to update
   * @param typeTag Implicit parameter for resolving type by reflection
   * @param classTag Implicit parameter for resolving class by reflection
   * @tparam T The certain Scala type of Granthi node
   */
  def merge[T <: GranthiNode[T]](node: T)(implicit typeTag: TypeTag[T], classTag: ClassTag[T]) {
    node.graphId match {
      case Some(id) => update(node)
      case None => add(node)
    }
  }

  /**
   * Syntactic sugar for merging a node into Neo4j database
   *
   * @param node The Granthi node to add or to update
   * @param typeTag Implicit parameter for resolving type by reflection
   * @param classTag Implicit parameter for resolving class by reflection
   * @tparam T The certain Scala type of Granthi node
   */
  def ~[T <: GranthiNode[T]](node: T)(implicit typeTag: TypeTag[T], classTag: ClassTag[T]) {
    merge(node)
  }

  /**
   * Deletes a node and all connecting edges from Neo4j database and
   * sets the graph id to None if deletion was successful.
   *
   * @param node The Granthi node to delete
   * @param typeTag Implicit parameter for resolving type by reflection
   * @tparam T The certain Scala type of Granthi node
   */
  def delete[T <: GranthiNode[T]](node: T)(implicit typeTag: TypeTag[T]) {
    node.graphId match {
      case Some(id) =>
        val delRes = Cypher(s"MATCH (n:${symbolOf[T].name}) WHERE id(n) = $id OPTIONAL MATCH (n)-[r]-() DELETE n, r").execute()
        if (delRes) node.graphId = None
      case None => throw new GranthiException(s"Deleting node failed: Missing the graph id for node '$node'")
    }
  }

  /**
   * Syntactic sugar for deleting a node from Neo4j database
   *
   * @param node The node to delete
   * @param typeTag Implicit parameter for resolving type by reflection
   * @tparam T The Scala type of Granthi node
   */
  def -[T <: GranthiNode[T]](node: T)(implicit typeTag: TypeTag[T]) {
    delete(node)
  }

  /**
   * Connects two Granthi nodes by a new edge with certain type. If the nodes
   * are not stored in database yet, they will be added first. Take care about
   * well defined properties for the edge that your specified. Otherwise it quits
   * with an exception.
   *
   * @param from The start node
   * @param to The end node
   * @param by Class of edge
   * @param properties Optional properties that have to be stored inside the edge
   * @param typeTagT Type Tag of type T
   * @param typeTagN Type Tag of type N
   * @param typeTagM Type Tag of type M
   * @param classTagN Class Tag of type N
   * @param classTagM Class Tag of type M
   * @tparam N Type of start node
   * @tparam M Type of end node
   * @tparam T Type of edge
   * @return The new edge
   */
  def connect[N <: GranthiNode[N], M <: GranthiNode[M], T <: GranthiEdge[T, N, M]](from: N, to: M, by: Class[T], properties: Map[String, Any])
                                                                                  (implicit typeTagT: TypeTag[T],
                                                                                   typeTagN: TypeTag[N],
                                                                                   typeTagM: TypeTag[M],
                                                                                   classTagN: ClassTag[N],
                                                                                   classTagM: ClassTag[M]): T = {
    if (from.graphId.isEmpty) add(from)
    if (to.graphId.isEmpty) add(to)
   // Stores the edge first to get the id
    val props = buildCypherPropsString(properties, CreateOrFind)
    val edgeId = Cypher(s"""
        MATCH (n:${from.getTypeName}),(m:${to.getTypeName})
        WHERE id(n) = ${from.graphId.get} AND id(m) = ${to.graphId.get}
        CREATE (n)-[r:${symbolOf[T].name} {${if (!props.isEmpty) s"$props," else ""} ${granthiTypeToProperty(by)}}]->(m)
        RETURN id(r) as relid""")().head[Long]("relid")
    try {
      // Try to create a new Granthi edge by reflection...
      newGranthiEdgeByType(edgeId, from, to, properties, GranthiType)
    } catch {
      case _:IllegalArgumentException =>
        // ...but this can fail while passing wrong parameters to the constructor
        // Then delete the edge from database again
        Cypher(s"MATCH ()-[r]-() WHERE id(r) = $edgeId DELETE r").execute()
        throw new GranthiException(s"Wrong properties definition for edge type $by while trying to connect $from with $to")
    }
  }

  /**
   * Connects two Granthi nodes by a new edge with certain type and empty properties.
   * If the nodes are not stored in database yet, they will be added first. Take care about
   * that your edge data type has no params but the two nodes. Otherwise it quits
   * with an exception.
   *
   * @param from The start node
   * @param to The end node
   * @param by Class of edge
   * @param typeTagT Type Tag of type T
   * @param typeTagN Type Tag of type N
   * @param typeTagM Type Tag of type M
   * @param classTagN Class Tag of type N
   * @param classTagM Class Tag of type M
   * @tparam N Type of start node
   * @tparam M Type of end node
   * @tparam T Type of edge
   * @return The new edge
   */
  def connect[N <: GranthiNode[N], M <: GranthiNode[M], T <: GranthiEdge[T, N, M]](from: N, to: M, by: Class[T])
                                                                                  (implicit typeTagT: TypeTag[T],
                                                                                   typeTagN: TypeTag[N],
                                                                                   typeTagM: TypeTag[M],
                                                                                   classTagN: ClassTag[N],
                                                                                   classTagM: ClassTag[M]): T =
    connect(from, to, by, Map.empty)

  /**
   * Disconnects two nodes by deleting the edge from database.
   *
   * @param from The start node
   * @param to The end node
   * @param by The class of edge
   * @param typeTagT Type Tag of type T
   * @param typeTagN Type Tag of type N
   * @param typeTagM Type Tag of type M
   * @param classTagN Class Tag of type N
   * @param classTagM Class Tag of type M
   *  @tparam N Type of start node
   * @tparam M Type of end node
   * @tparam T Type of edge
   * @return
   */
  def disconnect[N <: GranthiNode[N], M <: GranthiNode[M], T <: GranthiEdge[T, N, M]](from: N, to: M, by: Class[T])
                                                                                     (implicit typeTagT: TypeTag[T],
                                                                                     typeTagN: TypeTag[N],
                                                                                     typeTagM: TypeTag[M],
                                                                                     classTagN: ClassTag[N],
                                                                                     classTagM: ClassTag[M]) {
    val edges = AskNeo4jFor.incidentEdgesForRel(from, Outgoing, by)
    edges filter(edge => edge.endNode.equals(to)) foreach {
      edge => Cypher(s"MATCH ()-[r]-() WHERE id(r) = ${edge.graphId.get} DELETE r").execute()
    }
  }
}
