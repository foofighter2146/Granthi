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

import scala.reflect.ClassTag
import scala.reflect.runtime.universe._

/**
 *  Neo4jEdges provides persistence operations for Granthi edges inside
 * a Neo4j database.
 */
object Neo4jEdges {

  /**
   * Adds a Granthi edge to the Neo4j database edges and sets the graph id
   * which is given from the database while creating the edge. It expects that
   * start node and end node both are existing in database yet.
   *
   * @param edge The Granthi edge to add
   * @param typeTag Implicit parameter for resolving type by reflection
   * @param classTag Implicit parameter for resolving class by reflection
   * @tparam T The certain Scala type of Granthi edge
   */
  def add[T <: GranthiEdge[T, _, _]](edge: T)(implicit typeTag: TypeTag[T], classTag: ClassTag[T]) {
    val props = fieldsAsCypherPropsString(edge, CreateOrFind)
    edge.graphId match {
      case None =>
        val startNode = edge.startNode
        val endNode = edge.endNode
        startNode.graphId match {
          case Some(startNodeId) => endNode.graphId match {
            case Some(endNodeId) => edge.graphId = Some(Cypher(s"""
              MATCH (n:${startNode.getTypeName}),(m:${endNode.getTypeName})
              WHERE id(n) = $startNodeId AND id(m) = $endNodeId
              CREATE (n)-[r:${symbolOf[T].name} {${if (!props.isEmpty) s"$props," else ""} ${granthiTypeToProperty(edge)}}]->(m)
              RETURN id(r) as relid""")().head[Long]("relid"))
            case None =>
              throw new GranthiException(s"Adding of '$edge' edge failed because end node has no valid id (maybe exists not in database yet)")
          }
          case None =>
            throw new GranthiException(s"Adding of edge '$edge' failed because start node has no valid id (maybe exists not in database yet)")
        }
      case Some(_) => throw new GranthiException(s"Adding of edge '$edge' failed because it has yet a graph id")
    }
  }

  /**
   * Syntactic sugar for adding a Granthi edge into Neo4j database.
   *
   * @param edge The Granthi edge to add
   * @param typeTag Implicit parameter for resolving type by reflection
   * @param classTag Implicit parameter for resolving class by reflection
   * @tparam T The certain Scala type of Granthi edge
   */
  def +[T <: GranthiEdge[T, _, _]](edge: T)(implicit typeTag: TypeTag[T], classTag: ClassTag[T]) {
    add(edge)
  }
  
  /**
   * Updates the properties of a Granthi edge in the database by refreshing values of
   * existing properties and adding new properties if field signature changes.
   *
   * @param edge The Granthi edge to update
   * @param typeTag Implicit parameter for resolving type by reflection
   * @param classTag Implicit parameter for resolving class by reflection
   * @tparam T The certain Scala type of Granthi edge
   */
  def update[T <: GranthiEdge[T, _, _]](edge: T)(implicit typeTag: TypeTag[T], classTag: ClassTag[T]) {
    edge.graphId match {
      case Some(id) =>
        Cypher(s"MATCH ()-[n:${symbolOf[T].name}]->() WHERE id(n) = $id SET ${fieldsAsCypherPropsString(edge, Update)}").execute()
      case None => throw new GranthiException(s"Updating edge failed: Missing the graph id for edge '$edge'")
    }
  }

  /**
   * Syntactic sugar for updating properties of a Granthi edge in
   * the regarding edge in the Neo4j database.
   *
   * @param edge The Granthi edge to update
   * @param typeTag Implicit parameter for resolving type by reflection
   * @param classTag Implicit parameter for resolving class by reflection
   * @tparam T The certain Scala type of Granthi edge
   */
  def ^[T <: GranthiEdge[T, _, _]](edge: T)(implicit typeTag: TypeTag[T], classTag: ClassTag[T]) {
    update(edge)
  }

  /**
   * Adds an edge in Neo4j database or updates it, if it already exists in database.
   *
   * @param edge The Granthi edge to merge
   * @param typeTag Implicit parameter for resolving type by reflection
   * @param classTag Implicit parameter for resolving class by reflection
   * @tparam T The certain Scala type of Granthi edge
   */
  def merge[T <: GranthiEdge[T, _, _]](edge: T)(implicit typeTag: TypeTag[T], classTag: ClassTag[T]) {
    edge.graphId match {
      case Some(id) => update(edge)
      case None => add(edge)
    }
  }

  /**
   * Syntactic sugar for merging an edge into Neo4j database
   *
   * @param edge The Granthi edge to merge
   * @param typeTag Implicit parameter for resolving type by reflection
   * @param classTag Implicit parameter for resolving class by reflection
   * @tparam T The certain Scala type of Granthi edge
   */
  def ~[T <: GranthiEdge[T, _, _]](edge: T)(implicit typeTag: TypeTag[T], classTag: ClassTag[T]) {
    merge(edge)
  }

  /**
   * Deletes a graph edge and sets the graph id to None
   * if deletion was successful.
   *
   * @param edge The Granthi edge to delete
   * @param typeTag Implicit parameter for resolving type by reflection
   * @tparam T The certain Scala type of Granthi edge
   */
  def delete[T <: GranthiEdge[T, _, _]](edge: T)(implicit typeTag: TypeTag[T]) {
    edge.graphId match {
      case Some(id) =>
        val delRes = Cypher(s"MATCH ()-[r:${symbolOf[T].name}]->() WHERE id(r) = $id DELETE r").execute()
        if (delRes) edge.graphId = None
      case None => throw new GranthiException(s"Deleting edge failed: Missing the graph id for edge '$edge'")
    }
  }

  /**
   * Syntactic sugar for deleting an edge from Neo4j database
   *
   * @param edge The Granthi edge to delete
   * @param typeTag Implicit parameter for resolving type by reflection
   * @tparam T The certain Scala type of Granthi edge
   */
  def -[T <: GranthiEdge[T, _, _]](edge: T)(implicit typeTag: TypeTag[T]) {
    delete(edge)
  }
}
