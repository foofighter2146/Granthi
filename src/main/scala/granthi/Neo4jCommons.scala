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

import granthi.GraphReflections._
import granthi.StandardTypes._

import scala.reflect.ClassTag
import scala.reflect.runtime.universe._

/**
 * This object contains all methods that are necessary for Neo4j database
 * interactions for nodes and edges both. It uses <code>GraphReflections</code>.
 */
object Neo4jCommons {

  /**
   * Converts all fields of a graph element into a String that contains field names
   * and field values for Cypher queries.
   *
   * @param element The graph element
   * @param statementType CreateFind or Update
   * @param typeTag Implicit Scala reflection type tag
   * @param classTag Implicit Scala reflection class tag
   * @tparam T Type of graph element
   * @return String with Cypher properties
   */
  def fieldsAsCypherPropsString[T <: GranthiElement](element: T, statementType: StatementType)(
                                   implicit typeTag: TypeTag[T], classTag: ClassTag[T]): String =
    buildCypherPropsString(fieldsToPropertyMap(element), statementType)

  /**
   * Converts a properties map into a String that contains the content of the map as
   * Cypher properties. The structure of the result depends on statement type for that this
   * string is assigned. The syntax differs between update (SET) and create or find.
   *
   * @param properties The property map
   * @param statementType CreateFind or Update
   * @return String with Cypher properties
   */
  def buildCypherPropsString(properties: Map[String, Any], statementType: StatementType): String =
    if (properties.isEmpty) ""
    else {
      val assign = statementType match {
        case CreateOrFind => ":"
        case Update => "="
      }
      val prefix = statementType match {
        case CreateOrFind => ""
        case Update => "n."
      }
      properties.foldLeft("") {
      (z, prop) =>
        val (name, value) = prop
        z + s" $prefix$name$assign${toCypherParam(value)},"
      }.dropRight(1)
    }

  /**
   * Creates a new instance of a certain Granthi node by type using Scala reflection.
   *
   * @param id The id of the graph node
   * @param propertyMap The map of properties
   * @param typeTag Implicit parameter for resolving type by reflection
   * @tparam T The Scala type of graph node
   * @return A new instance of a graph node for type T
   */
  def newGranthiNodeByType[T <: GranthiNode[T]](id: Long, propertyMap : Map[String, Any])(implicit typeTag: TypeTag[T]): T = {
    val (constructorMirror, valueTypeList) = prepareGranthiElementCtorByType(propertyMap)
    val result = constructorMirror(castTypes(valueTypeList, AnormCypherType):_*).asInstanceOf[T]
    result.graphId = Some(id)
    result
  }

  /**
   * Creates a new instance of a certain Granthi node by name using Scala reflection.
   *
   * @param id The id of the graph node
   * @param propertyMap The map of properties
   * @return A new instance of a graph node
   */
  def newGranthiNodeByName(id: Long, propertyMap : Map[String, Any]) = {
    val (constructorMirror, valueTypeList) = prepareGranthiElementCtorByName(propertyMap)
    val result = constructorMirror(castTypes(valueTypeList, AnormCypherType):_*).asInstanceOf[GranthiNode[_]]
    result.graphId = Some(id)
    result
  }

  /**
   * Creates a new instance of a certain Granthi edge that connects two graph nodes
   * by type using Scala reflection.
   *
   * @param id The id of the graph edge
   * @param propertyMap The map of properties
   * @param node1 Start node of this edge
   * @param node2 End node of this edge
   * @param typeTag Implicit parameter for resolving type by reflection
   * @tparam T The Scala type of graph edge
   * @tparam N The Scala type of start graph node
   * @tparam M The Scala type of end graph node
   * @return A new instance of a graph edge for type T
   */
  def newGranthiEdgeByType[T <: GranthiEdge[T, N, M], N <: GranthiNode[N], M <: GranthiNode[M]](id: Long, node1: N, node2: M,
                                                                                                propertyMap: Map[String, Any],
                                                                                                propertiesType: PropertiesType)
                                                                                               (implicit typeTag: TypeTag[T]): T = {
    val (constructorMirror, valueTypeList) = prepareGranthiElementCtorByType(propertyMap)
    val result = constructorMirror(List(node1, node2) ++ castTypes(valueTypeList, propertiesType):_*).asInstanceOf[T]
    result.graphId = Some(id)
    result
  }

  /**
   * Creates a new instance of a certain Granthi edge that connects two graph nodes
   * by name using Scala reflection.
   *
   * @param id The id of the graph edge
   * @param node1 Start node of this edge
   * @param node2 End node of this edge
   * @param propertyMap The map of properties
   * @return A new instance of a graph edge for type T
   */
  def newGranthiEdgeByName(id: Long, node1: GranthiNode[_], node2: GranthiNode[_], propertyMap: Map[String, Any]) = {
    val (constructorMirror, valueTypeList) = prepareGranthiElementCtorByName(propertyMap)
    val result = constructorMirror(List(node1, node2) ++ castTypes(valueTypeList, AnormCypherType):_*).asInstanceOf[GranthiEdge[_, _, _]]
    result.graphId = Some(id)
    result
  }

  /**
   * Converts the Granthi type of a Granthi element into a Cypher property definition.
   *
   * @param element The Granthi Element
   * @tparam T Type of Granthi element
   * @return String that contains the part of a Cypher property definition
   */
  def granthiTypeToProperty[T <: GranthiElement](element: T) = s"_granthiType_: '${element.granthiType}'"

  /**
   * Converts the Granthi type of a Granthi node or edge class into a Cypher property definition.
   *
   * @param classOfT Implicit parameter for resolving class by reflection
   * @param typeTagT Implicit parameter for resolving type by reflection
   * @tparam T Type of Granthi element
   * @return String that contains the part of a Cypher property definition
   */
  def granthiTypeToProperty[T <: GranthiElement](classOfT: Class[T])(implicit typeTagT: TypeTag[T]) = s"_granthiType_: '${symbolOf[T].fullName}'"

}
