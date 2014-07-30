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

import scala.reflect.runtime.universe._

/**
 * Granthi element is a trait that provides common features of nodes and edges.
 * That are a optional graph id that stores the id given by Neo4j, if the Granthi
 * element has an representation in the database, and the full type name of the
 * concrete node or edge.
 */
trait GranthiElement {
  var graphId: Option[Long]
  val granthiType: String
  override def toString: String = s"[${if (graphId.isEmpty) "undef" else graphId.get}]"
}

/**
 * Granthi node is an abstract class that should be implemented by a concrete node.
 * It inherits from <code>granthi.GranthiElement</code>.
 *
 * @param graphId The optional graph id that is given by Neo4j
 * @tparam N Type of the node
 */
abstract class GranthiNode[N: TypeTag](override var graphId: Option[Long]) extends GranthiElement {
  protected def this() = this(None)
  override val granthiType = symbolOf[N].fullName
  override def toString: String = "<<Node>>" + symbolOf[N].name + super.toString
  def getTypeName: String = symbolOf[N].name.toString
}

/**
 * Granthi edge is an abstract class that should be implemented by a concrete edge.
 * An edge requires a start node and an end node.
 * It inherits from <code>granthi.GranthiElement</code>.
 *
 * @param graphId The optional graph id that is given by Neo4j
 * @param startNode The start node
 * @param endNode The end node
 * @tparam T Type of the edge
 * @tparam N Type of the start node
 * @tparam M Type of the end node
 */
abstract class GranthiEdge[T: TypeTag, N, M](override var graphId: Option[Long],
                                             val startNode: GranthiNode[N], val endNode: GranthiNode[M]) extends GranthiElement {
  protected def this(startNode: GranthiNode[N], endNode: GranthiNode[M])  = this(None, startNode, endNode)
  override val granthiType = symbolOf[T].fullName
  override def toString: String = "<<Edge>>" + symbolOf[T].name + super.toString
}
