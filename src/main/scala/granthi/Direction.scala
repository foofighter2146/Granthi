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

/**
 * Sealed trait to define directions of graph edges.
 */
sealed trait Direction
case object Outgoing extends Direction
case object Incoming extends Direction
case object Both extends Direction

/**
 * Provides functions regarding to <code>granthi.Direction</code>.
 */
object Directions {
  def addDirToCypherRelationship(s: String, direction: Direction): String =
    direction match {
      case Outgoing => s"$s>"
      case Incoming => s"<$s"
      case Both => s
  }
}
