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

import java.time.LocalDate
import java.util.Date

import granthi.Neo4jCommons._
import org.scalatest._

class Neo4jCommonsSpec extends FlatSpec with Matchers {
  import GranthiProperties._

  "Neo4j Common Interactions" should "convert the fields of a node into Cypher properties as a String" in {
    // Because the order of the properties inside the string is abritrary, the string must be parsed in a map first
    val peter = Person_GQT_("Shaw", "Peter", 43, LocalDate.of(1970, 8, 23))
    val remappedString = parseCypherPropertiesString(fieldsAsCypherPropsString(peter, CreateOrFind), ":")
    remappedString("tag") shouldBe "'granthitest'"
    remappedString("lastname") shouldBe "'Shaw'"
    remappedString("firstname") shouldBe "'Peter'"
    remappedString("age") shouldBe "43"
    remappedString("birthdate") shouldBe "'1970-08-23'"
  }

  it should "convert the fields of an edge into Cypher properties as a String" in {
    // Because the order of the properties inside the string is abritrary, the string must be parsed in a map first
    val peter = Person_GQT_("Shaw", "Peter", 43, LocalDate.of(1970, 8, 23))
    val dana = Person_GQT_("Shaw", "Dana", 41, LocalDate.of(1972, 11, 12))
    val peterAndDana = IsMarriedWith_GQT_(peter, dana, LocalDate.of(1990, 9, 9), "rk")
    val remappedString = parseCypherPropertiesString(fieldsAsCypherPropsString(peterAndDana, CreateOrFind), ":")
    remappedString("since") shouldBe "'1990-09-09'"
    remappedString("confession") shouldBe "'rk'"
  }

  it should "convert the fields of a node into a Cypher notation suitable for SET statements" in {
    val peter = Person_GQT_("Shaw", "Peter", 43, new LocalDateProperty(LocalDate.of(1970, 8, 23)))
    // Because the order of the properties inside the string is abritrary, the string must be parsed in a map first
    val remappedString = parseCypherPropertiesString(fieldsAsCypherPropsString(peter, Update), "=")
    remappedString("n.tag") shouldBe "'granthitest'"
    remappedString("n.lastname") shouldBe "'Shaw'"
    remappedString("n.firstname") shouldBe "'Peter'"
    remappedString("n.age") shouldBe "43"
    remappedString("n.birthdate") shouldBe "'1970-08-23'"
  }

  /**
   * Helper that parses a Cypher properties String in both variants into a map.
   * @param s The String to parse
   * @param keyValueSplitter The splitter symbol depending on the variant of the properties String
   * @return The result map
   */
  private def parseCypherPropertiesString(s: String, keyValueSplitter: String): Map[String, String] =
    s.replace('{', ' ').replace('}',' ').split(",").toList.map(_.split(keyValueSplitter).toList.map(_.trim)).map(x => (x.head, x.tail.head)).toMap

  it should "create a new node from id, a property map and the class of the new node" in {
    val peter = Person_GQT_("Shaw", "Peter", 43, LocalDate.of(1970, 8, 23))
    peter.graphId = Some(42)
    val peter2 = newGranthiNodeByType[Person_GQT_](42,
      Map("lastname" -> "Shaw", "firstname" -> "Peter", "age" -> BigDecimal(43), "birthdate" -> "1970-08-23", "tag" -> "granthitest"))
    peter2 shouldBe peter
  }

  it should "create a new node from id, a empty property map and the class of the new node" in {
    val emptyTestNode = EmptyNode_GQT_()
    emptyTestNode.graphId = Some(7)
    val emptyTestNode2 = newGranthiNodeByType[EmptyNode_GQT_](42, Map.empty)
    emptyTestNode2 shouldBe emptyTestNode
    emptyTestNode.graphId = None
  }

  it should "create a new edge from id, a property map, a start node, an end node and the classes of the new edge and both nodes" in {
    val peter = Person_GQT_("Shaw", "Peter", 43, LocalDate.of(1970, 8, 23))
    val dana = Person_GQT_("Shaw", "Dana", 41, LocalDate.of(1972, 11, 12))
    val peterAndDana = IsMarriedWith_GQT_(peter, dana, LocalDate.of(1990, 9, 9), "rk")
    peterAndDana.graphId = Some(5)
    val peterAndDana2 = newGranthiEdgeByType[IsMarriedWith_GQT_, Person_GQT_, Person_GQT_](
      5, peter, dana, Map("since" -> "1990-09-09", "confession" -> "rk"), AnormCypherType)
    peterAndDana2 shouldBe peterAndDana
    peterAndDana.graphId = None
  }

  it should "create a new edge from id, an empty property map, a start node, an end node and the classes of the new edge and both nodes" in {
    val peter = Person_GQT_("Shaw", "Peter", 43, LocalDate.of(1970, 8, 23))
    val shawp = User_GQT_("shawp", new Date())
    val userOfPeter = HasUser_GQT_(peter, shawp)
    userOfPeter.graphId = Some(6)
    val userOfPeter2 = newGranthiEdgeByType[HasUser_GQT_, Person_GQT_, User_GQT_](6, peter, shawp, Map.empty, AnormCypherType)
    userOfPeter2 shouldBe userOfPeter
    userOfPeter.graphId = None
  }

  it should "create a new node with properties" in {
    val peter = newGranthiNodeByName(42, Map("lastname" -> "Shaw", "firstname" -> "Peter",
      "age" -> BigDecimal(43), "birthdate" -> "1970-08-23", "tag" -> "granthitest", "_granthiType_" -> "granthi.Person_GQT_"))
    peter shouldBe Person_GQT_("Shaw", "Peter", 43, LocalDate.of(1970, 8, 23))
  }

  it should "create a new empty node" in {
    val empty = newGranthiNodeByName(42, Map("_granthiType_" -> "granthi.EmptyNode_GQT_"))
    empty shouldBe EmptyNode_GQT_()
  }

  it should "create a granthi edge for two existing nodes" in {
    val peter = newGranthiNodeByName(1, Map("lastname" -> "Shaw", "firstname" -> "Peter",
      "age" -> BigDecimal(43), "birthdate" -> "1970-08-23", "tag" -> "granthitest", "_granthiType_" -> "granthi.Person_GQT_"))
    val dana = newGranthiNodeByName(2, Map("lastname" -> "Shaw", "firstname" -> "Dana",
      "age" -> BigDecimal(41), "birthdate" -> "1972-11-12", "tag" -> "granthitest", "_granthiType_" -> "granthi.Person_GQT_"))
    val peterAndDana = newGranthiEdgeByName(3, peter, dana, Map("since" -> "1990-09-09", "confession" -> "rk",
      "_granthiType_" -> "granthi.IsMarriedWith_GQT_"))
    peterAndDana shouldBe IsMarriedWith_GQT_(peter.asInstanceOf[Person_GQT_],dana.asInstanceOf[Person_GQT_], LocalDate.of(1990, 9, 9), "rk")
  }
}
