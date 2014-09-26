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

import org.anormcypher._
import org.scalatest._

/**
 * Use only one test spec für nodes and edges operations to avoid multithreading
 * mismatches.
 */
class Neo4jInteractionsSpec extends FlatSpec with Matchers with BeforeAndAfterAll {
  import GranthiProperties._

  override def beforeAll() = {
    Neo4jREST.setServer(scala.util.Properties.envOrElse("NEO4J_SERVER", "localhost"))
    Cypher("""
        create(peter:Person_GQT_ {lastname:"Shaw", firstname:"Peter", age:43, birthdate:"1970-08-23", tag:"granthitest"
               , _granthiType_:"granthi.Person_GQT_"}),
              (dana:Person_GQT_ {lastname:"Shaw", firstname:"Dana", age:41, birthdate:"1972-11-12", tag:"granthitest"
              , _granthiType_:"granthi.Person_GQT_"}),
              (kevin:Person_GQT_ {lastname:"Shaw", firstname:"Kevin", age:46, birthdate:"1967-02-02", tag:"granthitest"
              , _granthiType_:"granthi.Person_GQT_"}),
              (andrea:Person_GQT_ {lastname:"Shaw", firstname:"Andrea", age:39, birthdate:"1975-07-31", tag:"granthitest"
              , _granthiType_:"granthi.Person_GQT_"}),
              (donald:Person_GQT_ {lastname:"Duck", firstname:"Donald", age:94, birthdate:"1920-03-13"
              , _granthiType_:"granthi.Person_GQT_"}),
              (cologneDome:Location_GQT_ {name:"Dome of Cologne", lattitude:50.94129098, longitude:6.958287, seaHeight:58, tag:"granthitest"
              , _granthiType_:"granthi.Location_GQT_"}),
              (towerBridge:Location_GQT_ {name:"Tower Bridge", lattitude:51.5095, longitude:-0.087174, seaHeight:22, tag:"granthitest"
              , _granthiType_:"granthi.Location_GQT_"}),
              (shawp:User_GQT_ {name:"shwap", tag:"granthitest", _granthiType_:"granthi.User_GQT_"});
      """).execute()
    Cypher("MATCH (n:Person_GQT_), (m:Person_GQT_) WHERE n.firstname = 'Peter' AND m.firstname = 'Dana' " +
      "CREATE (n)-[r:IsMarriedWith_GQT_ {since: '2009-09-09', confession: 'ev', _granthiType_:'granthi.IsMarriedWith_GQT_'}]->(m)").execute()
    Cypher("MATCH (n:Person_GQT_), (m:Person_GQT_) WHERE n.firstname = 'Kevin' AND m.firstname = 'Andrea' " +
      "CREATE (n)-[r:IsMarriedWith_GQT_ {since: '2005-05-05', confession: 'ev', _granthiType_:'granthi.IsMarriedWith_GQT_'}]->(m)").execute()
    Cypher("MATCH (n:Person_GQT_), (m:Person_GQT_) WHERE n.firstname = 'Dana' AND m.firstname = 'Andrea' " +
      "CREATE (n)-[r:Knows_GQT_ {years: 13, _granthiType_:'granthi.Knows_GQT_'}]->(m)").execute()
  }

  override def afterAll() = {
    Cypher("MATCH (n) WHERE n.tag = 'granthitest' OPTIONAL MATCH (n)-[r]-() DELETE n, r")()
  }

  /* First test only nodes */

  "The Neo4j persistence API" should "init the graphId after adding a node to database" in {
    val bob = Person_GQT_("Builder", "Bob", 42, LocalDate.of(1972, 9, 12))
    bob.graphId shouldBe None
    Neo4jNodes + bob
    bob.graphId should not be None
    AskNeo4jFor.allNodes[Person_GQT_] should contain (bob)
  }

  it should "find all nodes with matching properties correctly" in {
    val res1 = AskNeo4jFor.nodes[Person_GQT_](Map("firstname" -> "Peter"))
    res1 should have size 1
    val peter = res1.head
    peter.firstname shouldBe "Peter"
    peter.lastname shouldBe "Shaw"
    peter.age shouldBe 43
    peter.birthdate shouldBe LocalDateProperty(LocalDate.of(1970, 8, 23))
    val res2 = AskNeo4jFor.nodes[Person_GQT_](Map("lastname" -> "Shaw"))
    res2 should have size 4
    res2.map(_.firstname) should contain allOf ("Peter", "Dana", "Kevin", "Andrea")
  }

  it should "find all expected nodes of a certain type" in {
    AskNeo4jFor.allNodes[Location_GQT_] should have size 2
    val cologneDome = AskNeo4jFor.nodes[Location_GQT_](Map("name" -> "Dome of Cologne")).head
    val towerBridge = AskNeo4jFor.nodes[Location_GQT_](Map("name" -> "Tower Bridge")).head
    AskNeo4jFor.allNodes[Location_GQT_] should contain allOf (cologneDome, towerBridge)
    val peter = AskNeo4jFor.nodes[Person_GQT_](Map("lastname" -> "Shaw", "firstname" -> "Peter")).head
    val dana = AskNeo4jFor.nodes[Person_GQT_](Map("lastname" -> "Shaw", "firstname" -> "Dana")).head
    val kevin = AskNeo4jFor.nodes[Person_GQT_](Map("lastname" -> "Shaw", "firstname" -> "Kevin")).head
    AskNeo4jFor.allNodes[Person_GQT_] should contain allOf (peter, dana, kevin)
  }

  it should "fail adding a node with existing graphId in database" in {
    val peter = AskNeo4jFor.nodes[Person_GQT_](Map("lastname" -> "Shaw", "firstname" -> "Peter")).head
    peter.graphId should not be None
    an [GranthiException] should be thrownBy Neo4jNodes.add(peter)
  }

  it should "update a parameter for a node in database" in {
    val dana = AskNeo4jFor.nodes[Person_GQT_](Map("lastname" -> "Shaw", "firstname" -> "Dana")).head
    val ageBuffer = dana.age
    dana.age = 42
    AskNeo4jFor.nodes[Person_GQT_](Map("firstname" -> "Dana")).head.age shouldBe 41
    Neo4jNodes ^ dana
    AskNeo4jFor.nodes[Person_GQT_](Map("firstname" -> "Dana")).head.age shouldBe 42
    dana.age = ageBuffer
    Neo4jNodes ^ dana
  }

  it should "fail to update a node that is not persistent in database" in {
    an [GranthiException] should be thrownBy Neo4jNodes.update(Location_GQT_("Test", 1, 2, 3))
  }

  it should "decide to add a node to database if its graphId is None when merge is called" in {
    val micky = Person_GQT_("Mouse", "Micky", 86, LocalDate.of(1928, 11, 18))
    micky.graphId shouldBe None
    Neo4jNodes ~ micky
    micky.graphId should not be None
    AskNeo4jFor.allNodes[Person_GQT_] should contain (micky)
  }

  it should "decide to update a node in database if its graphId is Some id when merge is called" in {
    val peter = AskNeo4jFor.nodes[Person_GQT_](Map("lastname" -> "Shaw", "firstname" -> "Peter")).head
    val ageBuffer = peter.age
    peter.age = 36
    Neo4jNodes.merge(peter)
    val result =AskNeo4jFor.nodes[Person_GQT_](Map("firstname" -> "Peter", "lastname" -> "Shaw"))
    result should have size 1
    result.head.age should equal(36)
    peter.age = ageBuffer
    Neo4jNodes.update(peter)
  }

  it should "delete a persistent node from database" in {
    val donald = AskNeo4jFor.nodes[Person_GQT_](Map("lastname" -> "Duck", "firstname" -> "Donald")).head
    Neo4jNodes - donald
    AskNeo4jFor.allNodes[Person_GQT_] should not contain donald
  }

  it should "not delete a not persistent node from database" in {
    val daisy = Person_GQT_("Duck", "Daisy", 94,LocalDate.of(1920, 1, 9))
    AskNeo4jFor.allNodes[Person_GQT_] should not contain daisy
    an[GranthiException] should be thrownBy (Neo4jNodes - daisy)
  }

  it should "provide operations for a valid life cycle for nodes without properties" in {
    AskNeo4jFor.allNodes[EmptyNode_GQT_] shouldBe empty
    val emptyNode = EmptyNode_GQT_()
    Neo4jNodes + emptyNode
    AskNeo4jFor.allNodes[EmptyNode_GQT_] should have size 1
    Neo4jNodes ^ emptyNode
    AskNeo4jFor.allNodes[EmptyNode_GQT_] should have size 1
    Neo4jNodes ~ emptyNode
    AskNeo4jFor.allNodes[EmptyNode_GQT_] should have size 1
    Neo4jNodes - emptyNode
    AskNeo4jFor.allNodes[EmptyNode_GQT_] shouldBe empty
  }

  it should "find nothing if properties map contains a unknown property that does not exists for nodes in database" in {
    val res = AskNeo4jFor.nodes[Person_GQT_](Map("lastname" -> "Shaw", "foo" -> "bar"))
    res shouldBe empty
  }

  // Now nodes are save, test edges

  it should "init the graphId after adding an edge to database" in {
    val peter = AskNeo4jFor.nodes[Person_GQT_](Map("lastname" -> "Shaw", "firstname" -> "Peter")).head
    val kevin = AskNeo4jFor.nodes[Person_GQT_](Map("lastname" -> "Shaw", "firstname" -> "Kevin")).head
    val brothers = Knows_GQT_(peter, kevin, 43)
    brothers.graphId shouldBe None
    Neo4jEdges + brothers
    brothers.graphId should not be None
  }

  it should "fail adding an edge with existing graphId in database" in {
    val peterAndDana = AskNeo4jFor.allEdges[IsMarriedWith_GQT_, Person_GQT_](Outgoing).head
    an [GranthiException] should be thrownBy Neo4jEdges.add(peterAndDana)
  }

  it should "fail adding an edge if its start node does not exists in database" in {
    val manni = Person_GQT_("Shaw", "Manni", 65, LocalDate.of(1949, 1, 15))
    val peter =  AskNeo4jFor.nodes[Person_GQT_](Map("lastname" -> "Shaw", "firstname" -> "Peter")).head
    val daddyKnowsSon = Knows_GQT_(manni, peter, 43)
    an [GranthiException] should be thrownBy Neo4jEdges.add(daddyKnowsSon)
  }

  it should "fail adding an edge if its end node does not exists in database" in {
    val peter =  AskNeo4jFor.nodes[Person_GQT_](Map("lastname" -> "Shaw", "firstname" -> "Peter")).head
    val manni = Person_GQT_("Shaw", "Manni", 65, LocalDate.of(1949, 1, 15))
    val sonKnowsDaddy = Knows_GQT_(peter, manni, 40)
    an [GranthiException] should be thrownBy Neo4jEdges.add(sonKnowsDaddy)
  }

  it should "update a property of an edge in database" in {
    val peterAndDana = AskNeo4jFor.allEdges[IsMarriedWith_GQT_, Person_GQT_](Outgoing).head
    val confessionBuffer = peterAndDana.confession
    confessionBuffer shouldBe "ev"
    peterAndDana.confession = "rk"
    Neo4jEdges ^ peterAndDana
    AskNeo4jFor.allEdges[IsMarriedWith_GQT_, Person_GQT_](Outgoing).head.confession shouldBe "rk"
    peterAndDana.confession = confessionBuffer
    Neo4jEdges ^ peterAndDana
    AskNeo4jFor.allEdges[IsMarriedWith_GQT_, Person_GQT_](Outgoing).head.confession shouldBe "ev"
  }

  it should "fail to update an edge that is not persistent in database" in {
    val peter = AskNeo4jFor.nodes[Person_GQT_](Map("lastname" -> "Shaw", "firstname" -> "Peter")).head
    val kevin = AskNeo4jFor.nodes[Person_GQT_](Map("lastname" -> "Shaw", "firstname" -> "Kevin")).head
    val brothers = Knows_GQT_(peter, kevin, 43)
    an [GranthiException] should be thrownBy Neo4jEdges.update(brothers)
  }

  it should "decide to add an edge to database if its graphId is None when merge is called" in {
    val kevin = AskNeo4jFor.nodes[Person_GQT_](Map("lastname" -> "Shaw", "firstname" -> "Kevin")).head
    val dana = AskNeo4jFor.nodes[Person_GQT_](Map("lastname" -> "Shaw", "firstname" -> "Dana")).head
    val kevinKnowsDana = Knows_GQT_(kevin, dana, 15)
    kevinKnowsDana.graphId shouldBe None
    Neo4jEdges ~ kevinKnowsDana
    kevinKnowsDana.graphId should not be None
    AskNeo4jFor.allEdges[Knows_GQT_, Person_GQT_](Outgoing) should contain (kevinKnowsDana)
  }

  it should "decide to update an edge in database if its graphId is some id when merge is called" in {
    val peterAndDana = AskNeo4jFor.allEdges[IsMarriedWith_GQT_, Person_GQT_](Outgoing).head
    peterAndDana.graphId should not be None
    Neo4jEdges ~ peterAndDana
    val marriedEdges = AskNeo4jFor.allEdges[IsMarriedWith_GQT_, Person_GQT_](Outgoing)
    marriedEdges should contain (peterAndDana)
  }

  it should "delete a persistent edge from database" in {
    val danaKnowsAndrea = AskNeo4jFor.edges[Knows_GQT_, Person_GQT_](Map("years" -> 13), Outgoing).head
    AskNeo4jFor.allEdges[Knows_GQT_, Person_GQT_](Outgoing) should contain (danaKnowsAndrea)
    Neo4jEdges - danaKnowsAndrea
    AskNeo4jFor.allEdges[Knows_GQT_, Person_GQT_](Outgoing) should not contain danaKnowsAndrea
  }

  it should "not delete a not persistent edge from database" in {
    val peter = AskNeo4jFor.nodes[Person_GQT_](Map("lastname" -> "Shaw", "firstname" -> "Peter")).head
    val kevin = AskNeo4jFor.nodes[Person_GQT_](Map("lastname" -> "Shaw", "firstname" -> "Kevin")).head
    val brothers = Knows_GQT_(peter, kevin, 43)
    an[GranthiException] should be thrownBy Neo4jEdges.delete(brothers)
  }

  it should "find different numbers matches depending on direction" in {
    AskNeo4jFor.allEdges[IsMarriedWith_GQT_, Person_GQT_](Outgoing) should have size 2
    AskNeo4jFor.allEdges[IsMarriedWith_GQT_, Person_GQT_](Incoming) should have size 2
    AskNeo4jFor.allEdges[IsMarriedWith_GQT_, Person_GQT_](Both) should have size 4
  }

  it should "find all incident edges for a node" in {
    val peter = AskNeo4jFor.nodes[Person_GQT_](Map("lastname" -> "Shaw", "firstname" -> "Peter")).head
    val andrea = AskNeo4jFor.nodes[Person_GQT_](Map("lastname" -> "Shaw", "firstname" -> "Andrea")).head
    val peterKnowsAndrea = Knows_GQT_(peter, andrea, 10)
    Neo4jEdges + peterKnowsAndrea
    val peterAndDana = AskNeo4jFor.allEdges[IsMarriedWith_GQT_, Person_GQT_](Outgoing).head
    val incEdges = AskNeo4jFor.allIncidentEdges(peter, Outgoing)
    incEdges should contain allOf (peterAndDana, peterKnowsAndrea)
  }

  it should "find all incident edges for a node for a certain relation" in {
    val peter = AskNeo4jFor.nodes[Person_GQT_](Map("lastname" -> "Shaw", "firstname" -> "Peter")).head
    val dana = AskNeo4jFor.nodes[Person_GQT_](Map("lastname" -> "Shaw", "firstname" -> "Dana")).head
    val peterAndDana = AskNeo4jFor.allEdges[IsMarriedWith_GQT_, Person_GQT_](Outgoing).head
    val adjNodes =  AskNeo4jFor.incidentEdgesForRel(peter, Outgoing, classOf[IsMarriedWith_GQT_])
    adjNodes should have size 1
    adjNodes should contain (peterAndDana)
  }

  it should "find all adjacent nodes for a node" in {
    val peter = AskNeo4jFor.nodes[Person_GQT_](Map("lastname" -> "Shaw", "firstname" -> "Peter")).head
    val dana = AskNeo4jFor.nodes[Person_GQT_](Map("lastname" -> "Shaw", "firstname" -> "Dana")).head
    val adjNodes =  AskNeo4jFor.allAdjacentNodes(peter, Outgoing)
    adjNodes should contain (dana)
    adjNodes should not contain peter
  }

  it should "find all adjacent nodes for a node for a certain relation" in {
    val peter = AskNeo4jFor.nodes[Person_GQT_](Map("lastname" -> "Shaw", "firstname" -> "Peter")).head
    val dana = AskNeo4jFor.nodes[Person_GQT_](Map("lastname" -> "Shaw", "firstname" -> "Dana")).head
    val adjNodes =  AskNeo4jFor.adjacentNodesForRel(peter, Outgoing, classOf[IsMarriedWith_GQT_])
    adjNodes should have size 1
    adjNodes should contain (dana)
  }

  it should "connect two nodes with a new edge and suitable properties" in {
    val peter = AskNeo4jFor.nodes[Person_GQT_](Map("lastname" -> "Shaw", "firstname" -> "Peter")).head
    val paul = Person_GQT_("Parker", "Paul", 51, LocalDate.of(1961, 12, 25))
    val peterKnowsPaul = Neo4jNodes.connect(peter, paul, classOf[Knows_GQT_], Map("years" -> 35))
    AskNeo4jFor.allEdges[Knows_GQT_, Person_GQT_](Outgoing) should contain (peterKnowsPaul)
  }

  it should "connect two nodes with a new edge and no properties" in {
    val kevin = AskNeo4jFor.nodes[Person_GQT_](Map("lastname" -> "Shaw", "firstname" -> "Kevin")).head
    val kevinUser = User_GQT_("kevin123", new Date())
    val kevinHasUser = Neo4jNodes.connect(kevin, kevinUser, classOf[HasUser_GQT_])
    val result = AskNeo4jFor.allEdges[HasUser_GQT_, Person_GQT_, User_GQT_](Outgoing)
    result should have size 1
    val resultHead = result.head
    resultHead.graphId shouldBe kevinHasUser.graphId
    resultHead.person shouldBe kevinHasUser.person
    resultHead.user.name shouldBe kevinHasUser.user.name
  }

  it should "fail connection two nodes with an edge and wrong properties" in {
    val peter = AskNeo4jFor.nodes[Person_GQT_](Map("lastname" -> "Shaw", "firstname" -> "Peter")).head
    val dana = AskNeo4jFor.nodes[Person_GQT_](Map("lastname" -> "Shaw", "firstname" -> "Dana")).head
    an [GranthiException] should be thrownBy Neo4jNodes.connect(peter, dana, classOf[Knows_GQT_])
  }

  it should "disconnect two nodes" in {
    val peter = AskNeo4jFor.nodes[Person_GQT_](Map("lastname" -> "Shaw", "firstname" -> "Peter")).head
    val dana = AskNeo4jFor.nodes[Person_GQT_](Map("lastname" -> "Shaw", "firstname" -> "Dana")).head
    val fred = Person_GQT_("Flintstone", "Fred", 76, LocalDate.of(1938, 9, 10))
    Neo4jNodes + fred
    val peterKnowsDana = Neo4jNodes.connect(peter, dana, classOf[Knows_GQT_], Map("years" -> 23))
    val peterKnowsFred = Neo4jNodes.connect(peter, fred, classOf[Knows_GQT_], Map("years" -> 42))
    AskNeo4jFor.allIncidentEdges(peter,Outgoing) should contain (peterKnowsDana)
    Neo4jNodes.disconnect(peter, dana, classOf[Knows_GQT_])
    val incEdges = AskNeo4jFor.allIncidentEdges(peter,Outgoing)
    incEdges should not contain peterKnowsDana
    incEdges should contain (peterKnowsFred)

  }
}
