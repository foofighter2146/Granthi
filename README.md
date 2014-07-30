##Granthi

Granthi is a Sanskrit word for node and the name for this Scala API that provides a way to define Neo4j database nodes and edges as case classes.
Granthi lets the programmer decide when he wants to interact with the database. Writing an modifying nodes and edges is possible by using add, 
update, merge or delete. A Scala object `AskNeo4jFor` give you method to load some nodes and edges from the database into the Granthi nodes
and edges. The graph properties must be either standard types or complex types that are wrapped a case class that extends the trait
`GranthiProperty`.

For the communication with the Neo4j database Granthi uses [AnormCypher](http://anormcypher.org). Hence you need to run a Neo4J server. Because it is
very easy to install an start a Neo4j server on localhost, there is no need for supporting the embedded version of Neo4j. The whole magic to 
interact between the Granthi nodes and edges and the regarding Neo4j elements is done by Scala 2.11 reflection (see 
http://docs.scala-lang.org/overviews/reflection/overview.html)

##Build 

To build (or run) Granthi, you need

- Java 8
- sbt 0.13.5
- Neo4j 2.1.2 server  

Maybe you wonder the decision to use Java 8. Java 8 is the current Java framework and provides a simple to use date/time API. If you want store
something in a database, date and time are often needed. It is not too difucult to change to Java 7. You have to edit the `build.sbt` file
and remove the case classes `LocalDateProperty`, `LocalTimeProperty` and `LocalDateTimeProperty`. Of course theses classes are used inside the
tests and therefore you must edit the test classes too, if you want to run the tests under Java 7.
   
##Usage

To define a domain of nodes and edges you my use case classes that extends the abstract classes `GranthiNode` or `GranthiEdge`:

``` Scala
case class Person(firstname: String, lastname: String, age: Int) extends GranthiNode[Person]

case class IsMarriedWith(person1: Person, person2: Person, since: LocalDateProperty) extends GranthiEdge[IsMarriedWith, Person, Person]
```

A field of a node or an edge must be either of type String, Char, Int, Short, Long, Byte, Double, Float, Boolean or a complex data type wrapped in a
GranthiProperty. It is possible to use not managed data types for data that should not stored in the database. In the following example,
a node of type User has a field 'sessionDate' which holds a date that is only relevant for this session.

``` Scala
case class User(name: String, sessionStart: java.util.Date) extends GranthiNode[User]
```

Currently, Granthi does not support Neo4j arrays, what should be added in a later version.

To interact with the Neo4j database there exist the modules `Neo4jNodes`, `Neo4jEdges`, and `AskNeo4jFor`. With the `Neo4jNodes` you can
perform adding, deleting, meging and updating nodes. Each function exists in a wording and a symbolic variant, and so you can choose your
favorite way to use these.

``` Scala
val peter = Person("Smith", "Peter", 42)

Neo4jNodes + peter  // adds a node to database
Neo4jNodes - peter  // deletes a node from database
Neo4jNodes ^ peter  // updates properties in the regarding Noe4j node
Neo4jNodes ~ peter  // merges, i.e. add our update a node
```

You can do the same with

``` Scala
val peter = Person("Smith", "Peter", 42)

Neo4jNodes.add(peter)     // adds a node to database
Neo4jNodes.delete(peter)  // deletes a node from database
Neo4jNodes.update(peter)  // updates properties in the regarding Noe4j node
Neo4jNodes.merge(peter)   // merges, i.e. add our update a node
```

`Neo4jEdges` provides the same operations for edges. 

With `Neo4jNodes` you can connect and disconnect two nodes by a kind of edge. If the edge type contain further fields than both node, you
have to specify these in a Map in the same order as they occur in the class constructor of the edge. Note that this not type safe and you
must take care to apply the right types and the valid order. If no properties expected, you may obmit the last parameter of connect. 

``` Scala
case class Knows(person1: Person, person2: Person) extends GranthiEdge[IsMarriedWith, Person, Person]

val peter = Person("Smith", "Peter", 42)
val dana = Person("Smith", "Dana", 42)

val peterAndDana = Neo4jNodes.connect(peter, dana, classOf[IsMarriedWith], Map("since" -> new LocalDateProperty(LocalDate.of(2002, 9, 9))))
val danaKnowsPeter = Neo4jNodes.connect(dana, peter, classOf[Knows])

Neo4jNodes.disconnect(dana, peter, classOf[Knows])
```

To get data from the database an fill Granthi nodes and edges you have to use the module `AskNeo4jFor`. As AnormCypher the requests result
into Streams of nodes or edges. If you ask for a edge, a direction value is required. Possible directions are Incoming, Outgoing and Both.

``` Scala
AskNeo4jFor.nodes[Person](Map("lastname" -> "Smith")
AskNeo4jFor.allNodes[Person] 
AskNeo4jFor.edges[IsMarriedWith](Map("since" -> "2002-09-09", Outgoing)
AskNeo4jFor.allEdges[Knows](Both)
AskNeo4jFor.allIncidentEdges(peter, Outgoing) // Get all outgoing edges for person Peter
AskNeo4jFor.incidentEdgesForRel(peter, Outgoing, classOf[IsMarriedWith]) // Get all outgoing edges of type "IsMarriedWith" for person Peter
AskNeo4jFor.allAdjacentNodes(peter, Outgoing) // Get all adjacent nodes for person Peter
AskNeo4jFor.adjacentNodesForRel(peter, Outgoing, classOf[IsMarriedWith]) // Get all persons Peter is married with (hopefully only one)
```

Granthi provides a handful of pre-defined property wrappers. To write your own you simply have to extend the `trait GranthiProperty`, implement
the converter method from the wrapped type to a standard type and a constructor for this standard type. Examples: 

``` Scala
case class BigDecimalProperty(bigDecimal: BigDecimal) extends GranthiProperty[String] {
  def this(bigDecimalAsString: String) = this(BigDecimal(bigDecimalAsString))
  def to = bigDecimal.toString()
}

case class DateProperty(date: Date) extends GranthiProperty[Long] {
  def this(dateAsLong: Long) = this(new Date(dateAsLong))
  def to = date.getTime
}
```

##Licence Apache 2.0

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.