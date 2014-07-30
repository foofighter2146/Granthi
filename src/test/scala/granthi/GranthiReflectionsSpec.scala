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

import granthi.GraphReflections._
import org.scalatest._

import scala.reflect.runtime.universe._

class GranthiReflectionsSpec extends FlatSpec with Matchers {

  "Granthi reflections" should "convert fields of standard type of a graph element into a property map" in {
    val cologneDome = Location_GQT_("Kölner Dom", 50.94129098, 6.958287, 58)
    val testCandidate = fieldsToPropertyMap(cologneDome)
    testCandidate shouldBe a [Map[String, Any]]
    testCandidate.get("name") shouldBe Some("Kölner Dom")
    testCandidate.get("lattitude") shouldBe Some(50.94129098)
    testCandidate.get("longitude") shouldBe Some(6.958287)
    testCandidate.get("seaHeight") shouldBe Some(58)
  }

  it should "convert a field of graph property type of a graph element into a suitable standard type" in {
    val peter = Person_GQT_("Shaw", "Peter", 43, new LocalDateProperty(LocalDate.of(1970, 8, 23)))
    fieldsToPropertyMap(peter).get("birthdate") shouldBe Some("1970-08-23")
  }

  it should "ignore fields of type that is neither standard nor graph property" in {
    val shawp = User_GQT_("shawp", new Date())
    fieldsToPropertyMap(shawp) shouldBe Map("name" -> "shawp", "tag" -> "granthitest")
  }

  it should "cast values with types as resulting from an AnormCypher query into suitable standard types" in {
    import definitions._
    val testCandidate = castTypes(
      List((BigDecimal(23), IntTpe), (BigDecimal(2e32.toLong), LongTpe), (BigDecimal(23: Short), ShortTpe),
        (BigDecimal(210.toByte: Byte), ByteTpe), (BigDecimal(32.1265), DoubleTpe), (BigDecimal(32.1265), FloatTpe),
        (false, BooleanTpe), ("A", CharTpe), ("Test String", StringClass.toType)), AnormCypherType)
    testCandidate shouldBe a[List[Any]]
    testCandidate shouldBe List(23: Int, 2e32.toLong: Long, 23: Short, 210.toByte: Byte, 32.1265:Double,
      32.1265.toFloat: Float, false, 'A', "Test String")
  }

  it should "cast values with types resulting from an AnormCypher query into suitable property types" in {
    castTypes(List(("2014-06-19", typeOf[LocalDateProperty]),(BigDecimal(1403203689685l), typeOf[DateProperty])), AnormCypherType) shouldBe
      List(LocalDateProperty(LocalDate.of(2014, 6, 19)), DateProperty(new Date(1403203689685l)))
  }

  it should "prepare the constructor mirror and the list of parameters for the constructor as pairs of parameter name and parameter type" in {
    val peter = Person_GQT_("Shaw", "Peter", 43, new LocalDateProperty(LocalDate.of(1970, 8, 23)))
    val (cm, paramValueType) = prepareGranthiElementCtorByType[Person_GQT_](fieldsToPropertyMap(peter))
    cm.symbol.toString shouldBe "constructor Person_GQT_"
    paramValueType.toString() shouldBe
      "List((Shaw,String), (Peter,String), (43,scala.Int), (1970-08-23,granthi.LocalDateProperty), (granthitest,String))"
  }

  it should "prepare constructor mirror and the list of parameters for the constructor as pairs of parameter name and parameter type" in {
    val (cm, paramValueType) = prepareGranthiElementCtorByName(Map("lastname" -> "Shaw", "firstname" -> "Peter",
      "age" -> 43, "birthdate"-> "1970-08-23", "tag" -> "granthitest", "_granthiType_" -> "granthi.Person_GQT_"))
    cm.symbol.toString shouldBe "constructor Person_GQT_"
    paramValueType.toString() shouldBe
      "List((Shaw,String), (Peter,String), (43,scala.Int), (1970-08-23,granthi.LocalDateProperty), (granthitest,String))"
  }

  it should "fail if properties map does not contain a _granthiType_ property" in {
    an [GranthiException] should be thrownBy  prepareGranthiElementCtorByName(Map("lastname" -> "Shaw", "firstname" -> "Peter",
      "age" -> 43, "birthdate"-> "1970-08-23", "tag" -> "granthitest"))
  }
}
