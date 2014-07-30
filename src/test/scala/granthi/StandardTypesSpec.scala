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

import StandardTypes._
import org.scalatest._

import scala.reflect.runtime.universe.definitions._

class StandardTypesSpec extends FlatSpec with Matchers {

  "Standard Types" should "cast a String correctly" in {
    castValueToStandardType("String", StringClass.toType) match {
      case Some(x) => x shouldBe a [String]
      case None => fail()
    }
  }

  it should "not cast an Int into a String but throw a ClassCastException" in {
    intercept[ClassCastException] {
      castValueToStandardType(123, StringClass.toType)
    }
  }

  it should "cast BigDecimal to an Integer correctly" in {
    castValueToStandardType(BigDecimal(42: Int), IntTpe) match {
      case Some(x) => x shouldBe a[Integer] // I would expect a scala.Int but its java.lang.Integer
      case None => fail()
    }
  }

  it should "cast BigDecimal to a Long correctly" in {
    castValueToStandardType(BigDecimal(42:Long), LongTpe) match {
      case Some(x) => x shouldBe a [java.lang.Long] // I would expect a scala.Long but its java.lang.Long
      case None => fail()
    }
  }

  it should "cast a Boolean correctly" in {
    castValueToStandardType(true, BooleanTpe) match {
      case Some(x) => x shouldBe a[java.lang.Boolean] // I would expect a scala.Boolean but its java.lang.Boolean
      case None => fail()
    }
  }

  it should "cast a String to a Char correctly" in {
    castValueToStandardType("A", CharTpe) match {
      case Some(x) => x shouldBe a[Character] // I would expect a scala.Char but its java.lang.Character
      case None => fail()
    }
  }

  it should "not cast a String with length > 1 to Char" in {
    intercept[ClassCastException] {
      castValueToStandardType("Test", CharTpe)
    }
  }

  it should "add apostrophes to a text value" in {
    toCypherParam("A String") shouldBe "'A String'"
  }

  it should "not add apostrophes to a numeric value but convert it into a String" in {
    toCypherParam(123) shouldBe "123"
  }

  it should "not add apostrophes to a Boolean value but convert it into a String" in {
    toCypherParam(true) shouldBe "true"
  }
}
