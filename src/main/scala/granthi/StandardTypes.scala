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
 *  Standard Scala types that are mapped directly to properties of
 *  a Neo4J data base. Currently that are: String, Char, Boolean,
 *  Int, Long, Short, Byte, Double and Float.
 */
object StandardTypes {

  def isStandardValue(value: Any): Boolean = isNumericValue(value) || isTextValue(value) || isBooleanValue(value)

  def isNumericValue(value: Any): Boolean =
    isIntValue(value) || isLongValue(value) || isShortValue(value) ||
      isByteValue(value) || isDoubleValue(value) || isFloatValue(value)

  def isTextValue(value: Any): Boolean = isStringValue(value) || isCharValue(value)

  def isStringValue(value: Any): Boolean = value.isInstanceOf[String]
  def isCharValue(value: Any): Boolean = value.isInstanceOf[Char]
  def isBooleanValue(value: Any): Boolean = value.isInstanceOf[Boolean]
  def isIntValue(value: Any): Boolean = value.isInstanceOf[Int]
  def isLongValue(value: Any): Boolean = value.isInstanceOf[Long]
  def isShortValue(value: Any): Boolean = value.isInstanceOf[Short]
  def isByteValue(value: Any): Boolean = value.isInstanceOf[Byte]
  def isDoubleValue(value: Any): Boolean = value.isInstanceOf[Double]
  def isFloatValue(value: Any): Boolean = value.isInstanceOf[Float]

  def toInt(value: Any): Int = value.asInstanceOf[BigDecimal].toInt
  def toLong(value: Any): Long = value.asInstanceOf[BigDecimal].toLong
  def toShort(value: Any): Short = value.asInstanceOf[BigDecimal].toShort
  def toByte(value: Any): Byte = value.asInstanceOf[BigDecimal].toByte
  def toDouble(value: Any): Double = value.asInstanceOf[BigDecimal].toDouble
  def toFloat(value: Any): Float = value.asInstanceOf[BigDecimal].toFloat
  def toBoolean(value: Any): Boolean = value.asInstanceOf[Boolean]
  def toStr(value: Any): String = value.asInstanceOf[String]
  def toChar(value: Any): Char = {
    val s = value.asInstanceOf[String]
    s.length match {
      case 1 => s.charAt(0)
      case _ => throw new ClassCastException("Unable to cast from a String with other length than 1 to Char")
    }
  }


  def isStandardType(tpe: Type): Boolean = isNumericType(tpe) || isTextType(tpe) || isBooleanType(tpe)

  def isNumericType(tpe: Type): Boolean =
    isIntType(tpe) || isLongType(tpe) || isShortType(tpe) ||
      isByteType(tpe) || isDoubleType(tpe) || isFloatType(tpe)

  def isTextType(tpe: Type): Boolean = isStringType(tpe) || isCharType(tpe)

  import definitions._
  def isStringType(tpe: Type): Boolean = tpe =:= StringClass.toType
  def isCharType(tpe: Type): Boolean = tpe =:= CharTpe
  def isBooleanType(tpe: Type): Boolean = tpe =:= BooleanTpe
  def isIntType(tpe: Type): Boolean = tpe =:= IntTpe
  def isLongType(tpe: Type): Boolean = tpe =:= LongTpe
  def isShortType(tpe: Type): Boolean = tpe =:= ShortTpe
  def isByteType(tpe: Type): Boolean = tpe =:= ByteTpe
  def isDoubleType(tpe: Type): Boolean = tpe =:= DoubleTpe
  def isFloatType(tpe: Type): Boolean = tpe =:= FloatTpe

  /**
   * Casts a value of unknown type to some suitable standard type if possible.
   *
   * @param value The value of unknown type
   * @param tpe The standard type
   * @return Some casted value or None
   */
  def castValueToStandardType(value: Any, tpe: Type): Option[Any] =
    if (isIntType(tpe)) Some(toInt(value))
    else if (isLongType(tpe)) Some(toLong(value))
    else if (isShortType(tpe)) Some(toShort(value))
    else if (isByteType(tpe)) Some(toByte(value))
    else if (isDoubleType(tpe)) Some(toDouble(value))
    else if (isFloatType(tpe)) Some(toFloat(value))
    else if (isBooleanType(tpe)) Some(toBoolean(value))
    else if (isCharType(tpe)) Some(toChar(value))
    else if (isStringType(tpe)) Some(toStr(value))
    else None

  /**
   * Converts a value into a Cypher parameter. If it is a text type then
   * add apostrophes before and behind.
   *
   * @param value The standard value
   * @return A string representing the Cypher parameter
   */
  def toCypherParam(value: Any): String = {
    def apostropheIfNecessary(value: Any): String = if (isTextValue(value)) "'" else ""
    s"${apostropheIfNecessary(value)}${value.toString}${apostropheIfNecessary(value)}"
  } 
}
