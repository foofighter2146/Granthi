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

import scala.language.implicitConversions

import java.util.Date
import java.time.{LocalDateTime, LocalTime, LocalDate}

/**
 * A trait that is used to define properties to wrap
 * types that are not standard types.
 *
 * @tparam T Standard type that is used for conversion.
 */
trait GranthiProperty[T] {
  def to: T
}

case class DateProperty(date: Date) extends GranthiProperty[Long] {
  def this(dateAsLong: Long) = this(new Date(dateAsLong))
  def to = date.getTime
  override def toString = date.toString
}

case class LocalDateProperty(date: LocalDate) extends GranthiProperty[String] {
  def this(dateAsString: String) = this(LocalDate.parse(dateAsString))
  def to = date.toString
  override def toString = date.toString
}

case class LocalTimeProperty(time: LocalTime) extends GranthiProperty[String] {
  def this(timeAsString: String) = this(LocalTime.parse(timeAsString))
  def to = time.toString
  override def toString: String = time.toString
}

case class LocalDateTimeProperty(datetime: LocalDateTime) extends GranthiProperty[String] {
  def this(datetimeAsString: String) = this(LocalDateTime.parse(datetimeAsString))
  def to = datetime.toString
  override def toString: String = datetime.toString
}

case class BigDecimalProperty(bigDecimal: BigDecimal) extends GranthiProperty[String] {
  def this(bigDecimalAsString: String) = this(BigDecimal(bigDecimalAsString))
  def to = bigDecimal.toString()
  override def toString: String = bigDecimal.toString()
}

case class BigIntProperty(bigInt: BigInt) extends GranthiProperty[String] {
  def this(bigIntAsString: String) = this(BigInt(bigIntAsString))
  def to = bigInt.toString()
  override def toString: String = bigInt.toString()
}

/**
 * Defines some implicit wrappers for easier usage
 */
object GranthiProperties {
  implicit def wrapDate(date: Date) = new DateProperty(date)
  implicit def wrapLocalDate(localDate: LocalDate) = new LocalDateProperty(localDate)
  implicit def wrapLocalTime(localTime: LocalTime) = new LocalTimeProperty(localTime)
  implicit def wrapLocalDateTime(localDateTime: LocalDateTime) = new LocalDateTimeProperty(localDateTime)
  implicit def wrapBigDecimal(bigDecimal: BigDecimal) = new BigDecimalProperty(bigDecimal)
  implicit def wrapBigInt(bigInt: BigInt) = new BigIntProperty(bigInt)
}