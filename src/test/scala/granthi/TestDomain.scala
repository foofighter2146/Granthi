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

import java.util.Date

sealed trait TestDomain

case class Location_GQT_(name: String, lattitude: Double, longitude: Double, seaHeight: Int, tag: String = "granthitest")
  extends GranthiNode[Location_GQT_] with TestDomain

case class Person_GQT_(var lastname: String, firstname: String, var age: Int, birthdate: LocalDateProperty, tag: String = "granthitest")
  extends GranthiNode[Person_GQT_] with TestDomain {
  override def toString = s"${super.toString}: $firstname $lastname, $age, born at $birthdate"
}

case class User_GQT_(name: String, sessionStart: Date, tag: String = "granthitest") extends GranthiNode[User_GQT_] with TestDomain

case class EmptyNode_GQT_() extends GranthiNode[EmptyNode_GQT_] with TestDomain

case class IsMarriedWith_GQT_(person1: Person_GQT_, person2: Person_GQT_, since: LocalDateProperty, var confession: String)
  extends GranthiEdge[IsMarriedWith_GQT_, Person_GQT_, Person_GQT_](person1, person2) with TestDomain

case class Knows_GQT_(person1: Person_GQT_, person2: Person_GQT_, var years: Int)
  extends GranthiEdge[Knows_GQT_, Person_GQT_, Person_GQT_](person1, person2) with TestDomain

case class HasUser_GQT_(person: Person_GQT_, user: User_GQT_)
  extends GranthiEdge[HasUser_GQT_, Person_GQT_, User_GQT_](person, user) with TestDomain
