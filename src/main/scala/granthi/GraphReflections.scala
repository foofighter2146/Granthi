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

import scala.reflect.ClassTag
import scala.reflect.runtime.universe._

/**
 * This object provides methods based on Scala reflection to access
 * fields of an graph element.
 */
object GraphReflections {

  /**
   * Gets all fields of an instance of a class of generic type T as
   * an Iterable of term symbols.
   *
   * @param typeTag Implicit Scala reflection type tag
   * @tparam T Generic Type T
   * @return Iterable of term symbols
   */
  private def getAllFields[T](implicit typeTag: TypeTag[T]): Iterable[TermSymbol] =
    typeTag.tpe.decls.filter(x => x.isTerm && !x.isMethod).map(_.asTerm)

  /**
   * Stores all fields of standard type or of graph property type of a graph element
   * into a property map. It converts all values of fields of graph property type
   * to values of standard type and ignores all fields with values of other types,
   * because these fields cannot be stored in database.
   *
   * @param element The graph element
   * @param typeTag Implicit Scala reflection type tag
   * @param classTag Implicit Scala reflection class tag
   * @tparam T Type of graph element
   * @return Property map
   */
   def fieldsToPropertyMap[T <: GranthiElement](element: T)(implicit typeTag: TypeTag[T], classTag: ClassTag[T]): Map[String, Any] =
    getAllFields
      .map(field => (field.name.toString.trim, runtimeMirror(element.getClass.getClassLoader).reflect(element).reflectField(field).get))
      .toMap
      .mapValues {
        case v: GranthiProperty[_] => v.to // Convert elements of graph property type to standard type
        case value => value
      }
      .filter(value => isStandardValue(value._2)) //Filter all elements that are not of standard type

  /**
   * Builds from a list of pairs of values and type information a list
   * that contains the values that are casted to their regarding types.
   * The casting results depend on the properties type GranthiType or AnormCypherType.
   *
   * @param valueWithTypeList List of pairs of values and their types
   * @param propertiesType Type of properties
   * @return List of values with Granthi types
   */
  def castTypes(valueWithTypeList : List[(Any, Type)], propertiesType: PropertiesType): List[Any] =
    propertiesType match {
      case AnormCypherType =>
        valueWithTypeList.map(valueWithType => castToGranthiType(valueWithType._1, valueWithType._2) match {
          case Some (resultValue) => resultValue
          case None => null
        })
      case GranthiType => valueWithTypeList.map(valueWithType => valueWithType._1)
    }

  /**
   * Casts a value got from an AnormCypher result to a Granthi value if possible.
   * Notice that AnormCypher only knows BigDecimal for numbers, String (for Char too),
   * and Boolean.
   * If the type of the Granthi graph element is a graph property, then it uses the
   * createFrom-method to convert from the standard type to the property.
   *
   * @param value The value
   * @param tpe The type of Granthi
   * @return Some value with Granthi type or None
   */
  private def castToGranthiType(value: Any, tpe: Type): Option[Any] =
    castValueToStandardType(value, tpe) match {
      case Some(v) => Some(v)
      case None => if (tpe <:< typeOf[GranthiProperty[_]]) toProperty(value, tpe) else None
    }

  /**
   * Converts a value of standard type into a property.
   *
   * @param value The value
   * @param tpe The standard type
   * @return Some suitable property or None
   */
  private def toProperty(value: Any, tpe: Type): Option[Any] = {
    // Properties have as least two constructor methods
    val constructorMethods = tpe.decl(termNames.CONSTRUCTOR).asTerm.alternatives.map(_.asMethod)
    // Find index in constructorMethods with standard type parameter
    val indexOfStdTypeCtor = constructorMethods.map(_.paramLists).flatten.flatten.map(_.typeSignature).map(x => isStandardType(x)).indexOf(true)
    // Find constructor the right constructor as method
    val constructorMethod = constructorMethods(indexOfStdTypeCtor)
    // Get constructor mirror
    val constructorMirror = runtimeMirror(getClass.getClassLoader).reflectClass(tpe.typeSymbol.asClass).reflectConstructor(constructorMethod)
    // Get the standard type with that the property data is stored in the database
    val standardType = tpe.baseType(symbolOf[GranthiProperty[_]]).typeArgs(0)
    // Create and return property depending on standard type
    castValueToStandardType(value, standardType) match {
      case Some(v) => Some(constructorMirror(v))
      case None => None
    }
  }

  /**
   * Prepares the constructor mirror and the list of parameters for the constructor
   * as pairs of parameter name and parameter type from a property map.
   *
   * @param propertyMap The property map as result from a Cypher query
   * @param typeTag Implicit Scala reflection type tag
   * @tparam T Type of graph element
   * @return Tuple that holds constructor mirror and the list of pairs of parameter name and type
   */
  def prepareGranthiElementCtorByType[T <: GranthiElement](propertyMap: Map[String, Any])(implicit typeTag: TypeTag[T]) = {
    // Find constructor of T as method
    val constructorMethod = typeOf[T].decl(termNames.CONSTRUCTOR).asMethod
    // Get list of parameters of the constructor as TermSymbols and drop the first and second parameter if it is an edge
    // (that are the nodes which are connected by this edge)
    val paramList = constructorMethod.paramLists.flatten.map(_.asTerm).drop(if (typeOf[T] <:< typeOf[GranthiNode[T]]) 0 else 2)
    // Get list of values from property map. If a value is not stored in database, it will be set to null.
    val paramValues = paramList.map(_.name.toString).map(n => if (propertyMap.contains(n)) propertyMap(n) else null)
    // Get list of types of parameter list
    val paramTypes = paramList.map(_.typeSignature)
    // Get list as zip of values und types
    val paramValueType = paramValues.zip(paramTypes)
    // Get constructor mirror
    val constructorMirror = runtimeMirror(getClass.getClassLoader).reflectClass(symbolOf[T].asClass).reflectConstructor(constructorMethod)
    // Return the Value
    (constructorMirror, paramValueType)
  }

  /**
   * Prepares the constructor mirror and the list of parameters for the constructor
   * as pairs of parameter name and parameter type from a property map.
   *
   * @param propertyMap The property map as result from a Cypher query
   * @return Tuple that holds constructor mirror and the list of pairs of parameter name and type
   */
  def prepareGranthiElementCtorByName(propertyMap: Map[String, Any])  = {
    // Get granthi type value or throw exception if it is not available
    val granthiType = propertyMap.getOrElse("_granthiType_",
      throw new GranthiException("Missing _granthiType_ property to find node or edge class")).asInstanceOf[String]
    // The runtime mirror
    val mirror = runtimeMirror(getClass.getClassLoader)
    // Class symbol representing the node or edge type
    val granthiElementClassSymbol = mirror.classSymbol(Class.forName(granthiType))
    // Type symbol representing the node or edge type
    val granthiElementTypeSymbol = granthiElementClassSymbol.toType
    // Find constructor of T as method
    val constructorMethod = granthiElementTypeSymbol.decl(termNames.CONSTRUCTOR).asMethod
    // Get list of parameters of the constructor as TermSymbols and drop the first and second parameter if it is an edge
    // (that are the nodes which are connected by this edge)
    val paramList = constructorMethod.paramLists.flatten.map(_.asTerm).drop(if (granthiElementTypeSymbol <:< typeOf[GranthiNode[_]]) 0 else 2)
    // Get list of values from property map. If a value is not stored in database, it will be set to null.
    val paramValues = paramList.map(_.name.toString).map(n => if (propertyMap.contains(n)) propertyMap(n) else null)
    // Get list of types of parameter list
    val paramTypes = paramList.map(_.typeSignature)
    // Get list as zip of values und types
    val paramValueType = paramValues.zip(paramTypes)
    // Get constructor mirror
    val constructorMirror = mirror.reflectClass(granthiElementClassSymbol).reflectConstructor(constructorMethod)
    // Return the result
    (constructorMirror, paramValueType)
  }
}
