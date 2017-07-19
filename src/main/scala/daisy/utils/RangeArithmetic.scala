

package daisy
package utils


trait RangeArithmetic[T] { self: T =>

  def unary_-(): T
  def +(other: T): T
  def -(other: T): T
  def *(other: T): T
  def *(r: Rational): T
  def /(other: T): T
  def squareRoot: T
  def inverse: T
  def toInterval: Interval

  /* Add an error */
  def +/-(r: Rational): T

}