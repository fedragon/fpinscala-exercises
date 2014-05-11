package fpinscala

import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class UnitSpec extends FlatSpec with GeneratorDrivenPropertyChecks with Matchers
