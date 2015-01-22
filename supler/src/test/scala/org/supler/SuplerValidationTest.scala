package org.supler

import org.scalatest._
import org.supler.errors.{ ValidateInPath, ValidateNone, ValidateFilled, ValidateAll }
import Supler._

class SuplerValidationTest extends FlatSpec with ShouldMatchers {
  "field" should "validate required" in {
    // given
    case class Person(f1: String, f2: Option[String], f3: Int, f4: Option[Int])

    val p1 = Person("s1", Some("x1"), 10, Some(11))
    val p2 = Person("", None, 0, None)
    val p3 = Person(null, null, 12, null)

    // when
    object PersonMeta extends Supler[Person] {
      val f1Field = field(_.f1)
      val f2Field = field(_.f2)
      val f3Field = field(_.f3)
      val f3Field2 = field(_.f3).emptyValue(Some(10))
      val f4Field = field(_.f4)
    }

    // then
    import PersonMeta._

    f1Field.doValidate(EmptyPath, p1, ValidateAll).size should be(0)
    f1Field.doValidate(EmptyPath, p2, ValidateAll).size should be(1)
    f1Field.doValidate(EmptyPath, p3, ValidateAll).size should be(1)

    f2Field.doValidate(EmptyPath, p1, ValidateAll).size should be(0)
    f2Field.doValidate(EmptyPath, p2, ValidateAll).size should be(0)
    f2Field.doValidate(EmptyPath, p3, ValidateAll).size should be(0)

    f3Field.doValidate(EmptyPath, p1, ValidateAll).size should be(0)
    f3Field.doValidate(EmptyPath, p2, ValidateAll).size should be(1)
    f3Field.doValidate(EmptyPath, p3, ValidateAll).size should be(0)

    f3Field2.doValidate(EmptyPath, p1, ValidateAll).size should be(1)
    f3Field2.doValidate(EmptyPath, p2, ValidateAll).size should be(0)
    f3Field2.doValidate(EmptyPath, p3, ValidateAll).size should be(0)

    f4Field.doValidate(EmptyPath, p1, ValidateAll).size should be(0)
    f4Field.doValidate(EmptyPath, p2, ValidateAll).size should be(0)
    f4Field.doValidate(EmptyPath, p3, ValidateAll).size should be(0)
  }

  "field" should "not validate empty values if validating only filled" in {
    // given
    case class Person(f1: String)

    val p1 = Person("aaaa")
    val p2 = Person("aa")
    val p3 = Person("")
    val p4 = Person(null)

    // when
    object PersonMeta extends Supler[Person] {
      val f1Field = field(_.f1).validate(minLength(3))
    }

    // then
    import PersonMeta._

    f1Field.doValidate(EmptyPath, p1, ValidateAll).size should be(0)
    f1Field.doValidate(EmptyPath, p2, ValidateAll).size should be(1)
    f1Field.doValidate(EmptyPath, p3, ValidateAll).size should be(2)
    f1Field.doValidate(EmptyPath, p4, ValidateAll).size should be(1)

    f1Field.doValidate(EmptyPath, p1, ValidateFilled).size should be(0)
    f1Field.doValidate(EmptyPath, p2, ValidateFilled).size should be(1)
    f1Field.doValidate(EmptyPath, p3, ValidateFilled).size should be(0)
    f1Field.doValidate(EmptyPath, p4, ValidateFilled).size should be(0)
  }

  "form" should "validate the specified form fragment" in {
    // given
    case class Person(size: Int)
    case class City(name: String, people: List[Person])

    val personForm = form[Person](f => List(f.field(_.size).validate(gt(0))))
    val cityForm = form[City](f => List(
      f.field(_.name),
      f.subform(_.people, personForm)))

    val c1 = City("city1", List(Person(10), Person(20)))
    val c2 = City("city2", List(Person(-10)))
    val c3 = City("", List(Person(20), Person(-10), Person(0)))

    // when
    cityForm.doValidate(EmptyPath, c1, ValidateAll).size should be(0)
    cityForm.doValidate(EmptyPath, c2, ValidateAll).size should be(1)
    cityForm.doValidate(EmptyPath, c3, ValidateAll).size should be(4)

    cityForm.doValidate(EmptyPath, c1, ValidateFilled).size should be(0)
    cityForm.doValidate(EmptyPath, c2, ValidateFilled).size should be(1)
    cityForm.doValidate(EmptyPath, c3, ValidateFilled).size should be(1)

    cityForm.doValidate(EmptyPath, c1, ValidateNone).size should be(0)
    cityForm.doValidate(EmptyPath, c2, ValidateNone).size should be(0)
    cityForm.doValidate(EmptyPath, c3, ValidateNone).size should be(0)

    cityForm.doValidate(EmptyPath, c1, ValidateInPath(EmptyPath)).size should be(0)
    cityForm.doValidate(EmptyPath, c2, ValidateInPath(EmptyPath)).size should be(1)
    cityForm.doValidate(EmptyPath, c3, ValidateInPath(EmptyPath)).size should be(4)

    cityForm.doValidate(EmptyPath, c1, ValidateInPath(EmptyPath.append("people"))).size should be(0)
    cityForm.doValidate(EmptyPath, c2, ValidateInPath(EmptyPath.append("people"))).size should be(1)
    cityForm.doValidate(EmptyPath, c3, ValidateInPath(EmptyPath.append("people"))).size should be(3)

    cityForm.doValidate(EmptyPath, c1, ValidateInPath(EmptyPath.appendWithIndex("people", 1))).size should be(0)
    cityForm.doValidate(EmptyPath, c2, ValidateInPath(EmptyPath.appendWithIndex("people", 1))).size should be(0)
    cityForm.doValidate(EmptyPath, c3, ValidateInPath(EmptyPath.appendWithIndex("people", 1))).size should be(1)
  }
}
