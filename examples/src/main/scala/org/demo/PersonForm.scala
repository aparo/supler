package org.demo

import java.util.UUID

import org.joda.time.DateTime
import org.joda.time.format.ISODateTimeFormat
import org.supler.Message
import org.supler.Supler._
import org.supler.field.ActionResult
import org.supler.transformation.StringTransformer

object PersonForm {
  val carMakesAndModels = Map(
    "Ford" -> List("Ka", "Focus", "Mondeo", "Transit"),
    "Toyota" -> List("Aygo", "Yaris", "Corolla", "Auris", "Verso", "Avensis", "Rav4"),
    "KIA" -> List("Picanto", "Venga", "cee'd", "sport c'eed", "Carens", "Sportage"),
    "Lada" -> List("Niva"))

  def carForm(deleteAction: Car => ActionResult[Car]) = form[Car](f => List(
    f.selectOneField(_.make)(identity).possibleValues(_ => carMakesAndModels.keys.toList).label("Make") ||
      f.selectOneField(_.model)(identity).possibleValues(car => carMakesAndModels.getOrElse(car.make, Nil)).label("Model"),
    f.field(_.year).validate(gt(1900)).label("Year"),
    f.action("delete")(c => { println(s"Running action: delete car $c"); deleteAction(c) }).label("Delete")))

  def legoSetForm(deleteAction: LegoSet => ActionResult[LegoSet]) = form[LegoSet](f => List(
    f.field(_.name).label("label_lego_name"),
    f.selectOneField(_.theme)(identity).possibleValues(_ => List("City", "Technic", "Duplo", "Space", "Friends", "Universal")).label("label_lego_theme"),
    f.field(_.number).label("label_lego_setnumber").validate(lt(100000)),
    f.field(_.age).label("label_lego_age").validate(ge(0), le(50)),
    f.action("delete")(deleteAction).label("Delete")))

  implicit val dateTimeTransformer = new StringTransformer[DateTime] {
    override def serialize(t: DateTime) = ISODateTimeFormat.date().print(t)

    override def deserialize(u: String) = try {
      Right(ISODateTimeFormat.date().parseDateTime(u))
    } catch {
      case e: IllegalArgumentException => Left("error_custom_illegalDateFormat")
    }
  }

  val personForm = form[Person](f => List(
    f.field(_.firstName).label("label_person_firstname") || f.field(_.lastName).label("label_person_lastname")
      .validate(custom((v, e) => v.length > e.firstName.length, (v, e) => Message("error_custom_lastNameLongerThanFirstName"))),
    f.field(_.age).label("Age") || f.field(_.birthday).label("Birthday").description("Please tell us, when where you born"),
    f.field(_.likesBroccoli).label("Likes broccoli"),
    f.field(_.address1).label("Address 1"),
    f.field(_.address2).label("Address 2"),
    f.selectManyField(_.favoriteColors)(identity).possibleValues(_ => List("red", "green", "blue", "magenta")).label("Favorite colors") ||
      f.selectOneField(_.gender)(identity).possibleValues(_ => List("Male", "Female")).label("Gender").renderHint(asRadio()) ||
      f.field(_.secret).label("Secret").renderHint(asPassword()),
    f.field(_.bio).label("Biography").renderHint(asTextarea(rows = 6)),
    f.subform(_.cars, carForm(f.parentAction((person, index, car) => ActionResult(deleteCar(person, car))))).label("Cars"),
    f.action("addcar")(p => ActionResult(p.copy(cars = p.cars :+ Car("", "", 0)))).label("Add car"),
    f.subform(_.legoSets, legoSetForm(f.parentAction((person, index, ls) => ActionResult(deleteLegoSet(person, ls))))).label("Lego sets").renderHint(asTable()),
    f.action("addlegoset")(p => ActionResult(p.copy(legoSets = p.legoSets :+ LegoSet("", "", 0, 0)))).label("Add lego set"),
    f.staticField(p => Message(p.registrationDate)).label("Registration date"),
    f.field(_.id).renderHint(asHidden()),
    f.field(_.a1) || f.field(_.a2) || f.field(_.a3) || f.field(_.a4).label("4th field") || f.field(_.a5)
      || f.field(_.a6) || f.field(_.a7) || f.field(_.a8) || f.field(_.a9) || f.field(_.a10) || f.field(_.a11)
      || f.field(_.a12) || f.field(_.a13) || f.field(_.a14) || f.field(_.a15) || f.field(_.a16)))

  def deleteCar(p: Person, c: Car): Person = p.copy(cars = p.cars diff List(c))
  def deleteLegoSet(p: Person, ls: LegoSet): Person = p.copy(legoSets = p.legoSets diff List(ls))

  case class Person(
    firstName: String,
    lastName: String,
    birthday: DateTime,
    age: Int,
    address1: Option[String],
    address2: Option[String],
    gender: String,
    secret: Option[String],
    bio: Option[String],
    favoriteColors: Set[String],
    likesBroccoli: Boolean,
    cars: List[Car],
    legoSets: List[LegoSet],
    registrationDate: DateTime,
    id: String,
    a1: String, a2: String, a3: String, a4: String, a5: String, a6: String, a7: String, a8: String, a9: String,
    a10: String, a11: String, a12: String, a13: String, a14: String, a15: String, a16: String)

  case class Car(
    make: String,
    model: String,
    year: Int)

  case class LegoSet(
    name: String,
    theme: String,
    number: Int,
    age: Int)

  val aPerson = Person("Adam", "", new DateTime(), 10, None, None, null, None, None,
    Set("red", "blue"), likesBroccoli = false,
    List(
      Car("Ford", "Focus", 1990),
      Car("Toyota", "Avensis", 2004)),
    List(
      LegoSet("Motorcycle", "Technic", 1924, 31),
      LegoSet("Arctic Supply Plane", "City", 60064, 1),
      LegoSet("Princess and Horse", "Duplo", 4825, 7)),
    new DateTime(2012, 2, 19, 0, 0), UUID.randomUUID().toString,
    "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p")
}