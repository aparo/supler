package org.supler

import java.util.Date

import org.scalatest._
import org.supler.Supler._
import org.supler.transformation.JsonTransformer
import play.api.libs.json._

class TransformerTest extends FlatSpec with ShouldMatchers {

  case class DateObj(date: Date)

  val dateForm = form[DateObj](f => List(
    f.field(_.date)
  ))

  case class Point(x: Int, y: Int)
  case class PointObj(p: Point)

  implicit val pointJsonTransformer: JsonTransformer[Point] = new JsonTransformer[Point] {
    override def typeName = "point"

    override def fromJValue(jvalue: JsValue) = (for {
      x <- (jvalue \ "x").asOpt[Int]
      y <- (jvalue \ "y").asOpt[Int]

    } yield Point(x.toInt, y.toInt))

    override def toJValue(value: Point) = Some(
      Json.obj("x" -> JsNumber(value.x), "y" -> JsNumber(value.y)))
  }

  "date transformer" should "add date hint by default" in {
    // given
    val dateObj = DateObj(new Date())

    // when
    val json = dateForm(dateObj).generateJSON

    // then
    Json.stringify(json) should include (""""render_hint":{"name":"date"}""")
  }

  "complex object transformer" should "serialize value to a javascript object" in {
    // given
    val pointForm = form[PointObj](f => List(
      f.field(_.p)
    ))

    // when
    val json = pointForm(PointObj(Point(1, 2))).generateJSON

    // then
    Json.stringify(json) should include (""""value":{"x":1,"y":2}""")
  }

  "complex object transformer" should "deserialize values from a javascript object" in {
    // given
    val pointForm = form[PointObj](f => List(
      f.field(_.p)
    ))

    val json = Json.parse("""{"p": {"x":10,"y":20}}""")

    // when
    val result = pointForm(PointObj(Point(1, 2))).applyJSONValues(json)

    // then
    result.obj.p should be (Point(10, 20))
  }
}
