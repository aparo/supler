package org.supler

import java.util.Date

import org.json4s.JsValue
import org.json4s.JsonAST.{JsNumber, JsObject, JField}
import org.json4s.native.JsonMethods._
import org.scalatest._
import org.supler.Supler._
import org.supler.transformation.JsonTransformer

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
      JsObject(fields) <- jvalue
      JField("x", JsNumber(x)) <- fields
      JField("y", JsNumber(y)) <- fields
    } yield Point(x.toInt, y.toInt)).headOption

    override def toJValue(value: Point) = Some(
      JsObject(JField("x", JsNumber(value.x)), JField("y", JsNumber(value.y))))
  }

  "date transformer" should "add date hint by default" in {
    // given
    val dateObj = DateObj(new Date())

    // when
    val json = dateForm(dateObj).generateJSON

    // then
    compact(render(json)) should include (""""render_hint":{"name":"date"}""")
  }

  "complex object transformer" should "serialize value to a javascript object" in {
    // given
    val pointForm = form[PointObj](f => List(
      f.field(_.p)
    ))

    // when
    val json = pointForm(PointObj(Point(1, 2))).generateJSON

    // then
    compact(render(json)) should include (""""value":{"x":1,"y":2}""")
  }

  "complex object transformer" should "deserialize values from a javascript object" in {
    // given
    val pointForm = form[PointObj](f => List(
      f.field(_.p)
    ))

    val json = parse("""{"p": {"x":10,"y":20}}""")

    // when
    val result = pointForm(PointObj(Point(1, 2))).applyJSONValues(json)

    // then
    result.obj.p should be (Point(10, 20))
  }
}
