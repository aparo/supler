package org.supler

import org.scalatest._
import org.supler.Supler._
import play.api.libs.json._

class FieldOrderTest extends FlatSpec with ShouldMatchers {
  case class OrderTestObj(field1: String, field2: String, field3: String)

  case class OrderTestParentClass(field1: String, obj: OrderTestObj, objList: List[OrderTestObj])

  val flatForm = form[OrderTestObj](f => List(
    f.field(_.field1),
    f.field(_.field2),
    f.field(_.field3)
  ))

  val nonFlatForm = form[OrderTestObj](f => List(
    f.field(_.field1) || f.field(_.field2),
    f.field(_.field3)
  ))

  "supler" should "generate flat order list for simple forms" in {
    // given
    val form = flatForm(o)
    // when
    val json = form.generateJSON
    // then
    val orderFields = json.fields.filter(f => f._1 == "fieldOrder")

    orderFields should have size 1
    orderFields(0)._2 should be( JsArray(List(
      JsArray(List(JsString("field1"))),
      JsArray(List(JsString("field2"))),
      JsArray(List(JsString("field3"))) )))
  }

  "supler" should "generate non flat order list for ordered simple forms" in {
    // given
    val form = nonFlatForm(o)
    // when
    val json = form.generateJSON
    // then
    val orderFields = json.fields.filter(f => f._1 == "fieldOrder")

    orderFields should have size 1
    orderFields(0)._2 should be( JsArray(List(
      JsArray(List(JsString("field1"), JsString("field2"))),
      JsArray(List(JsString("field3"))) )))
  }

  "supler" should "generate order for subform" in {
    // given
    val formWithSubform = form[OrderTestParentClass](f => List(
      f.field(_.field1),
      f.subform(_.obj, flatForm)
    ))
    val formInst = formWithSubform(OrderTestParentClass("hej", o, Nil))
    // when
    val json = formInst.generateJSON
    // then
    val orderFields = json.fields.filter(f => f._1 == "fieldOrder")

    orderFields should have size 2

    // first the subform
    orderFields(0)._2 should be( JsArray(List(
      JsArray(List(JsString("field1"))),
      JsArray(List(JsString("field2"))),
      JsArray(List(JsString("field3"))) )))

    // and the main form
    orderFields(1)._2 should be( JsArray(List(
      JsArray(List(JsString("field1"))),
      JsArray(List(JsString("obj"))) )))
  }

  "supler" should "generate order for list subform" in {
    // given
    val formWithSubform = form[OrderTestParentClass](f => List(
      f.field(_.field1),
      f.subform(_.objList, nonFlatForm)
    ))
    val formInst = formWithSubform(OrderTestParentClass("hej", o, List(o, o)))
    // when
    val json = formInst.generateJSON
    // then
    val orderFields = json.fields.filter(f => f._1 == "fieldOrder")

    orderFields should have size 3

    // first the 2 subforms
    orderFields(0)._2 should be( JsArray(List(
      JsArray(List(JsString("field1"), JsString("field2"))),
      JsArray(List(JsString("field3"))) )))
    orderFields(1)._2 should be( JsArray(List(
      JsArray(List(JsString("field1"), JsString("field2"))),
      JsArray(List(JsString("field3"))) )))

    // and the main form
    orderFields(2)._2 should be( JsArray(List(
      JsArray(List(JsString("field1"))),
      JsArray(List(JsString("objList"))) )))
  }

  private def o = OrderTestObj("this", "is", "sparta")
}
