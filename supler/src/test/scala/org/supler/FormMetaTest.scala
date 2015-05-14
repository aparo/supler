package org.supler

import org.scalatest._
import play.api.libs.json._

class FormMetaTest extends FlatSpec with ShouldMatchers {
  "meta" should "serialize to json" in {
    // given
    val m = FormMeta(Map()) + ("tomek", "domek") + ("witek", "sprytek")

    // when
    val json = Json.stringify(JsObject(Seq(m.toJSON)))

    // then
    json should be("{\"supler_meta\":{\"tomek\":\"domek\",\"witek\":\"sprytek\"}}")
  }

  "meta" should "deserialize from json" in {
    // given
    val json =
      """
        |{
        |"supler_meta": {
        | "entityId": "123",
        | "power": "high"
        |},
        |"some_field": "foo",
        |"other_field": "bar"
        |}
      """.stripMargin
    val jsonParsed = Json.parse(json)

    // when
    val meta = FormMeta.fromJSON(jsonParsed)

    // then
    meta("entityId") should be ("123")
    meta("power") should be ("high")
  }
}
