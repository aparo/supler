package org.supler.field

import org.json4s.JsonAST.{ JField, JObject }
import org.json4s._
import org.supler._
import org.supler.transformation.FullTransformer

trait NonNestedFieldJSON[T, U] {
  this: Field[T] =>

  def valuesProvider: Option[ValuesProvider[T, U]]
  def label: Option[String]
  def transformer: FullTransformer[U, _]
  def renderHint: Option[RenderHint]
  def enabler: Option[T => Boolean]

  private[supler] override def generateJSON(parentPath: FieldPath, obj: T): List[JField] = {
    val data = valuesProvider match {
      case Some(vp) => generateJSONWithValuesProvider(obj, vp)
      case None     => generateJSONWithoutValuesProvider(obj)
    }

    import JSONFieldNames._

    val enabled = enabler match {
      case Some(enableFunc) =>
        enableFunc(obj)
      case None =>
        true
    }

    List(JField(name, JObject(List(
      JField(Label, JString(label.getOrElse(""))),
      JField(Type, JString(data.fieldTypeName)),
      JField(Validate, JObject(data.validationJSON.toList)),
      JField(Enabled, JBool(enabled)),
      JField(Path, JString(parentPath.append(name).toString))) ++ data.valueJSONValue.map(JField(Value, _)).toList
      ++ data.emptyValue.map(JField(EmptyValue, _)).toList
      ++ generateRenderHintJSONValue.map(JField(RenderHint, _)).toList
      ++ data.extraJSON)))
  }

  protected def generateJSONWithValuesProvider(obj: T, dp: ValuesProvider[T, U]): GenerateJSONData

  protected def generateJSONWithoutValuesProvider(obj: T): GenerateJSONData

  protected def generatePossibleValuesJSON(possibleValues: List[U]): List[JField] = {
    val possibleJValuesWithIndex = possibleValues.zipWithIndex
      .flatMap(t => transformer.serialize(t._1).map(jv => (jv, t._2)))
    val possibleJValues = possibleJValuesWithIndex.map {
      case (jvalue, index) =>
        JObject(JField("index", JInt(index)), JField("label", jvalue))
    }
    List(JField(JSONFieldNames.PossibleValues, JArray(possibleJValues)))
  }

  private def generateRenderHintJSONValue = renderHint.map(rh => JObject(
    JField("name", JString(rh.name)) :: rh.extraJSON))

  case class GenerateJSONData(
    fieldTypeName: String,
    valueJSONValue: Option[JValue],
    validationJSON: List[JField],
    emptyValue: Option[JValue],
    extraJSON: List[JField] = Nil)
}
