package org.supler.validation

import org.json4s.JsonAST.{ JArray, JString, JField, JObject }
import org.supler.{ FieldPath, Message }
import org.supler.field.Field

case class FieldErrorMessage(field: Field[_], path: FieldPath, message: Message) {
  def generateJSON = {
    JObject(
      JField("field_path", JString(path.toString)),
      JField("error_key", JString(message.key)),
      JField("error_params", JArray(message.params.map(p => JString(p.toString)).toList)))
  }
}
