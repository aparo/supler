package org.supler.field

import org.json4s.JsonAST.{ JField, JInt, JBool }

abstract class RenderHint(val name: String) {
  def extraJSON: List[JField] = Nil
}

case object BasicFieldPasswordRenderHint extends RenderHint("password") with BasicFieldCompatible
case class BasicFieldTextareaRenderHint(rows: Option[Int], cols: Option[Int]) extends RenderHint("textarea") with BasicFieldCompatible {
  override def extraJSON = rows.map(r => JField("rows", JInt(r))).toList ++ cols.map(c => JField("cols", JInt(c))).toList
}

case object BasicFieldHiddenRenderHint extends RenderHint("hidden") with BasicFieldCompatible

case object BasicFieldDateRenderHint extends RenderHint("date") with BasicFieldCompatible

case object CheckBoxRenderHint extends RenderHint("checkbox") with BasicFieldCompatible with SelectManyFieldCompatible

case object SelectOneFieldRadioRenderHint extends RenderHint("radio") with SelectOneFieldCompatible
case object SelectOneFieldDropdownRenderHint extends RenderHint("dropdown") with SelectOneFieldCompatible

case class SubformTableRenderHint(collapsible: Boolean = true) extends RenderHint("table") with SubformFieldCompatible {
  override def extraJSON: List[JField] = List(JField("collapsible", JBool(collapsible)))
}
case class SubformListRenderHint(collapsible: Boolean = true) extends RenderHint("list") with SubformFieldCompatible {
  override def extraJSON: List[JField] = List(JField("collapsible", JBool(collapsible)))
}

case class CustomRenderHint(override val name: String, override val extraJSON: List[JField] = Nil) extends RenderHint(name)
  with BasicFieldCompatible with SelectOneFieldCompatible with SelectManyFieldCompatible with EditManyFieldCompatible

case object EditableListHint extends RenderHint("list") with EditManyFieldCompatible

case object IconRenderHint extends RenderHint("icon") with BasicFieldCompatible

trait RenderHints {
  def asList(collapsible: Boolean = true) = SubformListRenderHint(collapsible)
  def asTable(collapsible: Boolean = true) = SubformTableRenderHint(collapsible)

  def asPassword() = BasicFieldPasswordRenderHint
  def asTextarea(rows: Int = -1, cols: Int = -1) = {
    def toOption(d: Int) = if (d == -1) None else Some(d)
    BasicFieldTextareaRenderHint(toOption(rows), toOption(cols))
  }
  def asRadio() = SelectOneFieldRadioRenderHint
  def asDropdown() = SelectOneFieldDropdownRenderHint
  def asHidden() = BasicFieldHiddenRenderHint
  def asDate() = BasicFieldDateRenderHint

  def customRenderHint(name: String, extraJSON: JField*) = CustomRenderHint(name, extraJSON.toList)
}