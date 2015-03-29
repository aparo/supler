package org.demo

import org.supler.{ Message, Supler }
import Supler._

object RegisterDataForm {
  form[RegisterData](f => List(
    f.field(_.login)
      .label("Login")
      .validate(minLength(3)),
    f.field(_.passwd)
      .label("Password:")
      .validate(minLength(8)),
    f.field(_.confirmPasswd).label("Confirm password:")
      .validate(custom((v, e) => v == e.passwd, (v, e) => Message("Passwords must match!")))))
}

class RegisterData(
  var login: String,
  var passwd: String,
  var confirmPasswd: String)
