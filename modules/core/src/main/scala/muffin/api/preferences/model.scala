package muffin.api.preferences

import muffin.predef.*

case class Preference(
  userId: UserId,
  category: String,
  name: String,
  value: String
)