package de.bwhc.user.api



import play.api.libs.json.Json


case class GivenName(value: String) extends AnyVal
object GivenName
{
  implicit val format = Json.valueFormat[GivenName]
}


case class FamilyName(value: String) extends AnyVal
object FamilyName
{
  implicit val format = Json.valueFormat[FamilyName]
}

