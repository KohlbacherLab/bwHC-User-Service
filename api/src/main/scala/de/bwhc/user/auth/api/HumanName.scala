package de.bwhc.user.auth.api



import play.api.libs.json.Json


object HumanName
{

  final case class Given(value: String) extends AnyVal
  final case class Family(value: String) extends AnyVal

  implicit val formatGiven     = Json.valueFormat[Given]
  implicit val formatFamily    = Json.valueFormat[Family]
  implicit val formatHumanName = Json.format[HumanName]

}


final case class HumanName
(
  given: HumanName.Given,
  family: HumanName.Family
)

