package de.bwhc.user.api



import play.api.libs.json.Json


object Role extends Enumeration
{
  type Role = Value

  val Admin                = Value
  val Documentarist        = Value
  val LocalZPMCoordinator  = Value
  val GlobalZPMCoordinator = Value
  val MTBCoordinator       = Value
  val Researcher           = Value

  implicit val format = Json.formatEnum(this)

}
