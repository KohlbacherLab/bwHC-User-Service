package de.bwhc.user.auth.api



import play.api.libs.json.Json


object Role extends Enumeration
{

  type Role = Value

  val Admin, ZPMCoordinator, MTBCoordinator, Researcher = Value

  implicit val format = Json.formatEnum(this)

}
