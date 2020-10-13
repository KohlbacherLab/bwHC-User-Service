package de.bwhc.user.api



import play.api.libs.json.Json


object Role extends Enumeration
{
  type Role = Value

  val Admin,
      Documentarist,
      ZPMCoordinator,
      MTBCoordinator,
      Researcher = Value

  implicit val format = Json.formatEnum(this)

}
