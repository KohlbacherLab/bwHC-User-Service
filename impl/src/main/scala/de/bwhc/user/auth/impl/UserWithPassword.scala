package de.bwhc.user.auth.impl


import java.time.{Instant,LocalDate}

import play.api.libs.json.Json

import de.bwhc.user.auth.api._


final case class UserWithPassword
(
  id: User.Id,
  name: User.Name,
  password: User.Password,
  humanName: HumanName,
  status: User.Status.Value,
  roles: Set[Role.Value],
  registeredOn: LocalDate,
  lastUpdate: Instant
)
{
  def toUser =
    User(id,name,humanName,status,roles,registeredOn,lastUpdate)
}

object UserWithPassword
{
  implicit val format = Json.format[UserWithPassword]
}
