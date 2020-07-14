package de.bwhc.util.oauth


import java.time.Instant

import play.api.libs.json.Json


final case class Username(value: String) extends AnyVal

final case class TokenInfo
(
  active: Boolean,
  username: Username,
  exp: Long,
  client_id: Option[String],
  scope: Option[String]
)

object TokenInfo
{
  implicit val formatUsername = Json.valueFormat[Username]

  implicit val format = Json.format[TokenInfo]
}

