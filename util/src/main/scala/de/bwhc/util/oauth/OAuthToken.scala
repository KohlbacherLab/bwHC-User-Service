package de.bwhc.util.oauth


import java.time.Instant

import play.api.libs.json.Json



final case class AccessToken(value: String) extends AnyVal
final case class RefreshToken(value: String) extends AnyVal

object TokenType extends Enumeration
{
  val Bearer = Value
}


final case class OAuthToken
(
  access_token: AccessToken,
  token_type: TokenType.Value,
  expires_in: Int,
  refresh_token: Option[RefreshToken],
  created_at: Long,
  scope: Option[String]
)
{
  def isExpired = (Instant.now isAfter Instant.ofEpochMilli(created_at + expires_in))
}

object OAuthToken
{
  implicit val formatAccessToken  = Json.valueFormat[AccessToken]
  implicit val formatRefreshToken = Json.valueFormat[RefreshToken]
  implicit val formatTokenType    = Json.formatEnum(TokenType)

  implicit val format = Json.format[OAuthToken]
}


/*
final case class OAuthToken
(
  access_token: String,
  token_type: String,
  expires_in: Int,
  refresh_token: Option[String],
  created_at: Long,
  scope: Option[String]
)
{
  def isExpired = (Instant.now isAfter Instant.ofEpochMilli(created_at + expires_in))
}

object OAuthToken
{

  implicit val format = Json.format[OAuthToken]
}
*/

