package de.bwhc.user.auth.api



import java.time.Instant

import scala.util.Either

import scala.concurrent.{
  ExecutionContext,
  Future
}
import play.api.libs.json.Json
import cats.data.NonEmptyList

import de.bwhc.util.oauth._


sealed abstract class SessionCommand
object SessionCommand
{

  final case class Login
  (
    username: User.Name,
    password: User.Password
  ) extends SessionCommand

  final case class Logout
  (
    username: User.Name
  ) extends SessionCommand

  implicit val formatLogin  = Json.format[Login]
  implicit val formatLogout = Json.format[Logout]

}


final case class UserWithRoles
(
  username: User.Name,
  roles: Set[Role.Value] 
)
object UserWithRoles
{
  implicit val format = Json.format[UserWithRoles]
}


sealed abstract class SessionEvent
object SessionEvent
{

  final case class LoggedIn
  (
    user: UserWithRoles,
    token: OAuthToken,
  ) extends SessionEvent

  final case object LoggedOut extends SessionEvent

}


final case class Session
(
  token: OAuthToken,
  user: UserWithRoles,
  lastUpdate: Instant
)
{
  def id = token.access_token
}

object Session
{
  implicit val format = Json.format[Session]
}
