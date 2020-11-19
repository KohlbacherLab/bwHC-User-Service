package de.bwhc.user.api



import java.time.{LocalDate,Instant}

import play.api.libs.json.Json

import de.bwhc.util.ddd.Event


object User
{

  final case class Id(value: String) extends AnyVal
  final case class Name(value: String) extends AnyVal
  final case class Password(value: String) extends AnyVal
  
  object Status extends Enumeration
  {
    val Active, Blocked, Deleted = Value

    implicit val format = Json.formatEnum(this)
  }

  implicit val formatId       = Json.valueFormat[Id] 
  implicit val formatName     = Json.valueFormat[Name] 
  implicit val formatPassword = Json.valueFormat[Password] 
  implicit val format         = Json.format[User]

}


final case class User
(
  id: User.Id,
  username: User.Name,
  givenName: GivenName, 
  familyName: FamilyName, 
  status: User.Status.Value,
  roles: Set[Role.Value],
  registeredOn: LocalDate,
  lastUpdate: Instant
)


sealed abstract class UserCommand
object UserCommand
{

  final case class Create
  (
    username:  User.Name,
    password:  User.Password,
    givenName: GivenName, 
    familyName: FamilyName, 
    roles:     Set[Role.Value]
  ) extends UserCommand

  final case class Update
  (
    id:        User.Id,
    givenName: Option[GivenName], 
    familyName: Option[FamilyName], 
    username:  Option[User.Name],
    password:  Option[User.Password],
  ) extends UserCommand

  final case class UpdateRoles
  (
    id:    User.Id,
    roles: Set[Role.Value]
  ) extends UserCommand

  final case class Delete
  (
    id: User.Id
  ) extends UserCommand


  implicit val formatCreate      = Json.format[Create]
  implicit val formatUpdate      = Json.format[Update]
  implicit val formatUpdateRoles = Json.format[UpdateRoles]
  implicit val formatDelete      = Json.format[Delete]

}


sealed abstract class UserEvent extends Event
object UserEvent
{

  final case class Created
  (
    user: User,
    timestamp: Instant = Instant.now
  ) extends UserEvent

  final case class Updated
  (
    user: User,
    timestamp: Instant = Instant.now
  ) extends UserEvent

  final case class Deleted
  (
    id: User.Id,
    timestamp: Instant = Instant.now
  ) extends UserEvent

  implicit val formatCreated = Json.format[Created]
  implicit val formatUpdated = Json.format[Updated]
  implicit val formatDeleted = Json.format[Deleted]

}
