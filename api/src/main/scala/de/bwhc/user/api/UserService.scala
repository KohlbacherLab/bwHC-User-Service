package de.bwhc.user.api



import java.time.Instant

import scala.util.{Either,Try}

import scala.concurrent.{
  ExecutionContext,
  Future
}
import play.api.libs.json.Json
import cats.data.NonEmptyList

import de.bwhc.util.spi._



trait UserServiceProvider extends SPI[UserService]

object UserService extends SPILoader[UserServiceProvider]


trait UserService
{

  type Or[+A,+B] = Either[A,B]

  type Errors = NonEmptyList[String]

  type ErrorsOr[+T] = Errors Or T


  def process(
    cmd: UserCommand
  )(
    implicit ec: ExecutionContext
  ): Future[ErrorsOr[UserEvent]]

  def !(cmd: UserCommand)(implicit ec: ExecutionContext) = process(cmd)


  def identify(
    username: User.Name,
    password: User.Password
  )(
    implicit ec: ExecutionContext
  ): Future[Option[User]]


  def getAll(
    implicit ec: ExecutionContext
  ): Future[Iterable[User]]


  def get(
    id: User.Id
  )(
    implicit ec: ExecutionContext
  ): Future[Option[User]]

}
