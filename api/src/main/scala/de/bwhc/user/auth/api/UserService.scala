package de.bwhc.user.auth.api



import java.time.Instant

import scala.util.{Either,Try}

import scala.concurrent.{
  ExecutionContext,
  Future
}
import play.api.libs.json.Json
import cats.data.NonEmptyList

import de.bwhc.util.spi._
import de.bwhc.util.oauth.AccessToken



trait UserService
{

  def process(
    cmd: UserCommand
  )(
    implicit ec: ExecutionContext
  ): Future[Either[NonEmptyList[String],User]]

  def !(cmd: UserCommand)(implicit ec: ExecutionContext) = process(cmd)


  def get(
    id: User.Id
  )(
    implicit ec: ExecutionContext
  ): Future[Option[User]]



  def process(
    cmd: SessionCommand
  )(
    implicit ec: ExecutionContext
  ): Future[Either[NonEmptyList[String],SessionEvent]]

  def !(cmd: SessionCommand)(implicit ec: ExecutionContext) = process(cmd)


  def sessionFor(
    token: AccessToken
  )(
    implicit ec: ExecutionContext
  ): Future[Option[Session]]


}


trait UserServiceProvider extends SPI[UserService]

object UserService extends SPILoader(classOf[UserServiceProvider])
