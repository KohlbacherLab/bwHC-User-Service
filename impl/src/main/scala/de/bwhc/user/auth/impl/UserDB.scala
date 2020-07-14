package de.bwhc.user.auth.impl



import scala.concurrent.{
  ExecutionContext,
  Future
}

import de.bwhc.util.spi._

import de.bwhc.user.auth.api.User



trait UserDBProvider extends SPI[UserDB]

object UserDB extends SPILoader(classOf[UserDBProvider])


trait UserDB
{

  def newId: User.Id


  def save(
    user: UserWithPassword
  )(
    implicit ec: ExecutionContext
  ): Future[UserWithPassword]


  def filter(
    f: UserWithPassword => Boolean
  )(
    implicit ec: ExecutionContext
  ): Future[Iterable[UserWithPassword]]


  def find(
    f: UserWithPassword => Boolean
  )(
    implicit ec: ExecutionContext
  ): Future[Option[UserWithPassword]] =
    filter(f).map(_.headOption)


  def get(
    id: User.Id
  )(
    implicit ec: ExecutionContext
  ): Future[Option[UserWithPassword]] =
    find(_.id == id)


  def update(
    id: User.Id,
    up: UserWithPassword => UserWithPassword
  )(
    implicit ec: ExecutionContext
  ): Future[Option[UserWithPassword]]


}
