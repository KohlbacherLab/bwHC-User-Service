package de.bwhc.user.fs.repo


import java.io.File
import java.util.UUID.randomUUID

import scala.concurrent.{
  ExecutionContext,
  Future
}

import de.ekut.tbi.repo.AsyncRepository
import de.ekut.tbi.repo.fs.AsyncFSBackedInMemRepository

import de.bwhc.user.api.User

import de.bwhc.user.impl.{
  UserDB,
  UserDBProvider,
  UserWithPassword
}


class UserDBProviderImpl extends UserDBProvider
{
  def getInstance: UserDB = {

    val userDataDir =
      new File(Config.getInstance.dataDir,"")

    val userFsDb: AsyncRepository[UserWithPassword,User.Id] =
      AsyncFSBackedInMemRepository(
        userDataDir,
        "User",
        _.id,
        _.value
      )

    new UserDBImpl(userFsDb)
  }
}


class UserDBImpl
(
  private val db: AsyncRepository[UserWithPassword,User.Id]
)
extends UserDB
{
/*
  private val idPool =
    LazyList.continually(randomUUID.toString)
      .map(User.Id)
      .iterator
  def newId: User.Id = idPool.next
*/

  def newId: User.Id = User.Id(randomUUID.toString)


  def save(
    user: UserWithPassword
  )(
    implicit ec: ExecutionContext
  ): Future[UserWithPassword] = {
    db.save(user)
      .map(_ => user)
  }


  def isEmpty(
    implicit ec: ExecutionContext
  ): Future[Boolean] = 
    for {
      users <- db.query(_ => true)
    } yield users.isEmpty


  def filter(
    f: UserWithPassword => Boolean
  )(
    implicit ec: ExecutionContext
  ): Future[Iterable[UserWithPassword]] = {
    db.query(f)
  }


  def update(
    id: User.Id,
    up: UserWithPassword => UserWithPassword
  )(
    implicit ec: ExecutionContext
  ): Future[Option[UserWithPassword]] = {
    db.update(id,up)
  }

}
