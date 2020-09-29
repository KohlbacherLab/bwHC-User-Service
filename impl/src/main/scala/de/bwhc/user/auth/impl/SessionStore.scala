package de.bwhc.user.auth.impl



import java.time.Instant

import scala.concurrent.{
  ExecutionContext,
  Future
}

import de.bwhc.util.Logging
import de.bwhc.util.spi._
import de.bwhc.util.oauth.AccessToken

import de.bwhc.user.auth.api.{
  Session,
  User
}


trait SessionStoreProvider extends SPI[SessionStore]

object SessionStore extends SPILoader(classOf[SessionStoreProvider])


trait SessionStore
{

  def newToken: AccessToken


  def save(
    session: Session
  )(
    implicit ec: ExecutionContext
  ): Future[Session]


  def get(
    token: AccessToken
  )(
    implicit ec: ExecutionContext
  ): Future[Option[Session]]


  def findFor(
    username: User.Name
  )(
    implicit ec: ExecutionContext
  ): Future[Option[Session]]


  def delete(
    token: AccessToken
  )(
    implicit ec: ExecutionContext
  ): Future[Option[Session]]

}



object DefaultSessionStore
extends SessionStore
with Logging
{

  import java.util.concurrent.Executors
  import java.util.concurrent.TimeUnit.SECONDS

  import scala.collection.concurrent.{
    Map,
    TrieMap
  }


  private val sessions: Map[AccessToken,Session] =
    TrieMap.empty[AccessToken,Session]


  private val executor = Executors.newSingleThreadScheduledExecutor

  executor.scheduleAtFixedRate(
    new Runnable {
      def run: Unit = {
        log.debug("Running expired session clean-up task")

        sessions --= sessions.values
                       .map(_.token)
                       .filter(
                         tkn => (Instant.now isAfter Instant.ofEpochMilli(tkn.created_at + tkn.expires_in))
                       )
                       .map(_.access_token)

        log.debug("Finished running session clean-up task")
      }
    },
    30,
    30,
    SECONDS
  )  

  //TODO: improve randomness of token  
  def newToken = AccessToken(java.util.UUID.randomUUID.toString)


  def save(
    session: Session
  )(
    implicit ec: ExecutionContext
  ): Future[Session] = 
    Future {
      sessions += (session.token.access_token -> session)
      session
    }


  def get(
    token: AccessToken
  )(
    implicit ec: ExecutionContext
  ): Future[Option[Session]] =
    Future { sessions.get(token) } //TODO: refresh session expiry dateTime


  def findFor(
    username: User.Name
  )(
    implicit ec: ExecutionContext
  ): Future[Option[Session]] =
    Future { sessions.values.find(_.user.username == username) }


  def delete(
    token: AccessToken
  )(
    implicit ec: ExecutionContext
  ): Future[Option[Session]] =
    Future { sessions.remove(token) }


}
