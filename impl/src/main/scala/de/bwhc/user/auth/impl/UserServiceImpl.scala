package de.bwhc.user.auth.impl


import java.time.{Instant,LocalDate}

import scala.util.{
  Either,
  Try,
  Failure,
  Success
}
import scala.util.matching.Regex

import scala.concurrent.{
  ExecutionContext,
  Future
}

import cats.data.{
  Validated,
  ValidatedNel,
  NonEmptyList
}
import cats.syntax.either._

import de.bwhc.util.Logging
import de.bwhc.util.oauth._
import de.bwhc.util.hash.MD5

import de.bwhc.user.auth.api._



class UserServiceProviderImpl extends UserServiceProvider
{

  def getInstance: UserService = {

    val userDB       = UserDB.getInstance.get
    val sessionStore = SessionStore.getInstance.getOrElse(DefaultSessionStore)

    new UserServiceImpl(userDB,sessionStore)
  }

}


object UserServiceImpl
{

  type ValidationResult[+T] = ValidatedNel[String,T]


  // Regex to check password criteria:
  // Minimum eight characters, at least one uppercase letter, one lowercase letter, one number and one special character
  // https://stackoverflow.com/questions/3802192/regexp-java-for-password-validation
  private val pwdRegex = """\A(?=\S*?[0-9])(?=\S*?[a-z])(?=\S*?[A-Z])(?=\S*?[@#$%/-\^&+=])\S{8,}\z""".r
  

  private def validatePassword(
    pwd: User.Password
  ): ValidationResult[User.Password] = {
    Validated.condNel(
      pwdRegex.matches(pwd.value),
      pwd,
      """Invalid password! Ensure the following criteria are met:
         - Minimum eight characters,
         - At least one uppercase letter and one lowercase letter
         - At least one number
         - At least one special character
         - No whitespace"""
    ) 
       
  }

}


class UserServiceImpl
(
  private val userDB: UserDB,
  private val sessions: SessionStore
)
extends UserService
with Logging
{

  import UserServiceImpl._


  //TODO: Logging!

  //---------------------------------------------------------------------------
  // User management ops
  //---------------------------------------------------------------------------


  def process(
    cmd: UserCommand
  )(
    implicit ec: ExecutionContext
  ): Future[ErrorsOr[UserEvent]] = {
//  ): Future[Either[NonEmptyList[String],UserEvent]] = {

    import cats.implicits._
    import UserCommand._
    import UserEvent._

    cmd match {

      //-----------------------------------------------------------------------
      case Create(username,pwd,humanName,roles) => {

        for {
          usernameOk <- userDB.find(_.name == username)
                         .map(user =>
                           Validated.condNel(
                             !user.isDefined,
                             username,
                             s"Username '${username.value}' is already in use"
                           )
                         )
          pwdOk      <- Future(validatePassword(pwd))
          result     <-
            (usernameOk,pwdOk).mapN(
              (_,_) =>
                userDB.save(
                  UserWithPassword(
                    userDB.newId,
                    username,
                    User.Password(MD5(pwd.value)),
                    humanName,
                    User.Status.Active,
                    roles,
                    LocalDate.now,
                    Instant.now
                  )
                )
                .map(_.toUser)
                .map(Created(_))
                .map(_.asRight[NonEmptyList[String]])
            )
            .fold(
              errs => Future.successful(errs.asLeft[UserEvent]),
              created => created
            )
        } yield result
      }

      //-----------------------------------------------------------------------
      case Update(id,humanName,newName,newPwd) => {

        for {
          userExists <-
            userDB.get(id)
            .map(
              user => Validated.fromOption(user,s"Invalid User ID ${id.value}").toValidatedNel
            )
          
          pwdOk <-
            Future(
              newPwd.fold(
                Validated.validNel[String,Option[User.Password]](None)
              )(
                pwd => validatePassword(pwd).map(Option(_))
              )
            )
          
          result <-
            (userExists,pwdOk).mapN(
              (user,_) =>
                userDB.update(
                  id,
                  _.copy(
                    humanName  = humanName.getOrElse(user.humanName),
                    name       = newName.getOrElse(user.name),
                    password   = newPwd.getOrElse(user.password),
                    lastUpdate = Instant.now
                  )
                )
                .map(_.get)
                .map(_.toUser)
                .map(Updated(_))
                .map(_.asRight[NonEmptyList[String]])
            )
            .fold(
              errs => Future.successful(errs.asLeft[UserEvent]),
              updated => updated
            )

        } yield result

      }

      //-----------------------------------------------------------------------
      case UpdateRoles(id,roles) => {

        for {
          exists <- userDB.get(id)
          result <- exists match {
            case Some(user) =>
              userDB.update(
                id,
                _.copy(
                  roles      = roles,
                  lastUpdate = Instant.now
                )
              )
              .map(_.get)
              .map(_.toUser)
              .map(Updated(_))
              .map(_.asRight[NonEmptyList[String]])

            case None =>
              Future.successful(NonEmptyList.one(s"Invalid User ID ${id.value}").asLeft[UserEvent])

          }
        } yield result

      }

      //-----------------------------------------------------------------------
      case Delete(id) => {

        for {
          exists <- userDB.get(id)
          result <- exists match {
            case Some(user) =>
              userDB.update(
                id,
                _.copy(
                  status = User.Status.Deleted,
                  lastUpdate = Instant.now
                )
              )
              .map(_.get)
              .map(_ => Deleted(id).asRight[NonEmptyList[String]])

            case None =>
              Future.successful(NonEmptyList.one(s"Invalid User ID ${id.value}").asLeft[UserEvent])
          }
        } yield result
      }

    }

  }


  def get(
    id: User.Id
  )(
    implicit ec: ExecutionContext
  ): Future[Option[User]] = {
    for {
      usr  <- userDB.get(id)
      user = usr.map(_.toUser)
    } yield user
  }


  //---------------------------------------------------------------------------
  // Session management ops
  //---------------------------------------------------------------------------

  def process(
    cmd: SessionCommand
  )(
    implicit ec: ExecutionContext
  ): Future[ErrorsOr[SessionEvent]] = {
//  ): Future[Either[NonEmptyList[String],SessionEvent]] = {

    import SessionCommand._
    import SessionEvent._

    cmd match {

      //-----------------------------------------------------------------------
      case Login(username,pwd) => {

        if (username == User.Name("admin") && pwd == User.Password("admin")){

          for {
            users <- userDB.filter(_ => true)
            if users.isEmpty
            token <-
              Future.successful(
                OAuthToken(
                  sessions.newToken,
                  TokenType.Bearer,
                  15*60*1000,  
                  None,
                  Instant.now.toEpochMilli,
                  Some("bwHealthCloud")
                )
              )

            userRoles = UserWithRoles(username,Set(Role.Admin))

            session <- 
              sessions.save(
                Session( 
                  token,
                  userRoles,
                  Instant.now
                )
              )
            loggedIn = LoggedIn(userRoles,token).asRight[NonEmptyList[String]] 

          } yield loggedIn 

        } else {

        for {
          exists <- userDB.find(usr => usr.name == username && usr.password == User.Password(MD5(pwd.value)))
          result <-
            exists match {
              case Some(user) if (user.status == User.Status.Active) =>
                for {
                  token <-
                    Future.successful(
                      OAuthToken(
                        sessions.newToken,
                        TokenType.Bearer,
                        15*60*1000,  
                        None,
                        Instant.now.toEpochMilli,
                        Some("bwHealthCloud")
                      )
                    )

                  userRoles = UserWithRoles(user.name,user.roles)

                  session <- 
                    sessions.save(
                      Session( 
                        token,
                        userRoles,
                        Instant.now
                      )
                    )
                  loggedIn = LoggedIn(userRoles,token).asRight[NonEmptyList[String]] 
                } yield loggedIn 

              case None =>
//TODO: block user account after N unsuccessful login attempts 
                Future.successful(NonEmptyList.one(s"Invalid credentials").asLeft[SessionEvent])
            }
        } yield result

        }
      }

      //-----------------------------------------------------------------------
      case Logout(username) => {

        Future.successful(LoggedOut().asRight[NonEmptyList[String]])
          .andThen {
            case Success(_) => 
              for {
                session <- sessions.findFor(username)
                if session.isDefined
                loggedOut <- sessions.delete(session.get.id)
              } yield loggedOut
          }
      }

    }

  }


  def sessionFor(
    token: AccessToken
  )(
    implicit ec: ExecutionContext
  ): Future[Option[Session]] = {

    sessions.get(token)

  }


}
