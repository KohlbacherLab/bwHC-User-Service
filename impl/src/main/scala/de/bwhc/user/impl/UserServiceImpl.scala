package de.bwhc.user.impl


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
import de.bwhc.util.data.Validation._
import de.bwhc.util.data.Validation.dsl._
import de.bwhc.util.oauth._
import de.bwhc.util.hash.MD5
import de.bwhc.util.mapping._
import de.bwhc.util.mapping.syntax._

import de.bwhc.user.api._



class UserServiceProviderImpl extends UserServiceProvider
{

  def getInstance: UserService = {

    val userDB       = UserDB.getInstance.get

    new UserServiceImpl(userDB)
  }

}


object UserServiceImpl
{

  // Regex to check password criteria:
  // Minimum eight characters, at least one uppercase letter, one lowercase letter, one number and one special character
  // https://stackoverflow.com/questions/3802192/regexp-java-for-password-validation
  private val pwdRegex = """\A(?=\S*?[0-9])(?=\S*?[a-z])(?=\S*?[A-Z])(?=\S*?[@#$%/-\^&+=])\S{8,}\z""".r
  

  implicit val passwordValidator = {
    (pwd: User.Password) =>
      pwd.value must matchRegex (pwdRegex) otherwise (
        """Invalid password! Ensure the following criteria are met:
           - Minimum 8 characters,
           - At least one uppercase letter and one lowercase letter
           - At least one number
           - At least one special character
           - No whitespace"""
        ) map (_ => pwd)
  }

}


class UserServiceImpl
(
  private val userDB: UserDB,
)
extends UserService
with Logging
{

  import UserServiceImpl._


  implicit val UserwithPasswordToUser = deriveMapping[UserWithPassword,User]


  //TODO: Logging!

  //---------------------------------------------------------------------------
  // User management ops
  //---------------------------------------------------------------------------


  override def process(
    cmd: UserCommand
  )(
    implicit ec: ExecutionContext
  ): Future[ErrorsOr[UserEvent]] = {

    import cats.syntax.apply._
    import UserCommand._
    import UserEvent._

    cmd match {

      //-----------------------------------------------------------------------
      case Create(username,pwd,humanName,roles) => {

        for {

          usernameOk <- userDB.find(_.username == username)
                         .map(user =>
                           Validated.condNel(
                             !user.isDefined,
                             username,
                             s"Username '${username.value}' is already in use"
                           )
                         )

          pwdOk      <- Future { pwd validate }

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
                .map(_.mapTo[User])
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
                pwd => pwd.validate.map(Option(_))
              )
            )
          
          result <-
            (userExists,pwdOk).mapN(
              (user,_) =>
                userDB.update(
                  id,
                  _.copy(
                    humanName  = humanName.getOrElse(user.humanName),
                    username   = newName.getOrElse(user.username),
                    password   = newPwd.getOrElse(user.password),
                    lastUpdate = Instant.now
                  )
                )
                .map(_.get)
                .map(_.mapTo[User])
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
              .map(_.mapTo[User])
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


  override def identify(
    username: User.Name,
    password: User.Password
  )(
    implicit ec: ExecutionContext
  ): Future[Option[User]] = {

    if (username == User.Name("admin") &&
        password == User.Password("admin")){

      for {
        empty <- userDB.isEmpty
        user = if (empty)
          Some(
            User(
              userDB.newId,
              username,
              HumanName(
                HumanName.Given("Admin"),
                HumanName.Family("istrator")
              ),
              User.Status.Active,
              Set(Role.Admin),
              LocalDate.now,
              Instant.now
            )
          )
           else None
      } yield user

    } else {

//TODO: block user account after N unsuccessful login attempts 
      for {
        optUser <-
          userDB.find(usr =>
            usr.username == username &&
            usr.password == User.Password(MD5(password.value))
          )
        user = optUser.filter(_.status == User.Status.Active)
//        user = optUser.filter(usr => usr.status == User.Status.Blocked || status != User.Status.Blocked)
                 .map(_.mapTo[User])
      } yield user

    }

  }


  override def getAll(
    implicit ec: ExecutionContext
  ): Future[Iterable[User]] = 
    for {
      users  <- userDB.filter(_ => true)
      result =  users.map(_.mapTo[User])
    } yield result


  override def get(
    id: User.Id
  )(
    implicit ec: ExecutionContext
  ): Future[Option[User]] = {
    for {
      usr  <- userDB.get(id)
      user = usr.map(_.mapTo[User])
    } yield user
  }


}
