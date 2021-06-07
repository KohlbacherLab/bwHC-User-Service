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


  import scala.collection.concurrent.{Map,TrieMap}

  private val tmpUsers: Map[User.Id,User] =
    TrieMap.empty[User.Id,User] 


  //---------------------------------------------------------------------------
  // User management ops
  //---------------------------------------------------------------------------


  override def process(
    cmd: UserCommand
  )(
    implicit ec: ExecutionContext
  ): Future[ErrorsOr[UserEvent]] = {

    import de.bwhc.util.syntax.piping._
    import cats.syntax.apply._
    import cats.syntax.validated._
    import UserCommand._
    import UserEvent._

    cmd match {

      //-----------------------------------------------------------------------
      case Create(username,pwd,givenName,familyName,roles) => {

        log.info(s"Handling User account creation for ${givenName.value} ${familyName.value}")

        for {

          // Ensure username is not a duplicate
          usernameOk <- userDB.find(_.username == username)
                          .map(user =>
                           Validated.condNel(
                             !user.isDefined,
                             username,
                             s"Username '${username.value}' is already in use"
                           )
                         )

          // Ensure first created user has Admin role
          adminEnsured <- userDB.filter(_.roles contains Role.Admin)
                            .map(
                              adminUsers =>
                                if (adminUsers.isEmpty)
                                   Role.Admin must be (in (roles)) otherwise ("First created user MUST have Admin rights")
                                else
                                   roles.validNel[String]
                            )

          // Ensure password meets complexity requirements
          pwdOk      <- Future { pwd validate }

          result     <-
            (usernameOk,adminEnsured,pwdOk).mapN(
              (_,_,_) =>
                userDB.save(
                  UserWithPassword(
                    userDB.newId,
                    username,
                    User.Password(MD5(pwd.value)),
                    givenName,
                    familyName,
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
              errs    => Future.successful(errs.asLeft[UserEvent]),
              created => created.andThen { case Success(_) => tmpUsers.clear }
            )
        } yield result
      }


      //-----------------------------------------------------------------------
      case Update(id,newGivenName,newFamilyName,newName,newPwd) => {

        log.info(s"Handling User Data update for User $id")

        for {
          userExists <-
            userDB.get(id)
              .map(_ mustBe defined otherwise (s"Invalid User $id"))
          
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
                  usr => {
                    newGivenName.fold(usr)(v => usr.copy(givenName = v)) |
                    (u => newFamilyName.fold(u)(v => u.copy(familyName = v))) |
                    (u => newName.fold(u)(v => u.copy(username = v))) |
                    (u => newPwd.fold(u)(v => u.copy(password = User.Password(MD5(v.value))))) |
                    (_.copy(lastUpdate = Instant.now))
                  }
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

        log.info(s"Handling User Roles update for User $id")

        for {
          userExists <- userDB.get(id)
                      .map(_ mustBe defined otherwise (s"Invalid User $id"))

          // Ensure Admin rights are not removed from last Admin user
          notLastAdmin <-
            if (!(roles contains Role.Admin))
              userDB.filter(usr => (usr.roles contains Role.Admin) && usr.id != id)
                .map( 
                  adminUsers =>
                    if (adminUsers.isEmpty)
                      "Can't retract Admin rights from last Admin".invalidNel[Boolean]
                    else
                      true.validNel[String]
                )
            else Future.successful(true.validNel[String])

          result <-
            (userExists,notLastAdmin).mapN(
              (_,_) =>
                userDB.update(
                  id,
                  _.copy(
                    roles      = roles,
                    lastUpdate = Instant.now
                  )
                )
            )
            .fold(
              errs => Future.successful(errs.asLeft[UserEvent]),
              _.map(_.get)
               .map(_.mapTo[User])
               .map(Updated(_))
               .map(_.asRight[NonEmptyList[String]])
            )

        } yield result

      }

      //-----------------------------------------------------------------------
      case Delete(id) => {

        log.info(s"Handling account deletion command for User $id")

        for {
          userExists <- userDB.get(id)
                          .map(_ mustBe defined otherwise (s"Invalid User $id"))

          // Ensure not last Admin user is deleted
          notLastAdmin <- userDB.filter(usr => (usr.roles contains Role.Admin) && usr.id != id)
                            .map( 
                              adminUsers =>
                                if (adminUsers.isEmpty)
                                  "Can't delete last user with Admin rights".invalidNel[Boolean]
                                else
                                  true.validNel[String]
                            )

          result <-
            (userExists,notLastAdmin).mapN(
              (_,_) =>
                userDB.update(
                  id,
                  _.copy(
                    status = User.Status.Deleted,
                    lastUpdate = Instant.now
                  )
                )
            )
            .fold(
              errs => Future.successful(errs.asLeft[UserEvent]),
              ok => ok.map(_ => Deleted(id).asRight[NonEmptyList[String]])
            )
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

    log.info(s"Handling identification request for $username")

    (username,password) match {

      case (User.Name("admin"),User.Password("admin")) => {

        val tmpUser =
          for {
            empty <- userDB.isEmpty
            user =
              if (empty)
                Some(
                  User(
                    userDB.newId,
                    username,
                    GivenName("Admin"),
                    FamilyName("istrator"),
                    User.Status.Active,
//TODO:   re-consider creating a temp user with ALL roles instead of just Admin
                    Set(Role.Admin),
                    LocalDate.now,
                    Instant.now
                  )
                )
              else None
          } yield user

        tmpUser.andThen {
          case Success(Some(usr)) => tmpUsers += (usr.id -> usr)
        }
      }

      case _ => {

//TODO: block user account after N unsuccessful login attempts 
        for {
          optUser <-
            userDB.find(usr =>
              usr.username == username &&
              usr.password == User.Password(MD5(password.value)) &&
              usr.status   == User.Status.Active
            )
          user = optUser.map(_.mapTo[User])
        } yield user
      }
      
    }

  }


  override def getAll(
    implicit ec: ExecutionContext
  ): Future[Iterable[User]] = {

    log.info(s"Handling request for Users list")

    for {
      users  <- userDB.filter(_ => true)
      result =  users.map(_.mapTo[User])
    } yield result

  }

  override def get(
    id: User.Id
  )(
    implicit ec: ExecutionContext
  ): Future[Option[User]] = {

    log.info(s"Handling request for User object $id")

    for {
      usr  <- userDB.get(id)
      user =  usr.map(_.mapTo[User])
                .orElse(tmpUsers.get(id))
    } yield user
  }


}
