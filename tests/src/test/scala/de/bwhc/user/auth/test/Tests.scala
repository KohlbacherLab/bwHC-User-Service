package de.bwhc.user.auth.test



import java.nio.file.Files.createTempDirectory

import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.must.Matchers._
import org.scalatest.OptionValues._

import de.bwhc.user.auth.api._



class Tests extends AsyncFlatSpec
{

  val tmpDir = createTempDirectory("bwhc_user_test_data").toFile

  lazy val serviceLoad = UserService.getInstance



  "UserService SPI" must "have worked" in {

    System.setProperty("bwhc.user.data.dir", tmpDir.getAbsolutePath)

    serviceLoad.isSuccess mustBe true

  }

 
  val create =
    UserCommand.Create(
      User.Name("test_user"),
      User.Password("changeit"),
      HumanName(
        HumanName.Given("Ute"),
        HumanName.Family("Musterfrau")
      ),
      Set(Role.Admin,Role.ZPMCoordinator)
    )
  
  
  lazy val userService = serviceLoad.get
  

  "User creation" must "work" in {

    for {
      result   <- userService ! create
      createOk =  result.isRight mustBe true      
      newUser  =  result.toOption.get
      user     <- userService.get(newUser.id)
      fetchOk  =  user mustBe defined
      ok       =  user.value mustBe newUser  
    } yield ok

  }


  import SessionCommand._
  import SessionEvent._

  "User login and logout" must "work" in {

    val login = Login(create.username,create.password)

    for {
      loginResult <- userService ! login 

      loginOk =  loginResult.isRight mustBe true

      LoggedIn(UserWithRoles(username,roles),token) = loginResult.toOption.get

      userOk  = username mustBe create.username
      rolesOk = roles    mustBe create.roles

      logoutResult <- userService ! Logout(username)

      logoutOk  = logoutResult.isRight mustBe true

    } yield logoutOk

  }



}
