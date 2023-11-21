package de.bwhc.user.auth.test



import java.nio.file.Files.createTempDirectory

import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.must.Matchers._
import org.scalatest.OptionValues._

import de.bwhc.user.api._



class Tests extends AsyncFlatSpec
{

  val tmpDir = createTempDirectory("bwhc_user_test_data").toFile

  tmpDir.deleteOnExit

  lazy val serviceLoad = UserService.getInstance



  "UserService SPI" must "have worked" in {

    System.setProperty("bwhc.user.data.dir", tmpDir.getAbsolutePath)

    serviceLoad.isSuccess mustBe true

  }

  import UserCommand._
  import UserEvent._

 
  
  lazy val userService = serviceLoad.get
 
 
  val userName = User.Name("test_user")
  val initPwd  = User.Password("ChAnGe1T!")
  val nextPwd  = User.Password("Te$ting1!")


  "User creation with invalid Password" must "NOT have worked" in {

    for {
      result <- userService ! Create(userName,User.Password("too-simple"),GivenName("Ute"),FamilyName("Musterfrau"),Role.values)
    } yield result.isLeft mustBe true      

  }


  "Creation of non-Admin first User " must "NOT have worked" in {

    for {
      result <- userService ! Create(userName,initPwd,GivenName("Ute"),FamilyName("Musterfrau"),(Role.values - Role.Admin))
    } yield result.isLeft mustBe true      

  }
  

  "User creation" must "have worked" in {

    for {
      result   <- userService ! Create(userName,initPwd,GivenName("Ute"),FamilyName("Musterfrau"),Role.values)
      createOk =  result.isRight mustBe true      
      Created(newUser,_) = result.toOption.get
      user     <- userService.get(newUser.id)
      fetchOk  =  user mustBe defined
      ok       =  user.value mustBe newUser  
    } yield ok

  }

  
  "Duplicate User creation" must "NOT have worked" in {

    for {
      result   <- userService ! Create(userName,initPwd,GivenName("Ute"),FamilyName("Musterfrau"),Role.values)
      ok     =  result.isLeft mustBe true      
    } yield ok

  }

  
  "User login" must "have worked" in {

    for {
      optUser <- userService.identify(userName,initPwd)     
    } yield optUser mustBe defined 

  }


  "User update" must "have worked" in {

    for {
      user    <- userService.identify(userName,initPwd)     
      usrId   =  user.value.id
      updated <- userService ! Update(usrId,None,None,None,Some(nextPwd))
      ok      =  updated.isRight mustBe true

      oldUser <- userService.identify(userName,initPwd)
      pwdChanged = oldUser must not be defined
      newUser <- userService.identify(userName,nextPwd)
      ok      = newUser mustBe defined
    } yield ok 

  }


  "Password change" must "have worked" in {

    for {
      user <- userService.identify(userName,nextPwd)

      updated <-
        userService ! ChangePassword(
          id = user.get.id,
          currentPassword = nextPwd,
          newPassword1 = initPwd,
          newPassword2 = initPwd
        )

      changed = updated.isRight mustBe true

      changedUser <- userService.identify(userName,initPwd)

    } yield changedUser mustBe defined

  }


  it must "have failed" in {

    for {
      user <- userService.identify(userName,initPwd)

      updated <-
        userService ! ChangePassword(
          id = user.get.id,
          currentPassword = initPwd,
          newPassword1 = User.Password("Te$ting1!"),
          newPassword2 = User.Password("Te$ting2!"),
        )

    } yield updated.isLeft mustBe true

  }


}
