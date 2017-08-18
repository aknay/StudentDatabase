//package ControllerTests
//
//import com.google.inject.AbstractModule
//import com.mohiva.play.silhouette.api.Environment
//import com.mohiva.play.silhouette.test._
//import controllers.{HomeController, UserController, routes}
//import dao.{AdminToolDao, UserDao}
//import models.{Role, User}
//import net.codingwell.scalaguice.ScalaModule
//import org.scalatest.concurrent.ScalaFutures
//import org.scalatestplus.play.PlaySpec
//import org.scalatestplus.play.guice.GuiceOneAppPerTest
//import play.api.Application
//import play.api.i18n.Messages
//import play.api.inject.guice.GuiceApplicationBuilder
//import play.api.libs.concurrent.Execution.Implicits._
//import play.api.test.Helpers.{contentAsString, _}
//import play.api.test.{FakeRequest, WithApplication}
//import utils.Silhouette._
//import play.api.i18n.Messages.Implicits._
//
//import scala.concurrent.{Await, Future}
//import scala.concurrent.duration.Duration
//
//class UserControllerTests extends PlaySpec with GuiceOneAppPerTest with ScalaFutures {
//  def homeController: UserController = app.injector.instanceOf(classOf[UserController])
//  //Ref:: https://github.com/playframework/play-slick/blob/master/samples/computer-database/test/ModelSpec.scala
//  def userDao(implicit app: Application) = {
//    val app2UserDAO = Application.instanceCache[UserDao]
//    app2UserDAO(app)
//  }
//
//  def adminToolDao(implicit app: Application) = {
//    val app2AdminToolDAO = Application.instanceCache[AdminToolDao]
//    app2AdminToolDAO(app)
//  }
//
//  private val EMAIL = "new@new.com"
//
////  def removeUser = userDao.deleteUserByEmail(EMAIL).futureValue
////
////  def getNewUser(): User = {
////    val password = "new"
////    val username = "new"
////    val user = userDao.getUserByEmail(EMAIL).futureValue
////    if (user.isDefined) userDao.deleteUserByEmail(EMAIL).futureValue
////    User(Some(1), EMAIL, password, username, Role.NormalUser, true)
////  }
//
//
//
//
//
//  "UserController" should {
//
////    "should insert rfid form for single team" in {
////      val rfid = "123456789A"//must be 10 chars
////      val result = homeController.submitRfidForSingleTeam(
////        FakeRequest().withFormUrlEncodedBody("rfid" -> rfid)
////      )
////      status(result) must equal(SEE_OTHER)
////      redirectLocation(result) mustBe Some(routes.HomeController.singleTeamForm().url)
////      flash(result).get("success") mustBe Some(Messages("form.success.rfid.single.team", rfid))
////    }
//
//
//
//
//
////    "able to access login page" in {
////      val loginPage = route(app, FakeRequest(routes.UserController.login())).get
////      status(loginPage) mustBe OK
////      contentType(loginPage) mustBe Some("text/html")
////      contentAsString(loginPage) must include("Login")
////    }
////
//    "redirect to homepage if user already logged in" in new NormalUserContext {
//      new WithApplication(application) {
//        val Some(result) = route(app, FakeRequest(routes.UserController.login())
//          .withAuthenticator[MyEnv](NORMAL_USER.loginInfo))
//        status(result) mustBe SEE_OTHER
//      }
//    }
////    "should fail to access edit page when user has not logged in" in {
////      val editPage = route(app, FakeRequest(routes.UserController.editUserInfo())).get
////      status(editPage) mustBe UNAUTHORIZED
////    }
////
////    "should  access edit page after user has logged in" in new NormalUserContext {
////      new WithApplication(application) {
////        val Some(result) = route(app, FakeRequest(routes.UserController.editUserInfo())
////          .withAuthenticator[MyEnv](normalUser.loginInfo))
////        status(result) mustBe OK
////        contentAsString(result) must include(Messages("settings.profile.title"))
////      }
////    }
////
////    "user can update UserInfo" in new NormalUserContext {
////      new WithApplication(application) {
////        val Some(editUserInfoPage) = route(app, FakeRequest(routes.UserController.updateUserInfo())
////          .withAuthenticator[MyEnv](normalUser.loginInfo)
////          .withFormUrlEncodedBody("name" -> "username", "location" -> "planet"))
////        status(editUserInfoPage) mustBe SEE_OTHER
////        redirectLocation(editUserInfoPage) mustBe Some(routes.UserController.editUserInfo().url)
////        val Some(result) = route(app, FakeRequest(routes.UserController.editUserInfo())
////          .withAuthenticator[MyEnv](normalUser.loginInfo))
////        status(result) mustBe OK
////        contentAsString(result) must include("planet")
////        contentAsString(result) must include("username")
////      }
////    }
////
////    "should able to sign up and redirect to login page" in {
////      userDao.createUserTableIfNotExisted
////      val user = getNewUser()
////
////      val signUpPage = route(app, FakeRequest(routes.UserController.submitSignUpForm()).withFormUrlEncodedBody("email" -> user.email, "password" -> user.password, "username" -> user.username)).get
////      status(signUpPage) mustBe OK
////      contentAsString(signUpPage) must include("Almost Signed Up")
////      userDao.deleteUserByEmail(user.email).futureValue
////    }
////
////    "should NOT be able to sign up if there is already account in DB" in {
////      val user = getNewUser()
////
////      val signUpPage = route(app, FakeRequest(routes.UserController.submitSignUpForm()).
////        withFormUrlEncodedBody("email" -> user.email, "password" -> user.password, "username" -> user.username)).get
////      status(signUpPage) mustBe OK
////
////      //another attempt to sign up with same email
////      val anotherSignUpPage = route(app, FakeRequest(routes.UserController.submitSignUpForm()).
////        withFormUrlEncodedBody("email" -> user.email, "password" -> user.password, "username" -> user.username)).get
////      status(anotherSignUpPage) mustBe SEE_OTHER
////
////      val signUpFailFlashMessage = "Login Failed"
////      val redirectedSignUpPage = route(app, FakeRequest(GET, redirectLocation(anotherSignUpPage).get).withFlash("error" -> signUpFailFlashMessage)).get
////      status(redirectedSignUpPage) mustBe OK
////      contentAsString(redirectedSignUpPage) must include("Sign Up")
////      contentAsString(redirectedSignUpPage) must include(signUpFailFlashMessage)
////
////      removeUser
////    }
////
////    "normal user should not access to master page" in new NormalUserContext {
////      new WithApplication(application) {
////        val Some(tryingAccessLoginPage) = route(app, FakeRequest(routes.AdminController.admin())
////          .withAuthenticator[MyEnv](normalUser.loginInfo))
////        status(tryingAccessLoginPage) mustBe UNAUTHORIZED
////      }
////    }
////
////    "should NOT be able to login and redirect to login page when there is no user" in {
////      val emailAddress = "nosuchuser@nosuchuser.com"
////      val password = "nosuchuser"
////      val username = "name"
////
////      val loginPage = route(app, FakeRequest(routes.UserController.submitLoginForm()).
////        withFormUrlEncodedBody("email" -> emailAddress, "password" -> password, "username" -> username)).get
////      status(loginPage) mustBe SEE_OTHER
////      redirectLocation(loginPage) mustBe Some(routes.UserController.login().url)
////
////      //after redirect
////      val loginFailFlashMessage = "Login Failed"
////      val redirectedLoginPage = route(app, FakeRequest(GET, redirectLocation(loginPage).get).withFlash("error" -> loginFailFlashMessage)).get
////      status(redirectedLoginPage) mustBe OK
////      contentAsString(redirectedLoginPage) must include("Login")
////      contentAsString(redirectedLoginPage) must include(loginFailFlashMessage)
////    }
////
////    "normal user can apply to be master" in new NormalUserContext {
////      new WithApplication(application) {
////        val Some(result) = route(app, FakeRequest(routes.UserController.requestToBeMaster())
////          .withAuthenticator[MyEnv](normalUser.loginInfo))
////        status(result) mustBe OK
////      }
////    }
////
////    "part 1 - normal user can become master using token" in new NormalUserContext {
////      new WithApplication(application) {
////
////        val Some(result) = route(app, FakeRequest(routes.UserController.requestToBeMaster())
////          .withAuthenticator[MyEnv](normalUser.loginInfo))
////        status(result) mustBe OK
////
////        val approveWithToken = route(app, FakeRequest(routes.UserController.approveUserWithToken(UserController.getToken)).
////          withAuthenticator[MyEnv](normalUser.loginInfo)).get
////        status(approveWithToken) mustBe OK
////      }
////    }
////
////    "part 2 - normal user can become master using token" in {
////      //Note: role in part 1 does not reflect immediately//No idea
////      //That's why, we are testing the role again in this test
////      val role = userDao.getUserByEmail(NORMAL_USER_EMAIL).futureValue.get.role
////
////      role mustBe Role.Admin
////    }
////
////    "logged in user should redirect to Home page if reset password page is accessed" in new NormalUserContext {
////      new WithApplication(application) {
////        val Some(result) = route(app, FakeRequest(routes.UserController.requestResetPassword())
////          .withAuthenticator[MyEnv](normalUser.loginInfo))
////        status(result) mustBe SEE_OTHER
////        redirectLocation(result) mustBe Some(routes.HomeController.index().url)
////      }
////    }
////
////    "user should see reset password page if haven't log in yet" in {
////      val Some(result) = route(app, FakeRequest(routes.UserController.requestResetPassword()))
////      status(result) mustBe OK
////    }
////
////    "user should reset the password" in new NormalUserContext {
////      new WithApplication(application) {
////        val forgetPasswordForm = route(app, FakeRequest(routes.UserController.handleForgotPassword()).
////          withFormUrlEncodedBody("email" -> normalUser.email)).get
////        status(forgetPasswordForm) mustBe OK
////        val token = UserController.getToken
////        val Some(resetPassword) = route(app, FakeRequest(routes.UserController.resetPassword(token)))
////        status(resetPassword) mustBe OK
////        //should see other when bot password are not same
////        val failResetPasswordForm = route(app, FakeRequest(routes.UserController.handleResetPassword(token))
////          .withFormUrlEncodedBody("password1" -> "abcdef", "password2" -> "abcdefg")).get
////        status(failResetPasswordForm) mustBe SEE_OTHER
////        //should success when both password are same
////        val successResetPasswordForm = route(app, FakeRequest(routes.UserController.handleResetPassword(token))
////          .withFormUrlEncodedBody("password1" -> "abcdefg", "password2" -> "abcdefg")).get
////        status(successResetPasswordForm) mustBe OK
////      }
////    }
////
////
////    "delete normal user to clear db--this is not a test" in new NormalUserContext {
////      new WithApplication(application) {
////        userDao.deleteUserByEmail(normalUser.email).futureValue
////      }
////    }
//  }
//
//
//  val NORMAL_USER_EMAIL = "xyz@xyz.com"
//
//  trait NormalUserContext {
//
//    class FakeModule extends AbstractModule with ScalaModule {
//      def configure() = {
//        bind[Environment[MyEnv]].toInstance(env)
//      }
//    }
//
//    val normalUser = User(Some(1), NORMAL_USER_EMAIL, "password", "username", Role.NormalUser, true)
////    userDao.deleteUserByEmail(NORMAL_USER_EMAIL).futureValue
//    await(userDao.deleteUserByEmail(NORMAL_USER_EMAIL))
////    userDao.insertUser(normalUser).futureValue
//    await(userDao.insertUser(normalUser))
//
//    val Some(user) = userDao.getUserByEmail(normalUser.email).futureValue
////    val Some(user) = await(userDao.getUserByEmail(normalUser.email))
//    val NORMAL_USER = user
//    implicit val env: Environment[MyEnv] = new FakeEnvironment[MyEnv](Seq(NORMAL_USER.loginInfo -> NORMAL_USER))
//
//    lazy val application = new GuiceApplicationBuilder().overrides(new FakeModule()).build
//  }
//
//  def await[T](v: Future[T]): T = Await.result(v, Duration.Inf)
//
//}
