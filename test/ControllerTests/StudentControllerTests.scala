package ControllerTests

import com.google.inject.AbstractModule
import com.mohiva.play.silhouette.api.Environment
import com.mohiva.play.silhouette.test._
import controllers.{UserController, routes}
import dao.UserDao
import models.{Role, User}
import net.codingwell.scalaguice.ScalaModule
import org.scalatest.concurrent.ScalaFutures
import org.scalatestplus.play.PlaySpec
import org.scalatestplus.play.guice.GuiceOneAppPerTest
import play.api.Application
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.concurrent.Execution.Implicits._
import play.api.test.Helpers._
import play.api.test.{FakeRequest, WithApplication}
import utils.Silhouette._

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

class StudentControllerTests extends PlaySpec with GuiceOneAppPerTest with ScalaFutures {
  def homeController: UserController = app.injector.instanceOf(classOf[UserController])

  //Ref:: https://github.com/playframework/play-slick/blob/master/samples/computer-database/test/ModelSpec.scala
  def userDao(implicit app: Application) = {
    val app2UserDAO = Application.instanceCache[UserDao]
    app2UserDAO(app)
  }

  "UserController" should {

    "redirect to homepage if user already logged in" in new NormalUserContext {
      new WithApplication(application) {
        val Some(result) = route(app, FakeRequest(routes.UserController.login())
          .withAuthenticator[MyEnv](NORMAL_USER.loginInfo))
        status(result) mustBe SEE_OTHER
      }
    }

    "should able to access add-student-page" in new NormalUserContext {
      new WithApplication(application) {
        val Some(result) = route(app, FakeRequest(routes.StudentController.add())
          .withAuthenticator[MyEnv](NORMAL_USER.loginInfo))
        status(result) mustBe OK
      }
    }

    "should able to access menu-student-page" in new NormalUserContext {
      new WithApplication(application) {
        val Some(result) = route(app, FakeRequest(routes.StudentController.menu())
          .withAuthenticator[MyEnv](NORMAL_USER.loginInfo))
        status(result) mustBe OK
      }
    }
  }

  val NORMAL_USER_EMAIL = "xyz@xyz.com"

  trait NormalUserContext {

    class FakeModule extends AbstractModule with ScalaModule {
      def configure() = {
        bind[Environment[MyEnv]].toInstance(env)
      }
    }

    val normalUser = User(Some(1), NORMAL_USER_EMAIL, "password", "username", Role.NormalUser, true)
    userDao.deleteUserByEmail(NORMAL_USER_EMAIL).futureValue
    userDao.insertUser(normalUser).futureValue

    val Some(user) = userDao.getUserByEmail(normalUser.email).futureValue
    val NORMAL_USER = user
    implicit val env: Environment[MyEnv] = new FakeEnvironment[MyEnv](Seq(NORMAL_USER.loginInfo -> NORMAL_USER))

    lazy val application = new GuiceApplicationBuilder().overrides(new FakeModule()).build
  }

  def await[T](v: Future[T]): T = Await.result(v, Duration.Inf)

}
