//import com.google.inject.AbstractModule
//import com.mohiva.play.silhouette.api.Environment
//import com.mohiva.play.silhouette.test._
//import controllers.routes
//import dao.{AdminToolDao, AlbumDao, UserDao}
//import models.{Album, Role, User}
//import net.codingwell.scalaguice.ScalaModule
//import org.joda.time.DateTime
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
//
//import scala.concurrent.Await
//import scala.concurrent.duration._
////the following import is needed even though it is showing gray in
//import play.api.i18n.Messages.Implicits._
//import scala.concurrent.Future
//
//
//class AlbumControllerTests extends PlaySpec with GuiceOneAppPerTest with ScalaFutures {
//
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
//  def albumDao(implicit app: Application) = {
//    val app2AlbumDAO = Application.instanceCache[AlbumDao]
//    app2AlbumDAO(app)
//  }
//
//  def await[T](fut: Future[T]): T = Await.result(fut, Duration.Inf)
//
//  def deleteNewUser(user: User) {
//    userDao.deleteUserByEmail(user.email) //clean up
//  }
//
//  def getNewAlbum: Album = {
//    val artistName = "TestArtistName"
//    val title = "TestTitleName"
//    Album(Some(1), Some(1), artistName, title)
//  }
//
//  "Album Controller" should {
//
//    "delete announcement" in new MasterUserContext {
//      new WithApplication(application) {
//        await(adminToolDao.deleteAnnouncement(ADMIN_USER))
//      }
//    }
//
//    "normal user can access the adding of an album page" in new NormalUserContext {
//      new WithApplication(application) {
//        val Some(addAlbumRoute) = route(app, FakeRequest(routes.AlbumController.add())
//          .withAuthenticator[MyEnv](NORMAL_USER.loginInfo))
//        status(addAlbumRoute) mustBe OK
//      }
//    }
//
//    "normal user can add album" in new NormalUserContext {
//      new WithApplication(application) {
//        val album = getNewAlbum
//        val saveAlbumRoute = route(app, FakeRequest(routes.AlbumController.save())
//          .withAuthenticator[MyEnv](NORMAL_USER.loginInfo)
//          .withFormUrlEncodedBody("artist" -> album.artist, "title" -> album.title)).get
//        status(saveAlbumRoute) mustBe SEE_OTHER
//        redirectLocation(saveAlbumRoute) mustBe Some(routes.UserController.user().url)
//
//        albumDao.retrieveAlbumId(album.artist, album.title, NORMAL_USER.id.get).futureValue.isDefined mustBe true
//      }
//    }
//
//    "normal user should able to delete an album" in new NormalUserContext {
//      new WithApplication(application) {
//        val album = getNewAlbum
//        val userId = userDao.getUserByEmail(NORMAL_USER.email).futureValue.get.id.get
//        albumDao.insertAlbum(album, userId).futureValue
//        val albumId = albumDao.retrieveAlbumId(album.artist, album.title, userId).futureValue
//
//        val Some(deleteAlbumRoute) = route(app, FakeRequest(routes.AlbumController.
//          delete(albumId.get)).withAuthenticator[MyEnv](NORMAL_USER.loginInfo))
//        status(deleteAlbumRoute) mustBe SEE_OTHER
//        redirectLocation(deleteAlbumRoute) mustBe Some(routes.UserController.user().url)
//
//        albumDao.retrieveAlbumId(album.artist, album.title, userId).futureValue.isDefined mustBe false
//      }
//    }
//
//    "normal user should should NOT be able to add an album when there is existing album" in new NormalUserContext {
//      new WithApplication(application) {
//        val album = getNewAlbum
//        val userId = userDao.getUserByEmail(NORMAL_USER.email).futureValue.get.id.get
//        albumDao.insertAlbum(album, userId)
//        val albumId = albumDao.retrieveAlbumId(album.artist, album.title, userId).futureValue
//
//        val Some(saveAlbumRoute) = route(app, FakeRequest(routes.AlbumController.save())
//          .withAuthenticator[MyEnv](NORMAL_USER.loginInfo)
//          .withFormUrlEncodedBody("artist" -> album.artist, "title" -> album.title))
//        status(saveAlbumRoute) mustBe SEE_OTHER
//        redirectLocation(saveAlbumRoute) mustBe Some(routes.AlbumController.add().url)
//      }
//    }
//
//    "Part 1-user cannot add album before starting date" in new MasterUserContext {
//      new WithApplication(application) {
//        val now = DateTime.now()
//        val announcementString = "announcement testing 2"
//        val startingDate = now.plusDays(1)
//        val endingDate = now.plusDays(2)
//        await(adminToolDao.createAnnouncement(ADMIN_USER, announcementString, startingDate, endingDate))
//      }
//    }
//
//    "Part 2-user cannot add album before starting date" in new NormalUserContext {
//      new WithApplication(application) {
//        val Some(addAlbumRoute) = route(app, FakeRequest(routes.AlbumController.add())
//          .withAuthenticator[MyEnv](NORMAL_USER.loginInfo))
//        status(addAlbumRoute) mustBe OK
//        contentAsString(addAlbumRoute) must include(Messages("album.notallowed"))
//      }
//    }
//
//    "Part 3-user cannot edit album before starting date" in new NormalUserContext {
//      new WithApplication(application) {
//        //adding part
//        val album = getNewAlbum
//        val userId = userDao.getUserByEmail(NORMAL_USER.email).futureValue.get.id.get
//        albumDao.insertAlbum(album, userId).futureValue mustBe true
//        val albumId = albumDao.retrieveAlbumId(album.artist, album.title, userId).futureValue
//
//        val Some(editAlbumRoute) = route(app, FakeRequest(routes.AlbumController.edit(albumId.get))
//          .withAuthenticator[MyEnv](NORMAL_USER.loginInfo))
//        status(editAlbumRoute) mustBe OK
//        contentAsString(editAlbumRoute) must include(Messages("album.notallowed"))
//      }
//    }
//
//    "Part 4-user cannot remove album before starting date" in new NormalUserContext {
//      new WithApplication(application) {
//
//        val album = getNewAlbum
//        val userId = userDao.getUserByEmail(NORMAL_USER.email).futureValue.get.id.get
//        albumDao.insertAlbum(album, userId).futureValue mustBe true
//        val albumId = albumDao.retrieveAlbumId(album.artist, album.title, userId).futureValue
//        albumId.isDefined mustBe true
//
//        val Some(deleteAlbumRoute) = route(app, FakeRequest(routes.AlbumController.delete(albumId.get))
//          .withAuthenticator[MyEnv](NORMAL_USER.loginInfo))
//        status(deleteAlbumRoute) mustBe OK
//        contentAsString(deleteAlbumRoute) must include(Messages("album.notallowed"))
//      }
//    }
//
//    "Part 1-user cannot add album after deadline" in new MasterUserContext {
//      new WithApplication(application) {
//        val now = DateTime.now()
//        val announcementString = "announcement testing 2"
//        val startingDate = now.minusDays(2)
//        val endingDate = now.minusDays(1)
//        await(adminToolDao.createAnnouncement(ADMIN_USER, announcementString, startingDate, endingDate))
//      }
//    }
//
//    "Part 2-user cannot add album after deadline" in new NormalUserContext {
//      new WithApplication(application) {
//        val Some(addAlbumRoute) = route(app, FakeRequest(routes.AlbumController.add())
//          .withAuthenticator[MyEnv](NORMAL_USER.loginInfo))
//        status(addAlbumRoute) mustBe OK
//        contentAsString(addAlbumRoute) must include(Messages("album.notallowed"))
//      }
//    }
//
//    "Part 3-user cannot edit album after deadline" in new NormalUserContext {
//      new WithApplication(application) {
//        //adding part
//        val album = getNewAlbum
//        val userId = userDao.getUserByEmail(NORMAL_USER.email).futureValue.get.id.get
//        albumDao.insertAlbum(album, userId).futureValue mustBe true
//        val albumId = albumDao.retrieveAlbumId(album.artist, album.title, userId).futureValue
//
//        val Some(editAlbumRoute) = route(app, FakeRequest(routes.AlbumController.edit(albumId.get))
//          .withAuthenticator[MyEnv](NORMAL_USER.loginInfo))
//        status(editAlbumRoute) mustBe OK
//        contentAsString(editAlbumRoute) must include(Messages("album.notallowed"))
//      }
//    }
//
//    "Part 4-user cannot remove album after deadline" in new NormalUserContext {
//      new WithApplication(application) {
//
//        val album = getNewAlbum
//        val userId = userDao.getUserByEmail(NORMAL_USER.email).futureValue.get.id.get
//        albumDao.insertAlbum(album, userId).futureValue mustBe true
//        val albumId = albumDao.retrieveAlbumId(album.artist, album.title, userId).futureValue
//        albumId.isDefined mustBe true
//
//        val Some(deleteAlbumRoute) = route(app, FakeRequest(routes.AlbumController.delete(albumId.get))
//          .withAuthenticator[MyEnv](NORMAL_USER.loginInfo))
//        status(deleteAlbumRoute) mustBe OK
//        contentAsString(deleteAlbumRoute) must include(Messages("album.notallowed"))
//      }
//    }
//
//    "Part 1-preparing for user with valid time" in new MasterUserContext {
//      new WithApplication(application) {
//        val now = DateTime.now()
//        val announcementString = "announcement testing 3"
//        val startingDate = now.minusDays(1)
//        val endingDate = now.plusDays(1)
//        await(adminToolDao.createAnnouncement(ADMIN_USER, announcementString, startingDate, endingDate))
//        await(adminToolDao.getStartingDate).get.get.compareTo(startingDate) mustBe 0
//        await(adminToolDao.getEndingDate).get.get.compareTo(endingDate) mustBe 0
//        await(adminToolDao.getAnnouncement).get.get.compareTo(announcementString) mustBe 0
//
//      }
//    }
//
//    "Part 2-user can add album during valid time" in new NormalUserContext {
//      new WithApplication(application) {
//        val Some(addAlbumRoute) = route(app, FakeRequest(routes.AlbumController.add())
//          .withAuthenticator[MyEnv](NORMAL_USER.loginInfo))
//        status(addAlbumRoute) mustBe OK
//        contentAsString(addAlbumRoute) must not include Messages("album.notallowed")
//      }
//    }
//
//    "Part 3-user can edit album during valid time" in new NormalUserContext {
//      new WithApplication(application) {
//        //adding part
//        val album = getNewAlbum
//        val userId = userDao.getUserByEmail(NORMAL_USER.email).futureValue.get.id.get
//        albumDao.insertAlbum(album, userId).futureValue mustBe true
//        val albumId = albumDao.retrieveAlbumId(album.artist, album.title, userId).futureValue
//
//        val Some(editAlbumRoute) = route(app, FakeRequest(routes.AlbumController.edit(albumId.get))
//          .withAuthenticator[MyEnv](NORMAL_USER.loginInfo))
//        status(editAlbumRoute) mustBe OK
//        contentAsString(editAlbumRoute) must not include (Messages("album.notallowed"))
//      }
//    }
//
//    "Part 4-user can delete album during valid time" in new NormalUserContext {
//      new WithApplication(application) {
//        val album = getNewAlbum
//        val userId = userDao.getUserByEmail(NORMAL_USER.email).futureValue.get.id.get
//        albumDao.insertAlbum(album, userId).futureValue
//        val albumId = albumDao.retrieveAlbumId(album.artist, album.title, userId).futureValue
//
//        val Some(deleteAlbumRoute) = route(app, FakeRequest(routes.AlbumController.
//          delete(albumId.get)).withAuthenticator[MyEnv](NORMAL_USER.loginInfo))
//        status(deleteAlbumRoute) mustBe SEE_OTHER
//        redirectLocation(deleteAlbumRoute) mustBe Some(routes.UserController.user().url)
//
//        albumDao.retrieveAlbumId(album.artist, album.title, userId).futureValue.isDefined mustBe false
//      }
//    }
//  }
//
//
//  trait NormalUserContext {
//
//    class FakeModule extends AbstractModule with ScalaModule {
//      def configure() = {
//        bind[Environment[MyEnv]].toInstance(env)
//      }
//    }
//
//    private val NORMAL_USER_EMAIL = "xyz@xyz.com"
//
//    //delete user
//    userDao.deleteUserByEmail(NORMAL_USER_EMAIL).futureValue
//
//    private val admin = User(Some(1), NORMAL_USER_EMAIL, "password", "username", Role.Admin, true)
//    userDao.insertUserWithHashPassword(admin).futureValue
//    val NORMAL_USER: User = userDao.getUserByEmail(NORMAL_USER_EMAIL).futureValue.get
//
//
//    implicit val env: Environment[MyEnv] = new FakeEnvironment[MyEnv](Seq(NORMAL_USER.loginInfo -> NORMAL_USER))
//
//    lazy val application = new GuiceApplicationBuilder().overrides(new FakeModule()).build
//  }
//
//  trait MasterUserContext {
//
//    class FakeModule extends AbstractModule with ScalaModule {
//      def configure() = {
//        bind[Environment[MyEnv]].toInstance(env)
//      }
//    }
//
//    private val ADMIN_EMAIL = "abc@abc.com"
//    //delete user
//    userDao.deleteUserByEmail(ADMIN_EMAIL).futureValue
//
//    private val admin = User(Some(1), ADMIN_EMAIL, "password", "username", Role.Admin, true)
//    userDao.insertUserWithHashPassword(admin).futureValue
//    val ADMIN_USER: User = userDao.getUserByEmail(ADMIN_EMAIL).futureValue.get
//
//    implicit val env: Environment[MyEnv] = new FakeEnvironment[MyEnv](Seq(ADMIN_USER.loginInfo -> ADMIN_USER))
//    lazy val application = new GuiceApplicationBuilder().overrides(new FakeModule()).build
//  }
//
//}