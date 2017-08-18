//package ModelTests
//
///**
//  * Created by aknay on 4/4/17.
//  */
//
//import dao.{AdminToolDao, UserDao}
//import org.joda.time.DateTime
//import org.scalatest.BeforeAndAfterEach
//import org.scalatest.concurrent.ScalaFutures
//import org.scalatestplus.play.PlaySpec
//import org.scalatestplus.play.guice.GuiceOneAppPerSuite
//import play.api.Application
//import play.test.WithApplication
//import scala.concurrent.ExecutionContext.Implicits.global
//
//
//class AdminToolModelTest extends PlaySpec with BeforeAndAfterEach with GuiceOneAppPerSuite with ScalaFutures {
//
//  import models._
//
//  private val ADMIN_EMAIL = "justAdmin@admin.com"
//
//  def getMasterUser: User = {
//    User(Some(1), ADMIN_EMAIL, "just email", "admin", Role.Admin, activated = true)
//  }
//
//  val userDao: UserDao = app.injector.instanceOf(classOf[UserDao])
//  val adminToolDao: AdminToolDao = app.injector.instanceOf(classOf[AdminToolDao])
//
//  def getUser(implicit app: Application): User = {
//    val user = userDao.getUserByEmail(ADMIN_EMAIL).futureValue
//    if (user.isEmpty) {
//      val u = User(Some(1), ADMIN_EMAIL, "just email", "admin", Role.Admin, activated = true)
//      userDao.insertUser(User(Some(1), ADMIN_EMAIL, "just email", "admin", Role.Admin, activated = true)).futureValue
//      return userDao.getUserByEmail(ADMIN_EMAIL).futureValue.get
//    }
//    user.get
//  }
//
//  def removeAllEvents(implicit app: Application) = {
//    val adminTool = adminToolDao.getAdminTool.futureValue.get
//    adminToolDao.updateAdminTool(getUser, adminTool.copy(event = None)).futureValue
//  }
//
//  "preparing and setup" in new WithApplication {
//    userDao.deleteUserByEmail(ADMIN_EMAIL).futureValue
//    val result = userDao.insertUser(getMasterUser).futureValue
//    result mustBe true
//    val user = userDao.getUserByEmail(ADMIN_EMAIL).futureValue
//    user.isDefined mustBe true
//  }
//
//  "should create an announcement" in {
//    val announcement = "This is an announcement"
//    val startingDate = DateTime.now()
//    val endingDate = DateTime.now()
//    adminToolDao.createAnnouncement(getUser, announcement, startingDate, endingDate).futureValue
//
//    val secondAdminTool = adminToolDao.getAdminTool.futureValue.get
//
//    secondAdminTool.announcement.get.compareTo(announcement) mustBe 0
//    secondAdminTool.startingDate.get.compareTo(startingDate) mustBe 0
//    secondAdminTool.endingDate.get.compareTo(endingDate) mustBe 0
//  }
//
//  "should delete announcement" in {
//    val announcement = "This is an announcement"
//    val startingDate = DateTime.now()
//    val endingDate = DateTime.now()
//    adminToolDao.createAnnouncement(getUser, announcement, startingDate, endingDate).futureValue
//
//    val secondAdminTool = adminToolDao.getAdminTool.futureValue.get
//
//    secondAdminTool.announcement.get.compareTo(announcement) mustBe 0
//    secondAdminTool.startingDate.get.compareTo(startingDate) mustBe 0
//    secondAdminTool.endingDate.get.compareTo(endingDate) mustBe 0
//
//    adminToolDao.deleteAnnouncement(getUser).futureValue
//
//    val adminTool = adminToolDao.getAdminTool.futureValue
//    adminTool.get.announcement mustBe None
//    adminTool.get.startingDate mustBe None
//    adminTool.get.endingDate mustBe None
//
//  }
//
//  "should add event" in {
//    removeAllEvents
//    val firstEvent = "abc event"
//
//    adminToolDao.addEvent(getUser, firstEvent).futureValue mustBe true
//    adminToolDao.getEvent.futureValue.get.get.compareTo(firstEvent) mustBe 0
//
//    //add additional event
//    val secondEvent = "def event"
//    adminToolDao.addEvent(getUser, secondEvent).futureValue mustBe true
//    val allEvents = firstEvent + "," + secondEvent
//    println("allEvents: " + allEvents)
//    println("get it from: " + adminToolDao.getEvent.futureValue.get.get)
//    adminToolDao.getEvent.futureValue.get.get.compareTo(allEvents) mustBe 0
//  }
//
//  "should delete an event" in {
//    removeAllEvents
//    val firstEvent = "abc event"
//    val secondEvent = "def event"
//    adminToolDao.addEvent(getUser, firstEvent).futureValue
//    adminToolDao.addEvent(getUser, secondEvent).futureValue
//
//    val isDeleted = adminToolDao.deleteEvent(getUser, firstEvent).futureValue
//    isDeleted mustBe true
//    adminToolDao.getEvent.futureValue.get.isDefined mustBe true
//
//    val isDeletedAgain = adminToolDao.deleteEvent(getUser, secondEvent).futureValue
//    isDeletedAgain mustBe true
//    adminToolDao.getEvent.futureValue.get.isDefined mustBe false
//  }
//}
