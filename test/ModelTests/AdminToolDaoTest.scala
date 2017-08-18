package ModelTests

/**
  * Created by aknay on 4/4/17.
  */

import dao.{AdminToolDao, UserDao}
import org.joda.time.DateTime
import org.scalatest.concurrent.ScalaFutures
import org.specs2.mutable.Specification
import play.api.Application
import play.api.test.{WithApplication, WithApplicationLoader}

class AdminToolDaoTest extends Specification with ScalaFutures {

  import models._

  def getNormalUser = User(Some(1), "normal_user@user.com", "password", "username", Role.NormalUser, activated = true)

  def getAdminUser = User(Some(2), "admin@user.com", "password", "username", Role.Admin, activated = true)


  def userDao(implicit app: Application) = Application.instanceCache[UserDao].apply(app)

  def adminToolDao(implicit app: Application) = Application.instanceCache[AdminToolDao].apply(app)

  def removeAllEvents(implicit app: Application) = {
    userDao.insertUser(getAdminUser).futureValue
    val user = userDao.getUserByEmail(getAdminUser.email).futureValue.get
    val adminTool = adminToolDao.getAdminTool.futureValue.get
    adminToolDao.updateAdminTool(user, adminTool.copy(event = None)).futureValue
  }

  def insertUser()(implicit app: Application): User = {
    userDao.insertUser(getAdminUser).futureValue
    userDao.getUserByEmail(getAdminUser.email).futureValue.get
  }

  def deleteUser(user: User)(implicit app: Application) = {
    userDao.deleteUserByEmail(user.email).futureValue
  }

  "should delete all user" in new WithApplication() {
    userDao.deleteAllUsers.futureValue
  }

  "should create announcement" in new WithApplicationLoader() {
    val announcement = "This is an announcement"
    val startingDate = DateTime.now()
    val endingDate = DateTime.now()
    val user = insertUser()
    adminToolDao.createAnnouncement(user, announcement, startingDate, endingDate).futureValue
    val secondAdminTool = adminToolDao.getAdminTool.futureValue.get
    secondAdminTool.announcement.get.compareTo(announcement) === 0
    secondAdminTool.startingDate.get.compareTo(startingDate) === 0
    secondAdminTool.endingDate.get.compareTo(endingDate) === 0
    deleteUser(user)
  }

  "should delete an announcement" in new WithApplication() {
    val user = insertUser()
    val announcement = "This is an announcement"
    adminToolDao.createAnnouncement(user, announcement, DateTime.now(), DateTime.now()).futureValue
    adminToolDao.deleteAnnouncement(user).futureValue
    val adminTool = adminToolDao.getAdminTool.futureValue
    adminTool.get.announcement === None
    adminTool.get.startingDate === None
    adminTool.get.endingDate === None
    deleteUser(user)
  }

  "should add an event" in new WithApplication() {
    val user = insertUser()
    removeAllEvents
    val firstEvent = "abc event"
    adminToolDao.addEvent(user, firstEvent).futureValue === true
    adminToolDao.getEvent.futureValue.get.get.compareTo(firstEvent) === 0

    //add additional event
    val secondEvent = "def event"
    adminToolDao.addEvent(user, secondEvent).futureValue === true
    val allEvents = firstEvent + "," + secondEvent
    println("allEvents: " + allEvents)
    println("get it from: " + adminToolDao.getEvent.futureValue.get.get)
    adminToolDao.getEvent.futureValue.get.get.compareTo(allEvents) === 0
    //clean up
    removeAllEvents
    deleteUser(user)
  }

  "should delete an event" in new WithApplication() {
    val user = insertUser
    val firstEvent = "abc event"
    val secondEvent = "def event"
    adminToolDao.addEvent(user, firstEvent).futureValue
    adminToolDao.addEvent(user, secondEvent).futureValue

    val isDeleted = adminToolDao.deleteEvent(user, firstEvent).futureValue
    isDeleted === true
    adminToolDao.getEvent.futureValue.get.isDefined === true

    val isDeletedAgain = adminToolDao.deleteEvent(user, secondEvent).futureValue
    isDeletedAgain === true
    adminToolDao.getEvent.futureValue.get.isDefined === false
    //clean up
    removeAllEvents
    deleteUser(user)
  }
}