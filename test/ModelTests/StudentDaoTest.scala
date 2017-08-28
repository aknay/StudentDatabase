package ModelTests

/**
  * Created by aknay on 4/4/17.
  */

import java.util.Date

import dao.{StudentDao, UserDao}
import org.scalatest.BeforeAndAfterEach
import org.scalatestplus.play.PlaySpec
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import utils.Utils
import org.scalatest.concurrent.ScalaFutures

class StudentDaoTest extends PlaySpec with BeforeAndAfterEach with GuiceOneAppPerSuite with ScalaFutures {

  import models._

  val leagueOne = LeagueInfo("some league", "some subLeague")
  val leagueTwo = LeagueInfo("some league 1", "some subLeague 1")
  val leagueThree = LeagueInfo("some league 1", "some subLeague 2")
  val leagueFour = LeagueInfo("some league 3", "some subLeague 5")
  val leagueFive = LeagueInfo("some league 4", "some subLeague 5")

  val student1 = Student(name = "student1", teamName = "league", institution = "some institution",
    country = "", league = leagueOne.league, subLeague = leagueOne.subLeague,
    event = "some event", id = Some(1), updateBy = Some(1), lastUpdateTime = Some(Utils.getTimeStampFromDate(new Date())))

  val student2 = Student(name = "student2", teamName = "league", institution = "some institution",
    country = "", league = leagueOne.league, subLeague = leagueOne.subLeague,
    event = "some event", id = Some(1), updateBy = Some(1), lastUpdateTime = Some(Utils.getTimeStampFromDate(new Date())))

  val student3 = Student(name = "student3", teamName = "league", institution = "some institution",
    country = "", league = leagueTwo.league, subLeague = leagueTwo.subLeague,
    event = "some event", id = Some(1), updateBy = Some(1), lastUpdateTime = Some(Utils.getTimeStampFromDate(new Date())))

  val student4 = Student(name = "student4", teamName = "league", institution = "some institution",
    country = "some country", league = leagueThree.league, subLeague = leagueThree.subLeague,
    event = "some event", id = Some(1), updateBy = Some(1), lastUpdateTime = Some(Utils.getTimeStampFromDate(new Date())))

  val student5 = Student(name = "student5", teamName = "league", institution = "some institution",
    country = "some country", league = leagueFour.league, subLeague = leagueFour.subLeague,
    event = "some event", id = Some(1), updateBy = Some(1), lastUpdateTime = Some(Utils.getTimeStampFromDate(new Date())))

  val student6 = Student(name = "student6", teamName = "league", institution = "some institution",
    country = "some country", league = leagueFive.league, subLeague = leagueFive.subLeague,
    event = "some event", id = Some(1), updateBy = Some(1), lastUpdateTime = Some(Utils.getTimeStampFromDate(new Date())))

  def getNormalUser: User = {
    User(Some(1), "user@user.com", "password", "username", Role.NormalUser, activated = true)
  }

  def getStudent: Student = {
    Student(name = "batman", teamName = "league", institution = "some institution",
      country = "some country", league = "some league", subLeague = "some subleague",
      event = "some event", id = Some(1), updateBy = Some(1), lastUpdateTime = Some(Utils.getTimeStampFromDate(new Date())))
  }

  override def afterEach(): Unit = {
    userDao.deleteUserByEmail(getNormalUser.email).futureValue
    studentDao.deleteStudentByName(getStudent.name).futureValue

    val allStudentsToDelete = studentDao.getAllStudents().futureValue
    allStudentsToDelete.foreach(student => studentDao.deleteStudentByName(student.name).futureValue)
  }

  override def beforeEach(): Unit = {
    userDao.deleteUserByEmail(getNormalUser.email).futureValue
    studentDao.deleteStudentByName(getStudent.name).futureValue

    val allStudentsToDelete = studentDao.getAllStudents().futureValue
    allStudentsToDelete.foreach(student => studentDao.deleteStudentByName(student.name).futureValue)
  }

  val userDao: UserDao = app.injector.instanceOf(classOf[UserDao])
  val studentDao: StudentDao = app.injector.instanceOf(classOf[StudentDao])

  "should add student" in {
    val isUserInserted = userDao.insertUser(getNormalUser).futureValue
    isUserInserted mustBe true
    val user = userDao.getUserByEmail(getNormalUser.email).futureValue
    val isAdded = studentDao.insertByUserId(getStudent, user.get.id.get).futureValue
    isAdded mustBe true
    val studentList = studentDao.getAllStudents().futureValue
    studentList.head.name mustBe getStudent.name
  }

  "should add student with same name for different event" in {
    val isUserInserted = userDao.insertUser(getNormalUser).futureValue
    isUserInserted mustBe true
    val user = userDao.getUserByEmail(getNormalUser.email).futureValue
    val isAddedWithEventAbc = studentDao.insertByUserId(student1.copy(event = "abc"), user.get.id.get).futureValue
    val isAddedWithEventXyz = studentDao.insertByUserId(student1.copy(event = "xyz"), user.get.id.get).futureValue
    isAddedWithEventAbc mustBe true
    isAddedWithEventXyz mustBe true
  }

  "should not add user when user is existed" in {
    userDao.insertUser(getNormalUser).futureValue
    val user = userDao.getUserByEmail(getNormalUser.email).futureValue

    val isAdded = studentDao.insertByUserId(getStudent, user.get.id.get).futureValue
    isAdded mustBe true

    val isAddedAgain = studentDao.insertByUserId(getStudent, user.get.id.get).futureValue
    isAddedAgain mustBe false
  }


  "should get unique league list" in {
    userDao.insertUser(getNormalUser).futureValue
    val user = userDao.getUserByEmail(getNormalUser.email).futureValue

    studentDao.insertByUserId(student1, user.get.id.get).futureValue
    studentDao.insertByUserId(student2, user.get.id.get).futureValue
    studentDao.insertByUserId(student3, user.get.id.get).futureValue
    studentDao.insertByUserId(student4, user.get.id.get).futureValue
    studentDao.insertByUserId(student5, user.get.id.get).futureValue
    studentDao.insertByUserId(student6, user.get.id.get).futureValue

    val leagueList = studentDao.getLeagueListByEvent(student1.event).futureValue
    leagueList.size mustBe 5
  }


  "should get students from each league" in {
    userDao.insertUser(getNormalUser).futureValue

    val user = userDao.getUserByEmail(getNormalUser.email).futureValue
    studentDao.insertByUserId(student1, user.get.id.get).futureValue
    studentDao.insertByUserId(student2, user.get.id.get).futureValue
    studentDao.insertByUserId(student3, user.get.id.get).futureValue
    studentDao.insertByUserId(student4, user.get.id.get).futureValue
    studentDao.insertByUserId(student5, user.get.id.get).futureValue
    studentDao.insertByUserId(student6, user.get.id.get).futureValue

    val leagueList = studentDao.getLeagueListByEvent(student1.event).futureValue
    leagueList.size mustBe 5

    val studentsFromLeagueOne = studentDao.getStudentsPerLeague(student1.event, leagueOne).futureValue
    studentsFromLeagueOne.students.size mustBe 2

    val studentsFromLeagueTwo = studentDao.getStudentsPerLeague(student1.event, leagueTwo).futureValue
    studentsFromLeagueTwo.students.size mustBe 1

    val allStudentsToDelete = studentDao.getAllStudents().futureValue
    allStudentsToDelete.foreach(student => studentDao.deleteStudentByName(student.name).futureValue)
  }

  "should get unique event list" in {
    userDao.insertUser(getNormalUser).futureValue

    val user = userDao.getUserByEmail(getNormalUser.email).futureValue

    studentDao.insertByUserId(student1.copy(event = "abc"), user.get.id.get).futureValue
    studentDao.insertByUserId(student2.copy(event = "abc"), user.get.id.get).futureValue
    studentDao.insertByUserId(student3.copy(event = "def"), user.get.id.get).futureValue
    studentDao.insertByUserId(student4.copy(event = "def"), user.get.id.get).futureValue
    studentDao.insertByUserId(student5.copy(event = "ghi"), user.get.id.get).futureValue
    studentDao.insertByUserId(student6.copy(event = "xyz"), user.get.id.get).futureValue

    val eventList = studentDao.getUniqueEventList.futureValue
    eventList.size mustBe 4
  }

  "should get student list from unique event" in {
    userDao.insertUser(getNormalUser).futureValue

    val user = userDao.getUserByEmail(getNormalUser.email).futureValue


    studentDao.insertByUserId(student1.copy(event = "abc"), user.get.id.get).futureValue
    studentDao.insertByUserId(student2.copy(event = "abc"), user.get.id.get).futureValue
    studentDao.insertByUserId(student3.copy(event = "def"), user.get.id.get).futureValue
    studentDao.insertByUserId(student4.copy(event = "def"), user.get.id.get).futureValue
    studentDao.insertByUserId(student5.copy(event = "ghi"), user.get.id.get).futureValue
    studentDao.insertByUserId(student6.copy(event = "xyz"), user.get.id.get).futureValue

    val eventList = studentDao.getUniqueEventList.futureValue

    val studentList = studentDao.getStudentsPerEvent(eventList.head).futureValue
    studentList.head.event mustBe "abc"
    studentList.size mustBe 2

    val lastStudentList = studentDao.getStudentsPerEvent(eventList.last).futureValue
    lastStudentList.head.event mustBe "xyz"
    lastStudentList.size mustBe 1
  }


}