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
    event = Some("some event"), id = Some(1), updateBy = Some(1), lastUpdateTime = Some(Utils.getTimeStampFromDate(new Date())))

  val student2 = Student(name = "student2", teamName = "league", institution = "some institution",
    country = "", league = leagueOne.league, subLeague = leagueOne.subLeague,
    event = Some("some event"), id = Some(1), updateBy = Some(1), lastUpdateTime = Some(Utils.getTimeStampFromDate(new Date())))

  val student3 = Student(name = "student3", teamName = "league", institution = "some institution",
    country = "", league = leagueTwo.league, subLeague = leagueTwo.subLeague,
    event = Some("some event"), id = Some(1), updateBy = Some(1), lastUpdateTime = Some(Utils.getTimeStampFromDate(new Date())))

  val student4 = Student(name = "student4", teamName = "league", institution = "some institution",
    country = "some country", league = leagueThree.league, subLeague = leagueThree.subLeague,
    event = Some("some event"), id = Some(1), updateBy = Some(1), lastUpdateTime = Some(Utils.getTimeStampFromDate(new Date())))

  val student5 = Student(name = "student5", teamName = "league", institution = "some institution",
    country = "some country", league = leagueFour.league, subLeague = leagueFour.subLeague,
    event = Some("some event"), id = Some(1), updateBy = Some(1), lastUpdateTime = Some(Utils.getTimeStampFromDate(new Date())))

  val student6 = Student(name = "student6", teamName = "league", institution = "some institution",
    country = "some country", league = leagueFive.league, subLeague = leagueFive.subLeague,
    event = Some("some event"), id = Some(1), updateBy = Some(1), lastUpdateTime = Some(Utils.getTimeStampFromDate(new Date())))

  def getNormalUser: User = {
    User(Some(1), "user@user.com", "password", "username", Role.NormalUser, activated = true)
  }

  def getStudent: Student = {
    Student(name = "batman", teamName = "league", institution = "some institution",
      country = "some country", league = "some league", subLeague = "some subleague",
      event = Some("some event"), id = Some(1), updateBy = Some(1), lastUpdateTime = Some(Utils.getTimeStampFromDate(new Date())))
  }

  override def afterEach(): Unit = {
    userDao.deleteUserByEmail(getNormalUser.email).futureValue
    studentDao.deleteStudentByName(getStudent.name).futureValue
  }

  override def beforeEach(): Unit = {
    userDao.deleteUserByEmail(getNormalUser.email).futureValue
    studentDao.deleteStudentByName(getStudent.name).futureValue
  }

  val userDao: UserDao = app.injector.instanceOf(classOf[UserDao])
  val studentDao: StudentDao = app.injector.instanceOf(classOf[StudentDao])

  "should add student" in {
    val isUserInserted = userDao.insertUser(getNormalUser).futureValue
    isUserInserted mustBe true
    val user = userDao.getUserByEmail(getNormalUser.email).futureValue
    val isAdded = studentDao.insertByUserId(getStudent, user.get.id.get).futureValue
    isAdded === true
    val studentList = studentDao.getAllStudents().futureValue
    studentList.head.name === getStudent.name
  }

  "should not add user when user is existed" in {
        userDao.insertUser(getNormalUser).futureValue
        val user = userDao.getUserByEmail(getNormalUser.email).futureValue

        val isAdded = studentDao.insertByUserId(getStudent, user.get.id.get).futureValue
        isAdded === true

        val isAddedAgain = studentDao.insertByUserId(getStudent, user.get.id.get).futureValue
        isAddedAgain === false
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

        val leagueList = studentDao.getLeagueList.futureValue
        leagueList.size === 5
        println(leagueList)

        val allStudentsToDelete = studentDao.getAllStudents().futureValue
        allStudentsToDelete.foreach(student => studentDao.deleteStudentByName(student.name).futureValue)
  }


    "should get students from each league" in  {
      userDao.insertUser(getNormalUser).futureValue

      val user = userDao.getUserByEmail(getNormalUser.email).futureValue
      studentDao.insertByUserId(student1, user.get.id.get).futureValue
      studentDao.insertByUserId(student2, user.get.id.get).futureValue
      studentDao.insertByUserId(student3, user.get.id.get).futureValue
      studentDao.insertByUserId(student4, user.get.id.get).futureValue
      studentDao.insertByUserId(student5, user.get.id.get).futureValue
      studentDao.insertByUserId(student6, user.get.id.get).futureValue

      val leagueList = studentDao.getLeagueList.futureValue
      leagueList.size === 5

      val studentsFromLeagueOne = studentDao.getStudentsPerLeague(leagueOne).futureValue
      studentsFromLeagueOne.students.size === 2

      val studentsFromLeagueTwo = studentDao.getStudentsPerLeague(leagueTwo).futureValue
      studentsFromLeagueTwo.students.size === 1

      val allStudentsToDelete = studentDao.getAllStudents().futureValue
      allStudentsToDelete.foreach(student => studentDao.deleteStudentByName(student.name).futureValue)
    }

}