package ModelTests

/**
  * Created by aknay on 4/4/17.
  */

import java.util.Date

import dao.{StudentDao, UserDao}
import utils.Utils
//import org.joda.time.DateTime
import org.scalatest.concurrent.ScalaFutures
import org.specs2.mutable.Specification
import play.api.Application
import play.api.test.WithApplication

class StudentDaoTest extends Specification with ScalaFutures {

  import models._

  def getNormalUser: User = {
    User(Some(1), "user@user.com", "password", "username", Role.NormalUser, activated = true)
  }

  def getStudent: Student = {
    Student(name = "batman", teamName = "league", institution = "some institution",
      country = "some country", league = "some league", subLeague = "some subleague",
      event = "some event", id = Some(1), updateBy = Some(1), lastUpdateTime = Some(Utils.getTimeStampFromDate(new Date())))
  }


  def userDao(implicit app: Application) = {
    val app2UserDAO = Application.instanceCache[UserDao]
    app2UserDAO(app)
  }

  def studentDao(implicit app: Application) = {
    val app2UserDAO = Application.instanceCache[StudentDao]
    app2UserDAO(app)
  }

  "just create a table" in new WithApplication() {
    studentDao.createTableIfNotExisted
  }

  "should add user" in new WithApplication() {
    val result = userDao.insertUser(getNormalUser).futureValue
//    result === true
    val user = userDao.getUserByEmail(getNormalUser.email).futureValue
//    user.isDefined === true

    studentDao.deleteStudentByName(getStudent.name).futureValue

    val isAdded = studentDao.insertByUser(getStudent, user.get.id.get).futureValue
    isAdded === true
    val studentList = studentDao.getAllStudents().futureValue
    studentList.head.name === getStudent.name

    //clean up
    userDao.deleteUserByEmail(getNormalUser.email).futureValue
    studentDao.deleteStudentByName(getStudent.name).futureValue
  }

  "should not add user when user is existed" in new WithApplication() {

    val result = userDao.insertUser(getNormalUser).futureValue
    result === true
    val user = userDao.getUserByEmail(getNormalUser.email).futureValue
    user.isDefined === true

    studentDao.deleteStudentByName(getStudent.name).futureValue

    val isAdded = studentDao.insertByUser(getStudent, user.get.id.get).futureValue
    isAdded === true

    val isAddedAgain = studentDao.insertByUser(getStudent, user.get.id.get).futureValue
    isAddedAgain === false

    //clean up
    userDao.deleteUserByEmail(getNormalUser.email).futureValue
    studentDao.deleteStudentByName(getStudent.name).futureValue
  }

  "should get unique league list" in new WithApplication() {
    val allStudents =studentDao.getAllStudents().futureValue
    allStudents.foreach(student => studentDao.deleteStudentByName(student.name).futureValue)
    userDao.insertUser(getNormalUser).futureValue
    val student1 = Student(name = "batman1", teamName = "league", institution = "some institution",
      country = "", league = "some league", subLeague = "some subleague",
      event = "some event", id = Some(1), updateBy = Some(1), lastUpdateTime = Some(Utils.getTimeStampFromDate(new Date())))

    val student2 = Student(name = "batman2", teamName = "league", institution = "some institution",
      country = "", league = "some league", subLeague = "some subleague",
      event = "some event", id = Some(1), updateBy = Some(1), lastUpdateTime = Some(Utils.getTimeStampFromDate(new Date())))

    val student3 = Student(name = "batman3", teamName = "league", institution = "some institution",
      country = "", league = "some league 1", subLeague = "some subleague 1",
      event = "some event", id = Some(1), updateBy = Some(1), lastUpdateTime = Some(Utils.getTimeStampFromDate(new Date())))

    val student4 = Student(name = "batman4", teamName = "league", institution = "some institution",
      country = "some country", league = "some league 1", subLeague = "some subleague 2",
      event = "some event", id = Some(1), updateBy = Some(1), lastUpdateTime = Some(Utils.getTimeStampFromDate(new Date())))

    val user = userDao.getUserByEmail(getNormalUser.email).futureValue
    studentDao.insertByUser(student1, user.get.id.get).futureValue
    studentDao.insertByUser(student2, user.get.id.get).futureValue
    studentDao.insertByUser(student3, user.get.id.get).futureValue
    studentDao.insertByUser(student4, user.get.id.get).futureValue

    val leagueList = studentDao.geLeagueList.futureValue
    leagueList.size === 3
    println(leagueList)

//    val allStudentsToDelete =studentDao.getAllStudents().futureValue
//    allStudentsToDelete.foreach(student => studentDao.deleteStudentByName(student.name).futureValue)


  }






}

