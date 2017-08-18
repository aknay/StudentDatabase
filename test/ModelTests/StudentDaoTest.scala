package ModelTests

/**
  * Created by aknay on 4/4/17.
  */

import dao.{StudentDao, UserDao}
import org.joda.time.DateTime
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
      event = "some event", id = Some(1), updateBy = Some(1), lastUpdateTime = Some(new DateTime()))
  }


  def userDao(implicit app: Application) = {
    val app2UserDAO = Application.instanceCache[UserDao]
    app2UserDAO(app)
  }

  def studentDao(implicit app: Application) = {
    val app2UserDAO = Application.instanceCache[StudentDao]
    app2UserDAO(app)
  }

  "should add user" in new WithApplication() {
    val result = userDao.insertUser(getNormalUser).futureValue
    result === true
    val user = userDao.getUserByEmail(getNormalUser.email).futureValue
    user.isDefined === true

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






}

