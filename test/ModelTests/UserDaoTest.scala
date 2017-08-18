package ModelTests

/**
  * Created by aknay on 4/4/17.
  */

import dao.UserDao
import org.scalatest.concurrent.ScalaFutures
import org.specs2.mutable.Specification
import play.api.Application
import play.api.test.{WithApplication, WithApplicationLoader}

class UserDaoTest extends Specification with ScalaFutures {

  import models._

  def getNormalUser: User = {
    User(Some(1), "user@user.com", "password", "username", Role.NormalUser, activated = true)
  }

  def userDao(implicit app: Application) = {
    val app2UserDAO = Application.instanceCache[UserDao]
    app2UserDAO(app)
  }

  "should delete all user" in new WithApplication() {
    userDao.deleteAllUsers.futureValue
  }

  "should add user" in new WithApplication() {
    val result = userDao.insertUser(getNormalUser).futureValue
    result === true
    val user = userDao.getUserByEmail(getNormalUser.email).futureValue
    user.isDefined === true
    //clean up
    userDao.deleteUserByEmail(getNormalUser.email).futureValue
  }

  "should add user" in new WithApplication() {
    val result = userDao.insertUser(getNormalUser).futureValue
    result === true
    val user = userDao.getUserByEmail(getNormalUser.email).futureValue
    user.isDefined === true
    //clean up
    userDao.deleteUserByEmail(getNormalUser.email).futureValue
  }

  "should remove user" in new WithApplication() {
    val isInserted = userDao.insertUser(getNormalUser).futureValue
    isInserted === true
    userDao.deleteUserByEmail(getNormalUser.email).futureValue
    val result = userDao.getUserByEmail(getNormalUser.email).futureValue
    result === None
  }

  "should also add user info when user is added" in new WithApplicationLoader() {
    userDao.deleteUserByEmail(getNormalUser.email).futureValue

    userDao.insertUser(getNormalUser).futureValue
    val insertedUser = userDao.getUserByEmail(getNormalUser.email).futureValue
    insertedUser.isDefined === true
    val userInfo = userDao.getUserInfo(insertedUser.get).futureValue
    userInfo.isDefined === true

    //clean up
    userDao.deleteUserByEmail(getNormalUser.email).futureValue
  }

  "should get user info" in new WithApplication() {
    userDao.insertUser(getNormalUser).futureValue
    val insertedUser = userDao.getUserByEmail(getNormalUser.email).futureValue
    val userInfo = userDao.getUserInfo(insertedUser.get).futureValue
    userInfo.isDefined === true
    userInfo.get.name === "EMPTY"
    userInfo.get.location === "EMPTY"

    //clean up
    userDao.deleteUserByEmail(getNormalUser.email).futureValue
  }

  "should modified user info" in new WithApplication() {
    userDao.insertUser(getNormalUser).futureValue
    val insertedUser = userDao.getUserByEmail(getNormalUser.email).futureValue
    val userInfo = userDao.getUserInfo(insertedUser.get).futureValue
    userDao.updateUserInfo(insertedUser.get, "User", "planet").futureValue
    val updatedUserInfo = userDao.getUserInfo(insertedUser.get).futureValue
    updatedUserInfo.get.name === "User"
    updatedUserInfo.get.location === "planet"

    //clean up
    userDao.deleteUserByEmail(getNormalUser.email).futureValue
  }

  "should get non admin user list" in new WithApplication() {
    val user1 = User(Some(1), "user1@user.com", "password", "username", Role.NormalUser, activated = true)
    val admin1 = User(Some(2), "admin1@user.com", "password", "username", Role.Admin, activated = true)
    val admin2 = User(Some(3), "admin2@user.com", "password", "username", Role.Admin, activated = true)

    userDao.insertUser(user1).futureValue
    userDao.insertUser(admin1).futureValue
    userDao.insertUser(admin2).futureValue

    val nonAdminUserList: Seq[User] = userDao.getNonAdminUserList().futureValue

    nonAdminUserList.size === 1
    nonAdminUserList.foreach(println)

    val admin1Retrieved = userDao.getUserByEmail(admin1.email).futureValue
    val admin2Retrieved = userDao.getUserByEmail(admin2.email).futureValue
    val user1Retrieved = userDao.getUserByEmail(user1.email).futureValue

    nonAdminUserList.head.email === user1.email

    //clean up
    userDao.deleteUserByEmail(user1.email)
    userDao.deleteUserByEmail(admin1.email)
    userDao.deleteUserByEmail(admin2.email)
  }

}

