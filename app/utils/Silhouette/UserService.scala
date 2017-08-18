package utils.Silhouette

import javax.inject.Inject
import scala.concurrent.Future

import com.mohiva.play.silhouette.api.LoginInfo
import com.mohiva.play.silhouette.api.services.IdentityService

import dao.UserDao
import models.User
import Implicits._

/**
  * Created by aknay on 30/1/17.
  */

class UserService @Inject()(userDao: UserDao) extends IdentityService[User] {
  def retrieve(loginInfo: LoginInfo): Future[Option[User]] = userDao.getUserByEmail(loginInfo)

  def save(user: User): Future[User] = userDao.saveUserByLoginInfo(user)   //here we implicitly convert
}
