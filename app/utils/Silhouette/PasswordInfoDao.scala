package utils.Silhouette

import javax.inject.Inject
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

import com.mohiva.play.silhouette.api.util.PasswordInfo
import com.mohiva.play.silhouette.persistence.daos.DelegableAuthInfoDAO
import com.mohiva.play.silhouette.api.LoginInfo

import Implicits._
import dao.UserDao

/**
  * Created by aknay on 1/2/17.
  */


class PasswordInfoDao @Inject()(userDao: UserDao) extends DelegableAuthInfoDAO[PasswordInfo] {

  def add(loginInfo: LoginInfo, authInfo: PasswordInfo): Future[PasswordInfo] =
    update(loginInfo, authInfo)

  def find(loginInfo: LoginInfo): Future[Option[PasswordInfo]] =
    userDao.getUserByEmail(loginInfo).map{
      case Some(user) if user.activated => Some(user.password)
      case _ => None
    }

  def remove(loginInfo: LoginInfo): Future[Unit] = userDao.deleteUserByEmail(loginInfo)

  def save(loginInfo: LoginInfo, authInfo: PasswordInfo): Future[PasswordInfo] =
    find(loginInfo).flatMap {
      case Some(_) => update(loginInfo, authInfo)
      case None => add(loginInfo, authInfo)
    }

  def update(loginInfo: LoginInfo, authInfo: PasswordInfo): Future[PasswordInfo] =
    userDao.getUserByEmail(loginInfo).map {
      case Some(user) => {
        userDao.updateUserByLoginInfo(user.copy(password = authInfo))
        authInfo
      }
      case _ => throw new Exception("PasswordInfoDAO - update : the user must exists to update its password")
    }
}