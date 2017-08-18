package dao

import javax.inject.Inject
import javax.inject.Singleton

import scala.concurrent._
import scala.concurrent.duration._
import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfigProvider}
import slick.jdbc.JdbcProfile

import scala.concurrent.ExecutionContext.Implicits.global
import slick.jdbc.meta.MTable
import org.mindrot.jbcrypt.BCrypt
import models.{User, UserInfo, Role}

/**
  * Created by aknay on 27/12/16.
  */
/** change to traits so that other dao can access this user dao */
/** Ref:https://github.com/playframework/play-slick/blob/master/samples/computer-database/app/dao/CompaniesDAO.scala */


@Singleton()
class UserDao @Inject()(protected val dbConfigProvider: DatabaseConfigProvider) extends UserTableComponent with HasDatabaseConfigProvider[JdbcProfile] {

  /** describe the structure of the tables: */
  /** Note: table cannot be named as 'user', otherwise we will problem with Postgresql */
  this.createUserTableIfNotExisted
  this.createUserInfoTableIfNotExisted

  import profile.api._

  /** The following statements are Action */
  private lazy val createTableAction = userTable.schema.create

  private val selectAlbumAction = userTable.result

  /** Ref: http://slick.lightbend.com/doc/3.0.0/database.html */

  //This is the blocking method with maximum waiting time of 2 seconds
  //This is also helper method for DBIO
  private def blockExec[T](action: DBIO[T]): T = Await.result(db.run(action), 5 seconds)

  def getUserTable: Future[Seq[User]] = db.run(userTable.result)

  createUserInfoTableIfNotExisted

  def createUserTableIfNotExisted {
    val x = blockExec(MTable.getTables(USER_TABLE_NAME)).toList
    if (x.isEmpty) {
      blockExec(createTableAction)
    }
  }

  def getNonAdminUserList(): Future[Seq[User]] = {
    db.run(userTable.filter(_.role =!= (Role.Admin: Role)).result)
  }

  //for testing purpose only
  def deleteAllUsers : Future[Unit] = {
     val users = db.run(userTable.result)
        for {
          u <- users
        } yield u.foreach(u => deleteUserByEmail(u.email))
  }

  def insertUserWithHashPassword(user: User): Future[Boolean] = {
    val hashedPassword = BCrypt.hashpw(user.password, BCrypt.gensalt())
    val userCopy = user.copy(password = hashedPassword)
    insertUser(userCopy)
  }

  def getUserByEmail(email: String): Future[Option[User]] = {
    db.run(userTable.filter(_.email === email).result.headOption)
  }

  def isUserExisted(email: String): Future[Boolean] = {
    for {
      user <- getUserByEmail(email)
      result <- Future(user.isDefined)
    } yield result
  }

  private val USER_INFO_TABLE_NAME = "userinfotable"

  class UserInfoTable(tag: Tag) extends Table[UserInfo](tag, USER_INFO_TABLE_NAME) {

    def name = column[String]("name")

    def location = column[String]("location")

    def userId = column[Long]("userId")

    def * = (userId, name, location) <> (UserInfo.tupled, UserInfo.unapply)

    def album = foreignKey("user_info_fk", userId, userTable)(_.id, onDelete = ForeignKeyAction.Cascade)

  }

  private lazy val createUserInfoTableAction = userInfoTable.schema.create

  def createUserInfoTableIfNotExisted {
    val x = blockExec(MTable.getTables(USER_INFO_TABLE_NAME)).toList
    if (x.isEmpty) {
      blockExec(createUserInfoTableAction)
    }
  }

  private val insertUser = userTable returning userTable.map(_.id)
  private val userInfoTable = TableQuery[UserInfoTable]

  def insertUser(user: User): Future[Boolean] = {
    isUserExisted(user.email).map {
      case true => {
        false
      }
      case false => {
        val insertAction = for {
          userId <- insertUser += user
          count <- userInfoTable ++= Seq(
            UserInfo(userId, "EMPTY", "EMPTY")
          )
        } yield count
        db.run(insertAction)
        true
      }
    }
  }

  def insertUserInfo(user: User, name: String = "", location: String = ""): Future[Unit] = {
    createUserInfoTableIfNotExisted
    val insertAction = userInfoTable ++= Seq(UserInfo(user.id.get, name, location))
    db.run(insertAction).map { _ => () }
  }

  def saveUserByLoginInfo(user: User): Future[User] = {
    isUserExisted(user.email).flatMap {
      case true => updateUserByLoginInfo(user)
      case false => db.run(insertUser += user).map {
        _ => user
      }
    }
  }

  def getUserInfo(user: User): Future[Option[UserInfo]] = {
    val get = for {
      userId <- userTable.filter(_.id === user.id).result.head
      rowsAffected <- userInfoTable.filter(_.userId === userId.id).result.headOption
    } yield rowsAffected
    db.run(get)
  }

  def updateUserInfo(user: User, name: String, location: String): Future[Int] = {
    val userInfo = for {
      userInfo <- getUserInfo(user)
    } yield userInfo

    userInfo.flatMap {
      case Some(a) => db.run(userInfoTable.filter(_.userId === user.id.get).update(a.copy(name = name, location = location)))
    }
  }

  def deleteUserByEmail(email: String): Future[Unit] = {
    db.run(userTable.filter(_.email === email).delete).map { _ => () }
  }

  def updateUserByLoginInfo(user: User): Future[User] = {
    val userToUpdate = user.copy(password = user.password)
    val update = userTable.filter(_.email === user.email).update(userToUpdate)

    db.run(update).map {
      _ => user
    }
  }

}
