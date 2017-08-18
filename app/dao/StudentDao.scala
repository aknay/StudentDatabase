package dao

import javax.inject.{Inject, Singleton}

import models.{Student, User}
import org.joda.time.DateTime
import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfigProvider}
import slick.jdbc.JdbcProfile

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._

/**
  * Created by aknay on 27/12/16.
  */
/** change to traits so that other dao can access this user dao */
/** Ref:https://github.com/playframework/play-slick/blob/master/samples/computer-database/app/dao/CompaniesDAO.scala */


@Singleton()
class StudentDao @Inject()(protected val dbConfigProvider: DatabaseConfigProvider) extends StudentTableComponent with HasDatabaseConfigProvider[JdbcProfile] {

  /** describe the structure of the tables: */
  /** Note: table cannot be named as 'user', otherwise we will problem with Postgresql */

  import profile.api._

  /** The following statements are Action */
  private lazy val createTableAction = studentTable.schema.create

  private val selectAlbumAction = studentTable.result

  /** Ref: http://slick.lightbend.com/doc/3.0.0/database.html */

  //This is the blocking method with maximum waiting time of 2 seconds
  //This is also helper method for DBIO
  private def blockExec[T](action: DBIO[T]): T = Await.result(db.run(action), 5 seconds)

  def getUserTable: Future[Seq[User]] = db.run(userTable.result)

  def insertByUser(student: Student, id: Long): Future[Boolean] = {
    getStudentByName(student.name).map{
      case Some(s) => false
      case None =>
        db.run(studentTable += student.copy(updateBy = Some(id), lastUpdateTime = Some(new DateTime())))
        true
    }
  }

  def getAllStudents(): Future[Seq[Student]] = {
    db.run(studentTable.result)
  }

  def getStudentByName(name: String): Future[Option[Student]] = {
    db.run(studentTable.filter(_.name === name).result.headOption)
  }

  def deleteStudentByName(name: String): Future[Unit] = {
    db.run(studentTable.filter(_.name === name).delete).map { _ => () }
  }

}
