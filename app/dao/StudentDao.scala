package dao


import java.util.Date
import javax.inject.{Inject, Singleton}

import models._
import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfigProvider}
import slick.jdbc.JdbcProfile
import slick.jdbc.meta.MTable
import utils.Utils

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._
import scala.math.BigDecimal.RoundingMode


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

  createTableIfNotExisted

  /** Ref: http://slick.lightbend.com/doc/3.0.0/database.html */

  //This is the blocking method with maximum waiting time of 2 seconds
  //This is also helper method for DBIO
  private def blockExec[T](action: DBIO[T]): T = Await.result(db.run(action), 5 seconds)

  def createTableIfNotExisted {
    val x = blockExec(MTable.getTables(STUDENT_TABLE_NAME.toString())).toList
    if (x.isEmpty) {
      blockExec(createTableAction)
    }
  }

  def getUserTable: Future[Seq[User]] = db.run(userTable.result)

  def insertByUser(student: Student, id: Long): Future[Boolean] = {
    getStudentByName(student.name).map{
      case Some(s) => false
      case None =>
        db.run(studentTable += student.copy(updateBy = Some(id), lastUpdateTime = Some(Utils.getTimeStampFromDate(new Date()))))
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

  def getLeagueList: Future[Seq[LeagueInfo]] = {
       for {
        l <- getAllStudents()
        a <- Future.successful(l.map{b => LeagueInfo(b.league, b.subLeague)})
      } yield a.distinct
  }

  def getTeamWithCountryList: Future[Seq[TeamWithCountry]] = {
    for {
      l <- getAllStudents()
      a <- Future.successful(l map{b => TeamWithCountry(b.teamName, b.country)})
    } yield a.distinct
  }

  private def teamSize(students: Seq[Student]): Int ={
  students.map(s => s.teamName).distinct.size
  }

  private def localTeamSize(students: Seq[Student]): Int = {
    val localCountry = "Singapore"
    students.filter( s => s.country.compareToIgnoreCase(localCountry) == 0)
      .map( t => t.teamName).distinct.size
  }

  private def internationalTeamSize(students: Seq[Student]): Int = {
    teamSize(students) - localTeamSize(students)
  }

  private def getRoundedPercentage(first: Int, second: Int): Double = {
    val double = first.toDouble * 100.0/ second.toDouble
    val bd = BigDecimal(double)
    bd.setScale(1, RoundingMode.HALF_UP).toDouble
  }

  def getStudentsPerLeague(leagueInfo: LeagueInfo): Future[StudentsPerLeague] = {
    val studentsPerLeague: Future[Seq[Student]] = db.run(studentTable
      .filter(_.league === leagueInfo.league)
      .filter(_.subLeague === leagueInfo.subLeague)
      .result)
    
     for {
      totalStudents <- getAllStudents()
      totalTeamSize <- Future.successful(teamSize(totalStudents))
      totalStudentSize <- Future.successful(totalStudents.size)
      students <- studentsPerLeague
    } yield StudentsPerLeague(leagueInfo, students, students.size,
      teamSize(students), localTeamSize(students), internationalTeamSize(students),
      getRoundedPercentage(students.size,totalStudentSize), getRoundedPercentage(teamSize(students), totalTeamSize))
  }
}
