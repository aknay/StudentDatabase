package controllers

import java.util.Date
import javax.inject.Inject

import com.github.tototoshi.csv.CSVReader
import com.mohiva.play.silhouette.api.Silhouette
import dao.{StudentDao, UserDao}
import forms.Forms
import models._
import play.api.i18n.{I18nSupport, Messages}
import play.api.mvc.{AbstractController, ControllerComponents, Flash}
import utils.Silhouette.{AuthController, MyEnv}
import utils.Utils

import scala.collection.immutable
import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

/**
  * Created by aknay on 5/12/2016.
  */
/** we need to use I18nSupport because we are using form helper
  * Without this --> Form: could not find implicit value for parameter messages: play.api.i18n.Messages
  * Ref: http://stackoverflow.com/questions/30799988/play-2-4-form-could-not-find-implicit-value-for-parameter-messages-play-api-i
  * */
class StudentController @Inject()(components: ControllerComponents,
                                  studentDao: StudentDao,
                                  userDao: UserDao)
                                 (val silhouette: Silhouette[MyEnv])(implicit exec: ExecutionContext)
  extends AbstractController(components) with I18nSupport with AuthController {
  /** we can use album form directly with Album case class by applying id as Option[Long] */

  def add = SecuredAction.async { implicit request =>
    val user = Some(request.identity)
    Future.successful(Ok(views.html.Student.add(user, Forms.studentForm)))
  }

  def submitStudentForm = SecuredAction.async { implicit request =>
    val user: User = request.identity
    val stduentForm = Forms.studentForm.bindFromRequest()
    stduentForm.fold(
      hasErrors = { form =>
        println("we are having error, try to check form data is matched with html")
        println(form.data)
        Future.successful(Redirect(routes.HomeController.index()))
      },
      success = {
        newAlbum =>
          studentDao.insertByUserId(newAlbum, request.identity.id.get).map {
            case true => Redirect(routes.StudentController.add()) flashing (
              "success" -> Messages("student.added.success"))

            case false => Redirect(routes.StudentController.add()) flashing (Flash(stduentForm.data) +
              ("error" -> Messages("student.alreadyExisted.error")))
          }
      })
  }

  def submitStudentWithCsvForm = SecuredAction.async { implicit request =>
    val user: User = request.identity
    val stduentForm = Forms.studentCsvForm.bindFromRequest()
    stduentForm.fold(
      hasErrors = { form =>
        println("we are having error, try to check form data is matched with html")
        println(form.data)
        Future.successful(Redirect(routes.HomeController.index()))
      },
      success = {
        stduentWithCsvForm =>

          val csv = stduentWithCsvForm._2

          println(csv)

          println("-----------------printing line--------------------------------->")
          val line = csv.split(" ").map(_.trim)
          line.foreach(println)


          val cols: Array[String] = csv.split(",").map(_.trim)
          println("-------------------------------------------------->")
          cols.foreach(println)
      })

    Future.successful(Ok)
  }

  def menu = SecuredAction.async { implicit request =>
    val user = Some(request.identity)
    Future.successful(Ok(views.html.Student.menu(user)))
  }

  def upload = Action(parse.temporaryFile) { request =>
    Ok(views.html.Student.upload())
  }

  def report = SecuredAction.async { implicit request =>
    val user = Some(request.identity)
    Future.successful(Ok(views.html.Student.report(user)))
  }

  def studentsPerLeague = SecuredAction.async { implicit request =>
    val user = Some(request.identity)
    for {
      l <- studentDao.getLeagueList
      studentsPerLeague <- Future.sequence(l map (v => studentDao.getStudentsPerLeague(v)))
      totalSizeInfo <- studentDao.getTotalSizeInfo
    } yield Ok(views.html.Student.numberofstudentsateachleague(user, studentsPerLeague, totalSizeInfo))

  }

  val country = "Country"
  val teamName = "Team Name"
  val institution = "Institution"
  val league = "League"
  val subLeague = "League (Sub-Category)"
  val studentName = "Student Name"

  private def isFollowingFormat(map: Map[String, String]): Boolean = {
    map.get(country).isDefined &&
      map.get(teamName).isDefined &&
      map.get(institution).isDefined &&
      map.get(league).isDefined &&
      map.get(subLeague).isDefined &&
      map.get(studentName).isDefined
  }

  private def getStudentFromMap(userId: Long, map: Map[String, String]): Student = {
    //TODO event is still empty
    Student(Some(1), map(studentName), map(studentName), map(institution), map(country), map(league),
      map(subLeague), Some(""), Some(Utils.getTimeStampFromDate(new Date())), updateBy = Some(userId))
  }

  def uploadCsv = SecuredAction.async(parse.multipartFormData) { implicit request =>
    val user = Some(request.identity)
    val students: Future[Seq[StudentWithStatus]] = request.body.file("csv").map { csv =>
      val reader = CSVReader.open(csv.ref.file)

      val v : Seq[Map[String, String]] = reader.allWithHeaders()

      val userId = user.get.id.get
      val students: Seq[Student] = v.filter(a => isFollowingFormat(a))
        .map(b => getStudentFromMap(userId, b))

      val studentWithStatusList: Seq[Future[StudentWithStatus]] = students.map {
        s =>
          studentDao.insertByUserId(s, userId)
            .map(a => StudentWithStatus(s.name, s.teamName, s.institution, s.country, s.league, s.subLeague, isExisted = !a))
      }
      Future.sequence(studentWithStatusList)
    }.get

    for {
      s <- students
    } yield Ok(views.html.Student.add_student_with_table(user, s))

  }

  private def combineLeagueAndSubLeague(league: String, subLeague: String): String = {
    league + "," + subLeague
  }

  def view(combinedLeagueName: String) = SecuredAction.async { implicit request =>
    val user = Some(request.identity)
    val totalNumberOfCombinedLeague: Future[Seq[(String, String)]] = studentDao.getLeagueList.map {
      a => a.map(b => (combineLeagueAndSubLeague(b.league, b.subLeague), combineLeagueAndSubLeague(b.league, b.subLeague)))
    }

    val submittedLeagueInfo: Option[LeagueInfo] = LeagueInfo.unapply(combinedLeagueName)
    if (submittedLeagueInfo.isEmpty) {
      println("we have empty")
      for {
        t <- totalNumberOfCombinedLeague
      } yield Ok(views.html.Student.view(user, Some(Forms.leagueForm), t, Seq()))
    }
    else {
      for {
        studentsPerLeague <- studentDao.getStudentsPerLeague(submittedLeagueInfo.get)
        g <- totalNumberOfCombinedLeague
      } yield Ok(views.html.Student.view(user, Some(Forms.leagueForm), g, Seq(studentsPerLeague)))
    }
  }

  def submitLeagueForm = SecuredAction.async { implicit request =>
    val user = Some(request.identity)
    val leagueForm = Forms.leagueForm.bindFromRequest()
    leagueForm.fold(
      hasErrors = { form =>
        println("we are having error, try to check form data is matched with html")
        println(form.data)
        Future.successful(Redirect(routes.HomeController.index()))
      },
      success = {
        successfulLeagueForm =>
          Future.successful(Redirect(routes.StudentController.view(successfulLeagueForm)))
      })
  }

  def update(id: Long) = SecuredAction.async { implicit request =>
    val user = Some(request.identity)
    studentDao.getStudentById(id).map {
      student => if (student.isDefined) Ok(views.html.Student.update(user, id, Forms.studentForm.fill(student.get))) else NotFound
    }
  }

  def submitUpdate(id: Long) = SecuredAction.async { implicit request =>
    val user = Some(request.identity)
    val studentForm = Forms.studentForm.bindFromRequest()

    studentForm.fold(
      hasErrors = {
        errorForm =>
          println("we are having error, try to check form data is matched with html")
          println(errorForm.data)
          Future.successful(Ok(views.html.Student.update(user, id, errorForm)))
      },
      success = { student =>

        for {
          _ <- studentDao.updateStudentById(id, user.get, student)
        } yield Redirect(routes.StudentController.view(combineLeagueAndSubLeague(student.league, student.subLeague)))
      })
  }

}