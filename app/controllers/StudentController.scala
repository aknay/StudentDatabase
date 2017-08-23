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

import scala.collection.mutable.ListBuffer
import scala.concurrent.{ExecutionContext, Future}

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
          studentDao.insertByUser(newAlbum, request.identity.id.get).map {
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

  def uploadCsv = SecuredAction.async(parse.multipartFormData) { implicit request =>
    val user = Some(request.identity)
    val studentList = ListBuffer[Student]()
    val studentListWithStatus = ListBuffer[StudentWithStatus]()
    request.body.file("csv").map { csv =>
      val reader = CSVReader.open(csv.ref.file)

      val v = reader.allWithHeaders()

      val country = "Country"
      val teamName = "Team Name"
      val institution = "Institution"
      val league = "League"
      val subLeague = "League (Sub-Category)"
      val studentName = "Student Name"

      for (a <- v) {
        val c = a.get(country)
        val t = a.get(teamName)
        val i = a.get(institution)
        val l = a.get(league)
        val sl = a.get(subLeague)
        val sn = a.get(studentName)


        if (c.isDefined && t.isDefined && i.isDefined && l.isDefined && sl.isDefined && sn.isDefined) {
          println(c.get)
          val student = Student(name = sn.get, teamName = t.get, institution = i.get,
            country = c.get, league = l.get, subLeague = sl.get, id = Some(1), event = "", lastUpdateTime = Some(Utils.getTimeStampFromDate(new Date())), updateBy = Some(1))
          studentList += student
        }
        else {
          println("we got problem")
        }

      }

      for (student <- studentList) {

        val status: Future[Boolean] = for {
          a <- studentDao.insertByUser(student, user.get.id.get)
        } yield a

        status.map {
          case true => studentListWithStatus += StudentWithStatus(student.name, student.teamName, student.institution, student.country, student.league, student.subLeague, isExisted = false)
          case false => studentListWithStatus += StudentWithStatus(student.name, student.teamName, student.institution, student.country, student.league, student.subLeague, isExisted = true)
        }
      }

      studentListWithStatus.foreach(println)

    }

    Future.successful(Ok(views.html.Student.add_student_with_table(user, studentListWithStatus)))
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


}