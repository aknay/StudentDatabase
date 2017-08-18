package controllers

import javax.inject.Inject

import com.github.tototoshi.csv.CSVReader
import com.mohiva.play.silhouette.api.Silhouette
import dao.{StudentDao, UserDao}
import forms.Forms
import models.{Student, User}
import org.joda.time.DateTime
import play.api.i18n.{I18nSupport, Messages, MessagesApi}
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.mvc.{Action, Flash}
import utils.Silhouette.{AuthController, MyEnv}

import scala.collection.mutable.ListBuffer
import scala.concurrent.Future

/**
  * Created by aknay on 5/12/2016.
  */
/** we need to use I18nSupport because we are using form helper
  * Without this --> Form: could not find implicit value for parameter messages: play.api.i18n.Messages
  * Ref: http://stackoverflow.com/questions/30799988/play-2-4-form-could-not-find-implicit-value-for-parameter-messages-play-api-i
  * */
class StudentController @Inject()(studentDao: StudentDao, userDao: UserDao)
                                 (val messagesApi: MessagesApi, val silhouette: Silhouette[MyEnv])
  extends AuthController with I18nSupport {
  /** we can use album form directly with Album case class by applying id as Option[Long] */

  def add = SecuredAction.async { implicit request =>
    val user = Some(request.identity)
    Future.successful(Ok(views.html.Student.add(user, Forms.studentForm)))
  }

  def addWithCsv = SecuredAction.async { implicit request =>
    val user = Some(request.identity)
    Future.successful(Ok(views.html.Student.add_with_csv(user, Forms.studentCsvForm)))
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

  def uploadCsv = SecuredAction.async(parse.multipartFormData) { implicit request =>
    println("going to upload")
    request.body.file("csv").map { csv =>
      val reader = CSVReader.open(csv.ref.file)

      val v = reader.allWithHeaders()

      val v1 = v.take(2)

      val country = "Country"
      val teamName = "Team Name"
      val institution = "Institution"
      val league = "League"
      val subLeague = "League (Sub-Category)"
      val studentName = "Student Name"

      val studentList = ListBuffer[Student]()
      for (a <- v1) {
        val c = a.get("Country")
        val t = a.get(teamName)
        val i = a.get(institution)
        val l = a.get(league)
        val sl = a.get(subLeague)
        val sn = a.get(studentName)


        if (c.isDefined && t.isDefined && i.isDefined && l.isDefined && sl.isDefined && sn.isDefined) {
          println(c.get)
          val student = Student(name = sn.get, teamName = t.get, institution = i.get,
            country = c.get, league = l.get, subLeague = sl.get, id = Some(1), event = "", lastUpdateTime = Some(new DateTime()), updateBy = Some(1))
          studentList += student
        }
        else {
          println("we got problem")
        }

      }

      studentList.foreach(println)


    }
    Future.successful(Ok)
  }

}