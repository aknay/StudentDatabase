package controllers

import javax.inject.Inject

import com.mohiva.play.silhouette.api.Silhouette
import dao.{StudentDao, UserDao}
import forms.Forms
import models.User
import play.api.i18n.{I18nSupport, Messages, MessagesApi}
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.mvc.Flash
import utils.Silhouette.{AuthController, MyEnv}

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

  def menu = SecuredAction.async { implicit request =>
    val user = Some(request.identity)
    Future.successful(Ok(views.html.Student.menu(user)))
  }
}