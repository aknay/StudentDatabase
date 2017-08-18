package controllers

import javax.inject.Inject

import com.mohiva.play.silhouette.api.{LoginInfo, Silhouette}
import com.mohiva.play.silhouette.api.repositories.AuthInfoRepository
import com.mohiva.play.silhouette.api.util.{Clock, PasswordHasherRegistry}
import com.mohiva.play.silhouette.impl.providers.CredentialsProvider
import dao.{AdminToolDao, AlbumDao, UserDao}
import forms.Forms
import models._
import org.joda.time.DateTime
import play.api.Configuration
import play.api.i18n.{I18nSupport, Messages, MessagesApi}
import play.api.mvc.Flash
import utils.Mailer
import utils.Silhouette._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future


/**
  * Created by aknay on 3/3/17.
  */
class AdminController @Inject()(userDao: UserDao,
                                albumDao: AlbumDao,
                                adminToolDao: AdminToolDao,
                                albumController: AlbumController,
                                userService: UserService,
                                authInfoRepository: AuthInfoRepository,
                                credentialsProvider: CredentialsProvider,
                                tokenService: MailTokenService[MailTokenUser],
                                masterTokenService: MailTokenService[MailTokenMasterUser],
                                mailer: Mailer,
                                conf: Configuration,
                                clock: Clock,
                                passwordHasherRegistry: PasswordHasherRegistry)
                               (val messagesApi: MessagesApi,
                                val silhouette: Silhouette[MyEnv])
  extends AuthController with I18nSupport {


  def admin = SecuredAction(WithServices(Role.Admin)).async { implicit request =>
    adminToolDao.getAdminTool.map {
      adminTool => Ok(views.html.Admin.profile(request.identity, adminTool))
    }
  }

  def viewAllNonAdminUser = SecuredAction(WithServices(Role.Admin)).async { implicit request =>
    userDao.getNonAdminUserList().map {
      list => Ok(views.html.Admin.viewallnonadminuser(request.identity, list))
    }
  }

  def viewAllAlbumsFromNonAdminUser(userId: Long, page: Int = 0) = SecuredAction(WithServices(Role.Admin)).async { implicit request =>
    albumDao.listWithPage(userId, page).map {
      (albums: Page[Album]) => Ok(views.html.AlbumView.list(albums, userId = userId))
    }
  }

  def viewAnnouncementForm = SecuredAction(WithServices(Role.Admin)) { implicit request =>
    val user = request.identity
    Ok(views.html.Admin.MakeAnnouncement(Some(user), Forms.announcementForm))
  }

  def viewAnnouncement = SecuredAction(WithServices(Role.Admin)).async { implicit request =>
    val user = request.identity
    val resultAndOptions = for {
      startDate: Option[Option[DateTime]] <- adminToolDao.getStartingDate
      endingDate: Option[Option[DateTime]] <- adminToolDao.getEndingDate
      announcement: Option[Option[String]] <- adminToolDao.getAnnouncement
    } yield (startDate, endingDate, announcement)

    resultAndOptions.map {
      case (startingDate, endingDate, announcement) =>
        (startingDate, endingDate, announcement) match {
          case (Some(Some(s)), Some(Some(e)), Some(Some(a))) => Ok(views.html.Admin.ViewAnnouncement(Some(user),
            Some(adminToolDao.getFormattedDateString(s)), Some(adminToolDao.getFormattedDateString(e)), Some(a)))
          case (Some(None), Some(None), Some(None)) => Ok(views.html.Admin.ViewAnnouncement(Some(user)))
        }
    }
  }

  def deleteAnnouncement = SecuredAction(WithService(Role.Admin)) { implicit request =>
    Ok(views.html.Admin.DeleteAnnouncement(true, Some(request.identity)))
  }

  def viewSuccessfulAnnouncement = SecuredAction(WithServices(Role.Admin)).async { implicit request =>

    val user = request.identity

    val resultAndOptions = for {
      startDate <- adminToolDao.getStartingDate
      endingDate <- adminToolDao.getEndingDate
      announcement <- adminToolDao.getAnnouncement
    } yield (startDate, endingDate, announcement)

    resultAndOptions.map {
      case (startingDate, endingDate, announcement) =>
        (startingDate, endingDate, announcement) match {
          case (s, e, a) => Ok(views.html.Admin.SuccessfulAnnouncement(Some(user),
            adminToolDao.getFormattedDateString(s.get.get), adminToolDao.getFormattedDateString(e.get.get), announcement.get.get))
          case (None, None, None) => Ok(views.html.Admin.ViewAnnouncement(Some(user)))
        }
    }
  }

  def submitAnnouncement = SecuredAction(WithServices(Role.Admin)).async { implicit request =>
    Forms.announcementForm.bindFromRequest.fold(
      formWithError => Future.successful(BadRequest(views.html.Admin.MakeAnnouncement(Some(request.identity), formWithError))
        .flashing(Flash(Forms.announcementForm.data))),
      formData => {
        val user = request.identity
        for {
          _ <- adminToolDao.createAnnouncement(user, formData.announcement.get, formData.startingDate.get, formData.endingDate.get)
        } yield Redirect(routes.AdminController.viewSuccessfulAnnouncement())
      }
    )
  }

  def addEvent = SecuredAction(WithServices(Role.Admin)).async { implicit request =>
    val user = request.identity
    Future.successful(Ok(views.html.Admin.AddEvent(Some(user), Forms.eventForm)))
  }

  def submitAddEventForm = SecuredAction(WithServices(Role.Admin)).async { implicit request =>
    val user = request.identity
    Forms.eventForm.bindFromRequest.fold(
      formWithError => {
        Future.successful(BadRequest(views.html.Admin.AddEvent(Some(user), formWithError)))
      },
      formData => {
        adminToolDao.addEvent(user, formData).map {
          case true => Ok(views.html.Admin.AddingEventWithSuccess(Some(user), formData))
          case false => Redirect(routes.AdminController.addEvent()).flashing("error" -> Messages("admin.add.event.fail"))
        }
      }
    )
  }

  def viewEvents = SecuredAction(WithServices(Role.Admin)).async { implicit request =>
    val user = request.identity
    adminToolDao.getEventAsList.map {
      events => Ok(views.html.Admin.ViewEvents(Some(user), events))
    }
  }

  def deleteEvent = SecuredAction(WithServices(Role.Admin)).async { implicit request =>
    val user = request.identity
    adminToolDao.getEventAsList.map {
      events => Ok(views.html.Admin.DeleteEvent(Some(user), Forms.eventForm, events))
    }
  }

  def submitDeleteEventForm = SecuredAction(WithServices(Role.Admin)).async { implicit request =>
    val user = request.identity
    Forms.eventForm.bindFromRequest.fold(
      formWithError => {
        adminToolDao.getEventAsList.map {
          events => Ok(views.html.Admin.DeleteEvent(Some(user), formWithError, events))
        }
      },
      formData => {
        adminToolDao.deleteEvent(user, formData).map {
          case true => Ok(views.html.Admin.DeletingEventWithSuccess(Some(user), formData))
          case false => Redirect(routes.AdminController.deleteEvent()).flashing("error" -> Messages("admin.delete.event.fail"))
        }
      }
    )
  }
}
