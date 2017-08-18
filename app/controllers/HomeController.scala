package controllers

import javax.inject._

import play.api.i18n.MessagesApi
import play.api.mvc.Action
import com.mohiva.play.silhouette.api.Silhouette
import dao.{AdminToolDao, UserDao}
import models.User
import org.joda.time.DateTime
import utils.Silhouette.{AuthController, MyEnv}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
  * This controller creates an `Action` to handle HTTP requests to the
  * application's home page.
  */
@Singleton
class HomeController @Inject()(userDao: UserDao,
                               adminToolDao: AdminToolDao)
                              (val messagesApi: MessagesApi,
                               val silhouette: Silhouette[MyEnv])
  extends AuthController {

  userDao.createUserInfoTableIfNotExisted

  def index = UserAwareAction.async { implicit request =>
    val p: Future[(Option[DateTime], Option[DateTime], Option[String], Option[User])] = for {
      startingDate <- adminToolDao.getStartingDate
      endingDate <- adminToolDao.getEndingDate
      announcement <- adminToolDao.getAnnouncement
    } yield (startingDate.get, endingDate.get, announcement.get, request.identity)


    //TODO : WE NEED TO TEST EACH CASE LATER
    p.map {
      case (startingDate, endingDate, announcement, user) =>
        (startingDate, endingDate, announcement, user) match {
          case (Some(s), Some(e), Some(a), Some(u)) => Ok(views.html.index(Some(u), Some(a), Some(adminToolDao.getFormattedDateString(s)), Some(adminToolDao.getFormattedDateString(e))))
          case (Some(s), Some(e), Some(a), None) => Ok(views.html.index(None, Some(a), Some(adminToolDao.getFormattedDateString(s)), Some(adminToolDao.getFormattedDateString(e))))
          case (None, None, None, None) => Ok(views.html.index(None, None, None, None))
          case (None, None, None, Some(u)) => Ok(views.html.index(Some(u), None, None, None))
        }
    }
  }

  def show(userName: String) = Action {
    Ok(views.html.users(userName))
  }
}
