package utils.Silhouette

/**
  * Created by s43132 on 21/2/2017.
  */

import com.mohiva.play.silhouette.api.Authorization
import com.mohiva.play.silhouette.impl.authenticators.CookieAuthenticator
import play.api.mvc.Request

import scala.concurrent.Future
import models.{User, Role}

/**
  * Only allows those users that have at least a service of the selected.
  * Master service is always allowed.
  * Ex: WithService("serviceA", "serviceB") => only users with services "serviceA" OR "serviceB" (or "master") are allowed.
  */
case class WithService(role: Role) extends Authorization[User, CookieAuthenticator] {
  def isAuthorized[A](user: User, authenticator: CookieAuthenticator)(implicit r: Request[A]) = Future.successful {
    WithService.isAuthorized(user, role: Role)
  }
}

object WithService {
  def isAuthorized(user: User, role: Role): Boolean =
    user.role == Role.Admin
}

/**
  * Only allows those users that have every of the selected services.
  * Master service is always allowed.
  * Ex: Restrict("serviceA", "serviceB") => only users with services "serviceA" AND "serviceB" (or "master") are allowed.
  */
case class WithServices(role: Role) extends Authorization[User, CookieAuthenticator] {
  def isAuthorized[A](user: User, authenticator: CookieAuthenticator)(implicit r: Request[A]) = Future.successful {
    WithServices.isAuthorized(user, role: Role)
  }
}

object WithServices {
  def isAuthorized(user: User, role: Role): Boolean =
    user.role == Role.Admin
}