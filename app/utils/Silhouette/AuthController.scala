package utils.Silhouette

/**
  * Created by aknay on 30/1/17.
  */

import play.api.mvc.Controller
import play.api.i18n.I18nSupport
import com.mohiva.play.silhouette.api.{Environment, Silhouette}
import com.mohiva.play.silhouette.api.actions._
import models.User

trait AuthController extends Controller with I18nSupport {
  def silhouette: Silhouette[MyEnv]
  def env: Environment[MyEnv] = silhouette.env

  def SecuredAction = silhouette.SecuredAction
  def UnsecuredAction = silhouette.UnsecuredAction
  def UserAwareAction = silhouette.UserAwareAction

  implicit def securedRequest2User[A](implicit request: SecuredRequest[MyEnv, A]): User = request.identity
  implicit def userAwareRequest2UserOpt[A](implicit request: UserAwareRequest[MyEnv, A]): Option[User] = request.identity
}