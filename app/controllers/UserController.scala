package controllers

import javax.inject.Inject

import com.mohiva.play.silhouette.api.exceptions.ProviderException
import com.mohiva.play.silhouette.api.repositories.AuthInfoRepository
import com.mohiva.play.silhouette.api.util.{Clock, Credentials, PasswordHasherRegistry}
import play.api.data.Form
import play.api.i18n.{I18nSupport, Messages, MessagesApi}
import play.api.mvc.{Action, Flash, RequestHeader}

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import play.api.data.Forms._
import com.mohiva.play.silhouette.api._
import com.mohiva.play.silhouette.impl.authenticators.CookieAuthenticator
import com.mohiva.play.silhouette.impl.exceptions.IdentityNotFoundException
import com.mohiva.play.silhouette.impl.providers.CredentialsProvider
import utils.Silhouette.Implicits._
import dao.{AdminToolDao, AlbumDao, UserDao}
import models._
import forms.Forms
import play.api.Configuration
import utils.Mailer
import utils.Silhouette._

/**
  * Created by aknay on 27/12/16.
  */

object UserController {
  //this is the quick hack to get token (which is used in 'test' to verify whether the normal user become admin user)
  var mToken: String = ""

  def getToken = {
    mToken
  }

  def setToken(token: String): Unit = {
    mToken = token
  }
}

class UserController @Inject()(userDao: UserDao,
                               albumDao: AlbumDao,
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

  userDao.createUserInfoTableIfNotExisted

  def login = UserAwareAction { implicit request =>
    request.identity match {
      case Some(_) => Redirect(routes.UserController.user())
      case None => Ok(views.html.User.login(Forms.signUpForm))
    }
  }

  def signUp = Action { implicit request =>
    Ok(views.html.User.signup(Forms.signUpForm))
  }

  def submitSignUpForm = UnsecuredAction.async { implicit request =>
    Forms.signUpForm.bindFromRequest.fold(
      form => Future.successful(BadRequest(views.html.User.signup(form))),
      user => {
        val loginInfo: LoginInfo = user.email
        userService.retrieve(loginInfo).flatMap {
          case Some(_) => Future.successful(Redirect(routes.UserController.signUp()).flashing(Flash(Forms.signUpForm.data) + ("error" -> Messages("User already existed")))) //user is not unique
          case None =>
            val token = MailTokenUser(user.email, isSignUp = true)
            for {
              savedUser <- userService.save(user)
              _ <- authInfoRepository.add(loginInfo, passwordHasherRegistry.current.hash(user.password))
              _ <- tokenService.create(token)
            } yield {
              mailer.welcome(savedUser, link = routes.UserController.signUpWithToken(token.id).absoluteURL())
              Ok(views.html.User.signupsuccess(savedUser))
            }
        }
      }
    )
  }

  //Note: we cannot go from Secure action to unsecured action
  //So after user is logged in, user cannot go to unsecured action
  def approveUserWithToken(tokenId: String) = UserAwareAction.async { implicit request =>
    masterTokenService.retrieve(tokenId).flatMap {
      case Some(token) if token.isToChangeToMaster && !token.isExpired => {
        userService.retrieve(token.email).flatMap {
          case Some(user) =>
            if (user.role != Role.Admin) {
              userService.save(user.copy(role = Role.Admin))
              masterTokenService.consume(tokenId)
              Future.successful(Ok(views.html.User.approvesuccess(user)))
            }
            else {
              Future.successful(NotFound)
            }

          case None => Future.failed(new IdentityNotFoundException("Couldn't find user"))
        }
      }
      case Some(token) =>
        tokenService.consume(tokenId)
        Future.successful(NotFound)

      case None => Future.successful(NotFound)
    }
  }

  def requestToBeMaster = SecuredAction.async { implicit request =>
    val loginInfo: LoginInfo = request.identity.loginInfo
    val user: User = request.identity
    userService.retrieve(loginInfo).flatMap {
      case Some(_) =>
        val token: MailTokenMasterUser = MailTokenMasterUser(user.email, isToChangeToMaster = true)
        masterTokenService.create(token)
        mailer.sendToDeveloper(user, link = routes.UserController.approveUserWithToken(token.id).absoluteURL())
        UserController.setToken(token.id)
        Future.successful(Ok(views.html.User.requestsuccess(user)))

      case None => Future.successful(NotFound)
    }
  }

  def signUpWithToken(tokenId: String) = UnsecuredAction.async { implicit request =>
    tokenService.retrieve(tokenId).flatMap {
      case Some(token) if token.isSignUp && !token.isExpired => {
        userService.retrieve(token.email).flatMap {
          case Some(user) =>
            env.authenticatorService.create(user.email).flatMap { authenticator =>
              if (!user.activated) {
                userService.save(user.copy(activated = true)).map { newUser =>
                  env.eventBus.publish(SignUpEvent(newUser, request))
                }
                userDao.insertUserInfo(user) //TODO not so right to be here
              }
              for {
                cookie <- env.authenticatorService.init(authenticator)
                result <- env.authenticatorService.embed(cookie, Ok(views.html.User.signedUp(user)))
              } yield {
                tokenService.consume(tokenId)
                env.eventBus.publish(LoginEvent(user, request))
                result
              }
            }
          case None => Future.failed(new IdentityNotFoundException("Couldn't find user"))
        }
      }
      case Some(token) =>
        tokenService.consume(tokenId)
        Future.successful(NotFound)

      case None => Future.successful(NotFound)
    }
  }

  def submitLoginForm = UnsecuredAction.async { implicit request =>
    val loginForm = Forms.loginForm.bindFromRequest()
    loginForm.fold(
      formWithErrors => Future.successful(BadRequest(views.html.User.login(Forms.loginForm))),
      formData => {
        credentialsProvider.authenticate(Credentials(formData.email, formData.password)).flatMap { loginInfo =>
          userService.retrieve(loginInfo).flatMap {
            case Some(user) => for {
              authenticator <- env.authenticatorService.create(loginInfo)
              cookie <- env.authenticatorService.init(authenticator)
              result <- env.authenticatorService.embed(cookie, Redirect(routes.UserController.user()))
            } yield {
              env.eventBus.publish(LoginEvent(user, request))
              result
            }
            case None => Future.failed(new IdentityNotFoundException("Couldn't find user"))
          }
        }.recover {
          case e: ProviderException => Redirect(routes.UserController.login()).flashing("error" -> Messages("auth.credentials.incorrect"))
        }
      }
    )
  }

  def logout = SecuredAction.async { implicit request =>
    env.eventBus.publish(LogoutEvent(request.identity, request))
    env.authenticatorService.discard(request.authenticator, Redirect(routes.HomeController.index()))
  }

  def user() = SecuredAction.async { implicit request =>
    val user: User = request.identity
    userDao.getUserInfo(user).map {
      userInfo => Ok(views.html.User.profile(Some(user), userInfo))
    }
  }

  def listAlbum(page: Int) = SecuredAction.async { request =>
    val user: User = request.identity
    val tempId: Long = user.id.get
    albumDao.listWithPage(tempId, page = page).map {
      page => Ok(views.html.User.albumLists(Some(user), page))
    }
  }

  val userInfoForm = Form(
    mapping(
      "userId" -> ignored(0: Long),
      "name" -> nonEmptyText,
      "location" -> nonEmptyText
    )(UserInfo.apply)(UserInfo.unapply)
  )

  def editUserInfo = SecuredAction.async { implicit request =>
    val user: User = request.identity

    userDao.getUserInfo(user).map {
      userInfo => {
        userInfo match {
          case Some(x) =>
            val form: Form[UserInfo] = userInfoForm.fill(x)
            Ok(views.html.User.profileSetting(Some(user), form))

          case None => NotFound
        }
      }
    }
  }

  def updateUserInfo = SecuredAction { implicit request =>
    val user = request.identity
    val newForm = userInfoForm.bindFromRequest()
    newForm.fold(
      hasErrors = { form =>
        Unauthorized("Oops, we are having form error")
      },
      success = {
        userInfo =>
          userDao.updateUserInfo(user, userInfo.name, userInfo.location)
          Redirect(routes.UserController.editUserInfo()).flashing("success" -> Messages("settings.profile.updated"))
      })
  }

  def requestResetPassword = UserAwareAction { implicit request =>
    request.identity match {
      case Some(_) => Redirect(routes.HomeController.index())
      case None => Ok(views.html.User.requestResetPassword(Forms.resetRequestViaEmailForm))
    }
  }

  /**
    * Sends an email to the user with a link to reset the password
    * //    */

  def handleForgotPassword = UnsecuredAction.async { implicit request =>

    val loginForm = Forms.resetRequestViaEmailForm.bindFromRequest()
    loginForm.fold(
      formWithErrors => {
        Future.successful(BadRequest)
      },
      email =>
        userService.retrieve(email).flatMap {
          case Some(_) =>
            val token = MailTokenUser(email, isSignUp = false)
            tokenService.create(token).map { _ =>
              println(routes.UserController.resetPassword(token.id).absoluteURL())
              mailer.forgotPassword(email, link = routes.UserController.resetPassword(token.id).absoluteURL())
              UserController.setToken(token.id)
              Ok(views.html.User.forgotPasswordSent(email))
            }
          case None =>
            Future.successful(BadRequest)
        }
    )

  }

  def resetPassword(tokenId: String) = UnsecuredAction.async { implicit request =>
    tokenService.retrieve(tokenId).flatMap {
      case Some(token) if !token.isSignUp && !token.isExpired =>
        Future.successful(Ok(views.html.User.resetPassword(tokenId, Forms.resetPasswordForm)))

      case Some(token) =>
        tokenService.consume(tokenId)
        Future.successful(BadRequest)

      case None => Future.successful(BadRequest)
    }
  }

  def handleResetPassword(tokenId: String) = UnsecuredAction.async { implicit request =>
    Forms.resetPasswordForm.bindFromRequest.fold(
      formWithErrors => {
        Future.successful(Redirect(routes.UserController.resetPassword(tokenId))
          .flashing(Flash(formWithErrors.data) + ("error" -> Messages("reset.password.require.same.password"))))
      },
      passwords => {
        tokenService.retrieve(tokenId).flatMap {
          case Some(token) if !token.isSignUp && !token.isExpired =>
            val loginInfo: LoginInfo = token.email
            userService.retrieve(loginInfo).flatMap {
              case Some(user) =>
                for {
                  _ <- authInfoRepository.update(loginInfo, passwordHasherRegistry.current.hash(passwords._1))
                  authenticator <- env.authenticatorService.create(user.email)
                  result <- env.authenticatorService.renew(authenticator, Ok(views.html.User.resetedPassword(user)))
                } yield {
                  tokenService.consume(tokenId)
                  env.eventBus.publish(LoginEvent(user, request))
                  result
                }
              case None => Future.failed(new IdentityNotFoundException("Couldn't find user"))
            }
          case Some(token) =>
            tokenService.consume(tokenId)
            Future.successful(BadRequest)

          case None =>
            Future.successful(BadRequest)
        }
      }
    )
  }

}