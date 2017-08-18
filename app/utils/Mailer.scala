package utils

/**
  * Created by s43132 on 20/2/2017.
  */
import models.User
import play.twirl.api.Html
import play.api.i18n.Messages
import views.html.mails
import javax.inject.{Inject, Singleton}

@Singleton
class Mailer @Inject() (ms: MailService) {

  implicit def html2String(html: Html): String = html.toString

  def welcome(user: User, link: String)(implicit m: Messages) {
    ms.sendEmailAsync(user.email)(
      subject = Messages("mail.welcome.subject"),
      bodyHtml = mails.welcome(user.email, link),
      bodyText = mails.welcometext(user.email, link)
    )
  }

  def sendToDeveloper(requestedUser: User, link: String)(implicit m: Messages) {
    ms.sendEmailAsync("aknay@outlook.com")(
      subject = "Requesting to become an admin",
      bodyHtml = mails.welcome(requestedUser.email, link),
      bodyText = mails.welcometext(requestedUser.email, link)
    )
  }

//TODO
  def forgotPassword(email: String, link: String)(implicit m: Messages) {
    ms.sendEmailAsync(email)(
      subject = Messages("mail.forgotpwd.subject"),
      bodyHtml = mails.forgotPassword(email, link),
      bodyText = mails.forgotPasswordTxt(email, link)
    )
  }

}