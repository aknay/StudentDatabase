package models

/**
  * Created by s43132 on 20/2/2017.
  */

import java.time.{LocalDateTime, ZoneId}
import java.util.Date

import utils.Silhouette.MailToken
import java.util.UUID
import scala.concurrent.Future

case class MailTokenUser(id: String, email: String, expirationTime: Date, isSignUp: Boolean) extends MailToken

object MailTokenUser {
  def apply(email: String, isSignUp: Boolean): MailTokenUser = {
    val l = LocalDateTime.now.plusHours(12)
    val date: Date = Date.from(l.atZone(ZoneId.systemDefault()).toInstant())
    MailTokenUser(UUID.randomUUID().toString, email, date, isSignUp)
  }

  val tokens = scala.collection.mutable.HashMap[String, MailTokenUser]()

  def findById(id: String): Future[Option[MailTokenUser]] = {
    Future.successful(tokens.get(id))
  }

  def save(token: MailTokenUser): Future[MailTokenUser] = {
    tokens += (token.id -> token)
    Future.successful(token)
  }

  def delete(id: String): Unit = {
    tokens.remove(id)
  }
}

case class MailTokenMasterUser(id: String, email: String, expirationTime: Date, isToChangeToMaster: Boolean) extends MailToken

object MailTokenMasterUser {
  def apply(email: String, isToChangeToMaster: Boolean): MailTokenMasterUser = {
    val l = LocalDateTime.now.plusHours(12)
    val date: Date = Date.from(l.atZone(ZoneId.systemDefault()).toInstant())
    MailTokenMasterUser(UUID.randomUUID().toString, email, date, isToChangeToMaster)
  }

  val tokens = scala.collection.mutable.HashMap[String, MailTokenMasterUser]()

  def findById(id: String): Future[Option[MailTokenMasterUser]] = {
    Future.successful(tokens.get(id))
  }

  def save(token: MailTokenMasterUser): Future[MailTokenMasterUser] = {
    tokens += (token.id -> token)
    Future.successful(token)
  }

  def delete(id: String): Unit = {
    tokens.remove(id)
  }
}
