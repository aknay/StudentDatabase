package utils.Silhouette

/**
  * Created by s43132 on 20/2/2017.
  */
import org.joda.time.DateTime

trait MailToken {
  def id: String
  def email: String
  def expirationTime: DateTime
  def isExpired = expirationTime.isBeforeNow
}