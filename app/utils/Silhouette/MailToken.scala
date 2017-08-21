package utils.Silhouette

import java.util.Date

/**
  * Created by s43132 on 20/2/2017.
  */
//import org.joda.time.DateTime

trait MailToken {
  def id: String
  def email: String
  def expirationTime: Date
  def isExpired = expirationTime.before(new Date())
}