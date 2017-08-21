package utils

import java.sql.Timestamp
import java.util.Date

object Utils {
  def getTimeStampFromDate(date: Date): Timestamp = new Timestamp(date.getTime)
}
