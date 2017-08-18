package dao

/**
  * Created by aknay on 2/3/17.
  */

import java.io
import javax.inject.Inject

import com.google.inject.Singleton
import slick.jdbc.meta.MTable
import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfigProvider}
import slick.jdbc.JdbcProfile
import models._
import org.joda.time.DateTime
import org.joda.time.format.{DateTimeFormat, DateTimeFormatter}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

/** Ref: http://slick.lightbend.com/doc/3.0.0/schemas.html */

@Singleton
class AdminToolDao @Inject()(userDao: UserDao)(protected val dbConfigProvider: DatabaseConfigProvider) extends UserTableComponent with HasDatabaseConfigProvider[JdbcProfile] {
  /** describe the structure of the tables: */


  import profile.api._


  val ADMIN_TOOL_TABLE_NAME = "AdminToolTable"

  /** Since we are using album id as Option[Long], so we need to use id.? */
  class AdminToolTable(tag: Tag) extends Table[AdminTool](tag, ADMIN_TOOL_TABLE_NAME) {

    def staringDate = column[Option[DateTime]]("strtingDate", O.Default(None))

    def endingDate = column[Option[DateTime]]("endingDate", O.Default(None))

    def announcement = column[Option[String]]("announcement")

    def lastUpdateTime = column[Option[DateTime]]("lastUpdateTime")

    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)

    def adminId = column[Long]("adminId")

    def event = column[Option[String]]("event")

    def * = (id.?, adminId.?, staringDate, endingDate, announcement, lastUpdateTime, event) <> (AdminTool.tupled, AdminTool.unapply)
  }

  def blockExec[T](action: DBIO[T]): T = Await.result(db.run(action), 2 seconds)


  lazy val adminToolTable = TableQuery[AdminToolTable]

  //Note: I cannot add this whole table as trait due to implicit
  //Either Inject dao error or it doesn't recognize the implicit value
  implicit val dateTimeTest = MappedColumnType.base[DateTime, String](
    { b => b.toString }, // map Date to String
    { i => DateTime.parse(i) } // map Sting to Date
  )

  this.createTableIfNotExisted
  this.createAdminToolIfNotExisted

  /** The following statements are Action */
  private lazy val createTableAction = adminToolTable.schema.create

  def createTableIfNotExisted {
    val x = blockExec(MTable.getTables(ADMIN_TOOL_TABLE_NAME)).toList
    if (x.isEmpty) {
      blockExec(createTableAction)
    }
  }

  def createAdminToolIfNotExisted = {
    getAdminTool.map { a => {
      if (a.isEmpty) db.run(adminToolTable += AdminTool(Some(1), Some(1), None, None, None, Some(DateTime.now()), None))
      else println("status" + a.isDefined)
    }
    }
  }

  def getAnnouncement: Future[Option[Option[String]]] = {
    db.run(adminToolTable.map(_.announcement).result.headOption)
  }

  def getStartingDate: Future[Option[Option[DateTime]]] = {
    db.run(adminToolTable.map(_.staringDate).result.headOption)
  }

  def getEndingDate: Future[Option[Option[DateTime]]] = {
    db.run(adminToolTable.map(_.endingDate).result.headOption)
  }

  def getAdminTool: Future[Option[AdminTool]] = {
    db.run(adminToolTable.result.headOption)
  }

  def updateAdminTool(user: User, adminTool: AdminTool): Future[Int] = {
    val adminToolCopy = adminTool.copy(adminId = user.id, lastUpdateTime = Some(DateTime.now)) //always update with time and adminId
    db.run(adminToolTable.update(adminToolCopy))
  }

  def deleteAllEvents(user: User): Future[Boolean] = {
    if (isValidToModifiedData(user)) {
      for {
        adminTool <- getAdminTool
        _ <- updateAdminTool(user, adminTool.get.copy(event = None))
      } yield true
    }
    Future.successful(false)
  }

  private def isValidToModifiedData(user: User): Boolean = {
    if (user.role != Role.Admin) {
      println("user is not admin")
      return false
    }
    true
  }

  val DATE_PATTERN: DateTimeFormatter = DateTimeFormat.forPattern("dd-MMM-YYYY")

  def getFormattedDateString(date: DateTime): String = {
    date.toString(DATE_PATTERN)
  }

  def getFormattedDate(date: String): DateTime = {
    DateTime.parse(date, DATE_PATTERN)
  }

  def createAnnouncement(user: User, announcement: String, startingDate: DateTime, endingDate: DateTime): Future[Unit] = {
    for {
      adminTool <- getAdminTool
      result <- updateAdminTool(user, adminTool.get.copy(announcement = Some(announcement), startingDate = Some(startingDate), endingDate = Some(endingDate)))
    } yield {}
  }

  def deleteAnnouncement(user: User): Future[Unit] = {
    for {
      adminTool <- getAdminTool
      result <- db.run(adminToolTable.update(adminTool.get.copy(announcement = None, startingDate = None, endingDate = None)))
    } yield {}
  }

  def isEventExisted(event: String, allEvents: String): Future[Boolean] = {
    val trimmedList = allEvents.split(",")
    val result = trimmedList.filter(_.compareToIgnoreCase(event) == 0)
    if (result.length > 0) return Future.successful(true)
    Future.successful(false)
  }

  def stringToList(s: String): List[String] = {
    if (s.isEmpty) return List[String]()
    if (s.trim().length == 0) return List[String]()
    s.split(",").toList
  }

  def removeStringFromList(s: String, listOfEvent: List[String]): List[String] = {
    //it will remove all elements that is equal to 's'
    listOfEvent.filterNot(x => x == s)
  }

  def getNumberOfEvents(): Future[Int] = {
    for {
      adminTool <- getAdminTool
      result <- if (adminTool.get.event.isDefined) {
        val allEvents = adminTool.get.event.get
        Future.successful(stringToList(allEvents).length)
      } else {
        Future.successful(0)
      }
    } yield result
  }


  def getEvent: Future[Option[Option[String]]] = {
    db.run(adminToolTable.map(_.event).result.headOption)
  }

  def getEventAsList: Future[Option[List[String]]] = {
    getEvent.map {
      x =>
        if (x.isEmpty) {
          None
        }
        else {
          if (x.get.isEmpty) None
          else {
            Some(x.get.get.split(",").toList)
          }
        }

    }
  }

  def addEvent(user: User, event: String): Future[Boolean] = {
    for {
      adminTool <- getAdminTool
      isEventEmpty <- Future.successful(adminTool.get.event.isEmpty)
      result <- if (isEventEmpty) {
        updateAdminTool(user, adminTool.get.copy(event = Some(event)))
        Future.successful(true)
      }
      else {
        val isSuccessful = isEventExisted(event, adminTool.get.event.get).map {
          case true => false
          case false =>
            getNumberOfEvents().map {
              x =>
                if (x > 0) {
                  val allevents: String = adminTool.get.event.get + "," + event
                  updateAdminTool(user, adminTool.get.copy(event = Some(allevents)))
                } else {
                  updateAdminTool(user, adminTool.get.copy(event = Some(event)))
                }
            }
            true
        }
        isSuccessful
      }
    } yield result
  }

  def deleteEvent(user: User, event: String): Future[Boolean] = {
    for {
      adminTool <- getAdminTool
      isEventEmpty <- Future.successful(adminTool.get.event.isEmpty)
      result <- if (isEventEmpty) {
        Future.successful(false)
      }
      else {
        val isExisted = isEventExisted(event, adminTool.get.event.get).map {
          case true =>
            val allEvents = adminTool.get.event.get
            val numberOfEvents = stringToList(allEvents).length
            if (numberOfEvents > 1) {
              val remainingList: Seq[String] = removeStringFromList(event, stringToList(adminTool.get.event.get))
              updateAdminTool(user, adminTool.get.copy(event = Some(remainingList.mkString(","))))
            }
            else {
              updateAdminTool(user, adminTool.get.copy(event = None))
            }
            true
          case false =>
            false
        }
        isExisted
      }
    } yield result
  }

}
