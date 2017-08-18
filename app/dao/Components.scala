package dao

import models.{Album, Role, Student, User}
import org.joda.time.DateTime
import play.api.db.slick.HasDatabaseConfigProvider
import slick.jdbc.JdbcProfile

/**
  * Created by aknay on 2/3/17.
  */

trait UserTableComponent {
  self: HasDatabaseConfigProvider[JdbcProfile] =>

  val USER_TABLE_NAME = "usertable"

  import profile.api._

  class UserTable(tag: Tag) extends Table[User](tag, USER_TABLE_NAME) {

    def email = column[String]("email")

    def password = column[String]("password")

    def username = column[String]("username")

    def activated = column[Boolean]("activated")

    def role = column[Role]("role")

    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)

    def * = (id.?, email, password, username, role, activated) <> (User.tupled, User.unapply)

  }

  lazy val userTable = TableQuery[UserTable]

}

trait AlbumTableComponent extends UserTableComponent {
  self: HasDatabaseConfigProvider[JdbcProfile] =>

  import profile.api._

  val ALBUM_TABLE_NAME = "album"

  /** Since we are using album id as Option[Long], so we need to use id.? */
  class AlbumTable(tag: Tag) extends Table[Album](tag, ALBUM_TABLE_NAME) {
    def artist = column[String]("artist")

    def title = column[String]("title")

    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)

    def userId = column[Long]("userId")

    def * = (id.?, userId.?, artist, title) <> (Album.tupled, Album.unapply)

    def fk = foreignKey("album_fk", userId, userTable)(_.id, onDelete = ForeignKeyAction.Cascade)
  }

  lazy val albumTable = TableQuery[AlbumTable]
}

trait StudentTableComponent extends UserTableComponent {
  self: HasDatabaseConfigProvider[JdbcProfile] =>

  import profile.api._

  val STUDENT_TABLE_NAME = "student_table"

  /** Since we are using album id as Option[Long], so we need to use id.? */
  class StudentTable(tag: Tag) extends Table[Student](tag, STUDENT_TABLE_NAME) {
    def name = column[String]("name")

    def teamName = column[String]("team_name")

    def institution = column[String]("institution")

    def country = column[String]("country")

    def league = column[String]("league")

    def subLeague = column[String]("sub_league")

    def event = column[String]("event")

    def lastUpdateTime = column[DateTime]("last_update_time")

    def updateBy = column[Long]("update_by")

    implicit val dateTimeTest = MappedColumnType.base[DateTime, String](
      { b => b.toString }, // map Date to String
      { i => DateTime.parse(i) } // map Sting to Date
    )

    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)

    def * = (id.?, name, teamName, institution, country, league, subLeague, event, lastUpdateTime.?, updateBy.?) <> (Student.tupled, Student.unapply)

  }

  lazy val studentTable = TableQuery[StudentTable]
}



