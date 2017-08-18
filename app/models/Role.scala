package models

/**
  * Created by aknay on 24/2/17.
  */

import slick.jdbc.PostgresProfile.api._

//Ref: https://github.com/underscoreio/scalax15-slick/blob/master/src/main/scala/queries/Main.scala
sealed abstract class Role(val role: String)

object Role {
  private val ADMIN = "admin"
  private val NORMAL_USER = "normaluser"

  final case object Admin extends Role(ADMIN)

  final case object NormalUser extends Role(NORMAL_USER)

  implicit val roleColumnType = MappedColumnType.base[Role, String](
    Role.toInt, Role.fromInt
  )

  private def fromInt(role: String): Role = role match {
    case ADMIN => Admin
    case NORMAL_USER => NormalUser
    case _ => sys.error("Role only applied to Admin and Normal User")
  }

  private def toInt(rating: Role): String = rating match {
    case Admin => ADMIN
    case NormalUser => NORMAL_USER
  }
}