package models

import org.joda.time.DateTime
import utils.Silhouette.IdentitySilhouette

/**
  * Created by aknay on 14/12/16.
  */

case class Album(id: Option[Long] = None, userId: Option[Long], artist: String, title: String)

case class User(id: Option[Long],
                email: String,
                password: String,
                username: String,
                role: Role,
                activated: Boolean
               ) extends IdentitySilhouette {
  def key = email
}

case class UserInfo(userId: Long, name: String, location: String)

case class AdminTool(id: Option[Long] = None, adminId: Option[Long], startingDate : Option[DateTime],
                     endingDate: Option[DateTime], announcement: Option[String], lastUpdateTime: Option[DateTime], event: Option[String])

case class Page[A](items: Seq[A], page: Int, offset: Long, total: Long) {
  lazy val prev = Option(page - 1).filter(_ >= 0)
  lazy val next = Option(page + 1).filter(_ => (offset + items.size) < total)
}

case class Student(id: Option[Long],
                   name: String,
                   teamName: String,
                   institution: String,
                   country: String,
                   league: String,
                   subLeague: String,
                   event: String,
                   lastUpdateTime: Option[DateTime],
                   updateBy: Option[Long]
                  )