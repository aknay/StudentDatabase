package models

import java.sql.Timestamp

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

case class AdminTool(id: Option[Long] = None, adminId: Option[Long], startingDate: Option[Timestamp],
                     endingDate: Option[Timestamp], announcement: Option[String], lastUpdateTime: Option[Timestamp], event: Option[String])

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
                   lastUpdateTime: Option[Timestamp],
                   updateBy: Option[Long]
                  )

case class StudentWithStatus(
                              name: String,
                              teamName: String,
                              institution: String,
                              country: String,
                              league: String,
                              subLeague: String,
                              event: String,
                              isExisted: Boolean
                            )

case class LeagueInfo(league: String, subLeague: String)

object LeagueInfo {
  def unapply(x: String): Option[LeagueInfo] = {
    val a = x.split(",")
    if (a.size == 2) Some(LeagueInfo(a {0}, a {1})) else None
  }
}

case class StudentsPerCombinedLeague(leagueInfo: LeagueInfo,
                                     students: Seq[Student],
                                     studentSize: Int,
                                     numberOfLocalStudent: Int,
                                     numberOfInternationalStudent: Int,
                                     teamSize: Int,
                                     localTeamSize: Int,
                                     internationalTeamSize: Int,
                                     studentDistribution: Double,
                                     teamDistribution: Double
                                    )

case class StudentsPerLeague(league: String,
                             students: Seq[Student],
                             studentSize: Int,
                             numberOfLocalStudent: Int,
                             numberOfInternationalStudent: Int,
                             teamSize: Int,
                             localTeamSize: Int,
                             internationalTeamSize: Int,
                             studentDistribution: Double,
                             teamDistribution: Double)

case class TotalSizeInfo(numberOfStudent: Int,
                         numberOfTeam: Int,
                         numberOfLocalTeam: Int,
                         numberOfInternationalTeam: Int,
                         numberOfLocalStudent: Int,
                         numberOfInternationalStudent: Int
                        )

case class StudentsPerLeagueInfo(studentsOfPerLeague: StudentsPerLeague, listOfStudentPerCombinedLeague: Seq[StudentsPerCombinedLeague])

case class CombinedLeagueNameWithEvent(combinedLeagueName: String, event: String)

