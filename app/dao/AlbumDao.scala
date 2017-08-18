package dao

/**
  * Created by aknay on 2/12/2016.
  */

import javax.inject.Inject

import com.google.inject.Singleton
import slick.jdbc.meta.MTable

import scala.concurrent._
import scala.concurrent.duration._
import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfigProvider}
import slick.jdbc.JdbcProfile
import models.{Album, Page}

import scala.concurrent.ExecutionContext.Implicits.global

/** Ref: http://slick.lightbend.com/doc/3.0.0/schemas.html */

//import slick.driver.H2Driver.api._ //we cannot import both drivers at same place
import slick.jdbc.PostgresProfile.api._

@Singleton
class AlbumDao @Inject()(userDao: UserDao)(protected val dbConfigProvider: DatabaseConfigProvider) extends AlbumTableComponent with HasDatabaseConfigProvider[JdbcProfile] {

  //To define a mapped table that uses a custom type for
  // its * projection by adding a bi-directional mapping with the <> operator:
  //
  /** describe the structure of the tables: */
  this.createTableIfNotExisted
  userDao.createUserInfoTableIfNotExisted
  userDao.createUserTableIfNotExisted

  /** The following statements are Action */
  private lazy val createTableAction = albumTable.schema.create

  private val selectAlbumAction = albumTable.result


  /** Ref: http://slick.lightbend.com/doc/3.0.0/database.html */
  //loading database configuration
  //  private val db = Database.forConfig("testpostgresql")

  //This is the blocking method with maximum waiting time of 2 seconds
  //This is also helper method for DBIO
  //  private def exec[T](action: DBIO[T]): T = Await.result(db.run(action), 2 seconds)
  private def blockExec[T](action: DBIO[T]): T = Await.result(db.run(action), 2 seconds)

  def getAlbumTable: Future[Seq[Album]] = db.run(albumTable.result)

  def createTableIfNotExisted() {
    val x = blockExec(MTable.getTables(ALBUM_TABLE_NAME)).toList
    if (x.isEmpty) {
      blockExec(createTableAction)
    }
  }

  def count(filter: String): Future[Int] = {
    db.run(albumTable.filter { album => album.artist like filter.toLowerCase }.length.result)
  }

  def listWithPage(userId: Long, page: Int = 0, pageSize: Int = 10, orderBy: Int = 1, filter: String = "%"): Future[Page[(Album)]] = {
    /** Ref:: http://slick.lightbend.com/doc/3.0.0/queries.html */
    val offset = pageSize * page
    for {
      albumSize <- retrieveByUserId(userId)
      result <- retrieveByUserIdWitPageSize(userId, offset, pageSize)
    } yield Page(result, page, offset, albumSize.size)
  }

  def retrieveByUserIdWitPageSize(userId: Long, offset: Int, pagesize: Int): Future[Seq[Album]] = {
    db.run(albumTable.filter(_.userId === userId).drop(offset).take(pagesize).result)
  }

  def insertAlbum(album: Album, userId: Long): Future[Boolean] = {
    isAlbumExisted(album, userId).map {
      a => {
        if (a.isDefined) {
          false
        }
        else {
          val anotherAlbum: Album = Album(album.id, Some(userId), album.artist, album.title)
          db.run(albumTable += anotherAlbum)
          true
        }
      }
    }
  }

  def isAlbumExisted(album: Album, userId: Long): Future[Option[Long]] = {
    db.run(albumTable.filter(_.title === album.title)
      .filter(_.artist === album.artist)
      .filter(_.userId === userId)
      .map(_.id).result.headOption)
  }

  def retrieveByUserId(userId: Long): Future[Seq[(String, String)]] = {
    db.run(albumTable.filter(_.userId === userId).map(x => (x.artist, x.title)).result)
  }

  def delete(id: Long) {
    db.run(albumTable.filter(_.id === id).delete)
  }

  /** result.head is to get single result */
  def find(id: Long): Future[Option[Album]] = {
    db.run(albumTable.filter(_.id === id).result.headOption)
  }

  /** we make album id as Option[Long]; so we have to use Some to get id */
  def update(id: Long, album: Album, userId: Long): Unit = {
    val albumToUpdate: Album = album.copy(Some(id))
    val anotherAlbum: Album = Album(albumToUpdate.id, Some(userId), albumToUpdate.artist, albumToUpdate.title)
    db.run(albumTable.filter(_.id === id).update(anotherAlbum))
  }

  /** for testing purpose */
  def retrieveAlbumId(artist: String, title: String, userId: Long): Future[Option[Long]] = {
    db.run(albumTable.filter(_.artist === artist).filter(_.title === title).filter(_.userId === userId).map(_.id).result.headOption)
  }

}