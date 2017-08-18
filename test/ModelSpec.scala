///**
//  * Created by aknay on 6/1/17.
//  */
//
//import org.scalatestplus.play.PlaySpec
//import play.api.Application
//import dao.{AdminToolDao, AlbumDao, UserDao}
//import org.joda.time.DateTime
//import org.scalatest.BeforeAndAfterEach
//import org.scalatestplus.play.guice.GuiceOneAppPerSuite
//import views.html.Template.pages
//import scala.concurrent.ExecutionContext.Implicits.global
//import scala.concurrent.{Await, Future}
//import scala.concurrent.duration._
//
//class ModelSpec extends PlaySpec with BeforeAndAfterEach with GuiceOneAppPerSuite {
//
//  import models._
//
//  def userDao(implicit app: Application) = {
//    val app2UserDAO = Application.instanceCache[UserDao]
//    app2UserDAO(app)
//  }
//
//  def albumDao(implicit app: Application) = {
//    val app2AlbumDAO = Application.instanceCache[AlbumDao]
//    app2AlbumDAO(app)
//  }
//
//  def adminToolDao(implicit app: Application) = {
//    val app2AdminToolDAO = Application.instanceCache[AdminToolDao]
//    app2AdminToolDAO(app)
//  }
//
//
//  val EMAIL_NAME1 = "qwe@qwe.com"
//  val PASSWORD1 = "qwe"
//  val EMAIL_NAME2 = "abc@abc.com"
//  val PASSWORD2 = "abc"
//  val EMAIL_NAME3 = "def@def.com"
//  val EMAIL_NAME4 = "ghi@ghi.com"
//
//  override def beforeEach(): Unit = {
//    userDao.createUserTableIfNotExisted
//    userDao.createUserInfoTableIfNotExisted
//    //    adminToolDao.createTableIfNotExisted
//  }
//
//  def getMasterUser(email: String): User = {
//    User(Some(1), email, "just email", "user name", Role.Admin, true)
//  }
//
//  def getNormalUser(email: String): User = {
//    User(Some(1), email, "just email", "user name", Role.NormalUser, true)
//  }
//
//  "User Model" should {
//
//    "insert data and check its existence" in {
//      val user = getNormalUser(EMAIL_NAME1)
//      userDao.insertUserWithUserInfo(user) mustBe true
//      userDao.insertUserWithUserInfo(user) mustBe false
//    }
//
//    "should get non admin users" in {
//      userDao.insertUserWithUserInfo(getMasterUser(EMAIL_NAME1)) mustBe true
//      userDao.insertUserWithUserInfo(getMasterUser(EMAIL_NAME2)) mustBe true
//      userDao.insertUserWithUserInfo(getNormalUser(EMAIL_NAME3)) mustBe true
//      userDao.insertUserWithUserInfo(getNormalUser(EMAIL_NAME4)) mustBe true
//
//      val nonAdminUserList: Seq[User] = userDao.getNonAdminUserList()
//
//      val admin1 = userDao.getUserByEmailAddress(EMAIL_NAME1)
//      val admin2 = userDao.getUserByEmailAddress(EMAIL_NAME2)
//      val user1 = userDao.getUserByEmailAddress(EMAIL_NAME3)
//      val user2 = userDao.getUserByEmailAddress(EMAIL_NAME4)
//
//      nonAdminUserList.contains(admin1.get) mustBe false
//      nonAdminUserList.contains(admin2.get) mustBe false
//      nonAdminUserList.contains(user1.get) mustBe true
//      nonAdminUserList.contains(user2.get) mustBe true
//
//    }
//
//    "should get user info" in {
//      userDao.insertUserWithUserInfo(getMasterUser(EMAIL_NAME1)) mustBe true
//      val admin = userDao.getUserByEmailAddress(EMAIL_NAME1)
//      admin.isDefined mustBe true
//      val userInfo = userDao.getUserInfo(admin.get)
//      userInfo.isDefined mustBe true
//      userInfo.get.name contains "EMPTY" mustBe true
//      userInfo.get.location contains "EMPTY" mustBe true
//      userDao.updateUserInfo(admin.get, "User", "planet")
//      val updatedUserInfo = userDao.getUserInfo(admin.get)
//      updatedUserInfo.isDefined mustBe true
//      updatedUserInfo.get.name contains "User" mustBe true
//      updatedUserInfo.get.location contains "planet" mustBe true
//    }
//  }
//
//  "Admin Tool Model" should {
//
//    "should create announcement" in {
//      userDao.insertUserWithUserInfo(getMasterUser(EMAIL_NAME1))
//      val user = userDao.getUserByEmailAddress(EMAIL_NAME1).get
//      val announcement = "This is an AAA announcement"
//      val firstAdminTool = Await.result(adminToolDao.getAdminTool, Duration.Inf)
//      println("first admin tool" + firstAdminTool.get)
//
//      for {
//        adminTool <- adminToolDao.getAdminTool
//        action <- adminToolDao.createAnnouncement(user, adminTool.get, announcement, DateTime.now(), DateTime.now())
//      } yield action
//
//      val secondAdminTool = Await.result(adminToolDao.getAdminTool, Duration.Inf)
//      println("second admin tool" + secondAdminTool.get)
//
//    }
//
//
//
//    "normal user cannot make announcement" in {
//      userDao.insertUserWithUserInfo(getNormalUser(EMAIL_NAME1))
//      val user = userDao.getUserByEmailAddress(EMAIL_NAME1)
//      //TODO adminToolDao.makeAnnouncement(user.get, DateTime.now(), DateTime.now(), "") mustBe false
//    }
//
//    "inserted admin user can make announcement in admin tool table" in {
//      userDao.insertUserWithUserInfo(getMasterUser(EMAIL_NAME1))
//      val user = userDao.getUserByEmailAddress(EMAIL_NAME1).get
//      val startingDateNow = DateTime.now()
//      val endingDateNow = DateTime.now()
//      val announcement = "This is an announcement"
//      adminToolDao.createAdminToolIfNotExisted
//      val tool = Await.result(adminToolDao.getAdminTool, 10 seconds)
//      tool.isDefined mustBe true
//      //      println("time is" + tool.get.lastUpdateTime.get)
//
//      //TODO adminToolDao.makeAnnouncement(user, startingDateNow, endingDateNow, announcement) mustBe true
//
//
//      val adminTool = Await.result(adminToolDao.getAdminTool, 10 seconds)
//      adminTool.isDefined mustBe true
//      //      println("time is" + adminTool.get.lastUpdateTime.get)
//      //      println("announcemnet is" + adminTool.get.announcement.get)
//
//
//      //      val startingDate: Future[Option[Option[DateTime]]] = adminToolDao.getStartingDate
//      val startingDateResult: Option[Option[DateTime]] = Await.result(adminToolDao.getStartingDate, 10 seconds)
//      startingDateResult.get.isDefined mustBe true
//      startingDateResult.get.get.compareTo(startingDateNow) mustBe 0 //0 is same for both date// less than 0// more than 0
//
//      val endingDate = adminToolDao.getEndingDate
//      val endingDateResult: Option[Option[DateTime]] = Await.result(endingDate, 10 seconds)
//      endingDateResult.get.isDefined mustBe true
//      endingDateResult.get.get.compareTo(endingDateNow) mustBe 0
//
//      val announcementResult = Await.result(adminToolDao.getAnnouncement, 10 seconds)
//
//
//      announcementResult.get.get.compareTo(announcement) mustBe 0
//    }
//
//    "inserted admin user can delete Announcement" in {
//      userDao.insertUserWithUserInfo(getMasterUser(EMAIL_NAME1))
//      val user = userDao.getUserByEmailAddress(EMAIL_NAME1).get
//      val announcement = "This is an announcement"
//
//      for {
//        adminTool <- adminToolDao.getAdminTool
//        action <- adminToolDao.createAnnouncement(user, adminTool.get, announcement, DateTime.now(), DateTime.now())
//      } yield action
//
////      Await.result(adminToolDao.getAdminTool, 1 seconds).get.announcement.isDefined mustBe true
//      val tool = Await.result(adminToolDao.getAdminTool, 10 seconds)
//      println("tool here is" + tool.get)
//
//      val adminToolAfterDelete = for {
//        adminTool <- adminToolDao.getAdminTool
//        action <- adminToolDao.deleteAnnouncement(user, adminTool.get)
//        adminToolAfterDelete <- adminToolDao.getAdminTool
//      } yield {
//        adminToolAfterDelete
//      }
//
//      val adminTool: Option[AdminTool] = Await.result(adminToolAfterDelete, 1 seconds)
//
////      print("admin too here is " + adminTool)
//      //      adminToolDao.deleteAnnouncement(user)
//      //      val adminTool = Await.result(adminToolDao.getAdminTool, 10 seconds)
//      adminTool.get.announcement mustBe None
//      adminTool.get.startingDate mustBe None
//      adminTool.get.endingDate mustBe None
//    }
//
//    //    "get latest Announcement" in {
//    //      userDao.insertUserWithUserInfo(getMasterUser(EMAIL_NAME1))
//    //      val user1 = userDao.getUserByEmailAddress(EMAIL_NAME1).get
//    //      val announcement1 = "This is an announcement from user 1"
//    //      adminToolDao.makeAnnouncement(user1, DateTime.now(), DateTime.now(), announcement1) mustBe true
//    //      val user1LastUpdatedTime = adminToolDao.getAdminTool.get.lastUpdateTime.get
//    //      val latestUpdatedTime = adminToolDao.getAdminTool.get.lastUpdateTime.get
//    //      latestUpdatedTime.compareTo(user1LastUpdatedTime) mustBe 0
//    //      adminToolDao.getAnnouncement.get.compareTo(announcement1) mustBe 0
//    //    }
//
//    "isEventExisted function should pass" in {
//      val result = adminToolDao.isEventExisted("test", "test test1 test2")
//      result mustBe true
//
//      val secondResult = adminToolDao.isEventExisted("txzy", "test test1 test2")
//      secondResult mustBe false
//
//    }
//
//    //    "should add event" in {
//    //
//    //      val masterUser = getMasterUser(EMAIL_NAME1)
//    //      userDao.insertUserWithUserInfo(masterUser) mustBe true
//    //
//    //      val retrievedMasterUser = userDao.getUserByEmailAddress(masterUser.email)
//    //      retrievedMasterUser.isDefined mustBe true
//    //      adminToolDao.deleteAllEvents(retrievedMasterUser.get)
//    //      adminToolDao.addEvent(retrievedMasterUser.get, "test") mustBe true
//    //      adminToolDao.addEvent(retrievedMasterUser.get, "test") mustBe false //we cant add same event
//    //      adminToolDao.addEvent(retrievedMasterUser.get, "Test") mustBe false //we cant add same event even the case is different
//    //      adminToolDao.addEvent(retrievedMasterUser.get, "Test1") mustBe true //
//    //      println("result" + adminToolDao.getEvent.get)
//    //      adminToolDao.getEvent.get.compareTo("test Test1") == 0 mustBe true
//    //
//    //    }
//
//  }
//
//
//  "Album Model" should {
//    "Album Dao" should {
//      "insert data and check its existence" in {
//        val user = getNormalUser(EMAIL_NAME1)
//        userDao.insertUserWithUserInfo(user) mustBe true
//        val userTemp = userDao.getUserByEmailAddress(EMAIL_NAME1);
//        userTemp.isDefined mustBe true
//        val album = Album(Some(1), userTemp.get.id, "ARTIST", "TITLE")
//
//        albumDao.isAlbumExisted(album, userTemp.get.id.get) mustBe false //should not exist before we insert an album
//        if (userTemp.isDefined) albumDao.insertAlbum(album, userTemp.get.id.get)
//        albumDao.isAlbumExisted(album, userTemp.get.id.get) mustBe true //should exist after we insert the album
//        albumDao.delete(album, userTemp.get.id.get)
//        albumDao.isAlbumExisted(album, userTemp.get.id.get) mustBe false //should not exist after the album is deleted
//
//        albumDao.insertAlbum(album, userTemp.get.id.get)
//        albumDao.isAlbumExisted(album, userTemp.get.id.get) mustBe true //should exist after we insert the album
//        userDao.deleteUser(EMAIL_NAME1)
//        albumDao.isAlbumExisted(album, userTemp.get.id.get) mustBe false //should not exist after we delete the user
//      }
//
//      "insert data and check its existence for two user with same album and artist" in {
//        val user1 = getNormalUser(EMAIL_NAME1)
//        userDao.insertUserWithUserInfo(user1) mustBe true
//
//        val user2 = getNormalUser(EMAIL_NAME2)
//        userDao.insertUserWithUserInfo(user2) mustBe true
//
//        val userTemp1 = userDao.getUserByEmailAddress(EMAIL_NAME1);
//        userTemp1.isDefined mustBe true
//
//        val userTemp2 = userDao.getUserByEmailAddress(EMAIL_NAME2);
//        userTemp2.isDefined mustBe true
//
//
//        val album1 = Album(Some(1), userTemp1.get.id, "ARTIST", "TITLE")
//        val album2 = Album(Some(2), userTemp2.get.id, "ARTIST", "TITLE")
//
//        albumDao.isAlbumExisted(album1, userTemp1.get.id.get) mustBe false //should not exist before we insert an album
//        albumDao.isAlbumExisted(album2, userTemp2.get.id.get) mustBe false //should not exist before we insert an album
//
//        albumDao.insertAlbum(album1, userTemp1.get.id.get)
//        albumDao.insertAlbum(album2, userTemp2.get.id.get)
//
//
//        albumDao.isAlbumExisted(album1, userTemp1.get.id.get) mustBe true //should exist after we insert the album
//        albumDao.delete(album1, userTemp1.get.id.get)
//        albumDao.isAlbumExisted(album1, userTemp1.get.id.get) mustBe false //should not exist after the album is deleted
//        albumDao.isAlbumExisted(album2, userTemp2.get.id.get) mustBe true //user2 should still has album2 even album1 with same artist and title are deleted
//        albumDao.isAlbumExisted(album2, userTemp1.get.id.get) mustBe false //user1 should not have album2 info also even it has same artist and title
//        albumDao.isAlbumExisted(album1, userTemp2.get.id.get) mustBe true //should be true since we have same artist and title
//      }
//
//      "insert data and check its existence for one user with two album" in {
//        val user1 = getNormalUser(EMAIL_NAME1)
//        userDao.insertUserWithUserInfo(user1) mustBe true
//
//        val userTemp1 = userDao.getUserByEmailAddress(EMAIL_NAME1);
//        userTemp1.isDefined mustBe true
//
//        val album1 = Album(Some(1), userTemp1.get.id, "ARTIST", "TITLE")
//        val album2 = Album(Some(2), userTemp1.get.id, "ARTIST", "TITLE")
//        val album3 = Album(Some(3), userTemp1.get.id, "ARTIST_ONE", "TITLE")
//        val album4 = Album(Some(4), userTemp1.get.id, "ARTIST", "TITLE_ONE")
//
//        albumDao.insertAlbum(album1, userTemp1.get.id.get) mustBe true
//        albumDao.insertAlbum(album2, userTemp1.get.id.get) mustBe false //we can't add  both same artist and title name
//        albumDao.insertAlbum(album3, userTemp1.get.id.get) mustBe true //we can add different artist with same title name
//        albumDao.insertAlbum(album4, userTemp1.get.id.get) mustBe true //we can add same artist with different title name
//      }
//
//      "insert data and check pages" in {
//        val user1 = getNormalUser(EMAIL_NAME1)
//        userDao.insertUserWithUserInfo(user1) mustBe true
//
//        val userTemp1 = userDao.getUserByEmailAddress(EMAIL_NAME1);
//        userTemp1.isDefined mustBe true
//        val userId = userTemp1.get.id
//
//        val album1 = Album(Some(1), userId, "ARTIST", "TITLE")
//        val album2 = Album(Some(2), userId, "ARTIST", "TITLE")
//        val album3 = Album(Some(3), userTemp1.get.id, "ARTIST_ONE", "TITLE")
//        val album4 = Album(Some(4), userTemp1.get.id, "ARTIST", "TITLE_ONE")
//
//
//        albumDao.insertAlbum(album1, userTemp1.get.id.get) mustBe true
//        albumDao.insertAlbum(album2, userTemp1.get.id.get) mustBe false //we can't add  both same artist and title name
//        albumDao.insertAlbum(album3, userTemp1.get.id.get) mustBe true //we can add different artist with same title name
//        albumDao.insertAlbum(album4, userTemp1.get.id.get) mustBe true //we can add same artist with different title name
//
//
//        val pages: Future[Seq[Album]] = albumDao.retrieveByUserIdWitPageSize(userTemp1.get.id.get, 0, 10)
//        /** http://stackoverflow.com/questions/17713642/accessing-value-returned-by-scala-futures */
//        /** we cannot use map to Future type and get result to TEST.
//          * The test for both true and false are correct which is WRONG if we use map with Future and test inside the map loop.
//          * So use Await.result to get result from Future */
//        val allAlbums: Seq[Album] = Await.result(pages, 10 seconds)
//        allAlbums.size mustBe 3
//        //we don't know albumId after an album is inserted. we need get the album id back
//        val album1Id = albumDao.retrieveAlbumId("ARTIST", "TITLE", userId.get)
//        allAlbums.contains(Album(album1Id, userId, "ARTIST", "TITLE")) mustBe true
//
//        val album4Id = albumDao.retrieveAlbumId("ARTIST", "TITLE_ONE", userId.get)
//        allAlbums.contains(Album(album4Id, userId, "ARTIST", "TITLE_ONE")) mustBe true
//
//      }
//
//      "should retrieve albums" in {
//        val user1 = getNormalUser(EMAIL_NAME1)
//        userDao.insertUserWithUserInfo(user1) mustBe true
//
//        val userTemp1 = userDao.getUserByEmailAddress(EMAIL_NAME1);
//        userTemp1.isDefined mustBe true
//
//        val album1 = Album(Some(1), userTemp1.get.id, "ARTIST", "TITLE")
//        val album2 = Album(Some(2), userTemp1.get.id, "ARTIST_ONE", "TITLE")
//        val album3 = Album(Some(3), userTemp1.get.id, "ARTIST", "TITLE_ONE")
//
//        albumDao.insertAlbum(album1, userTemp1.get.id.get)
//        albumDao.insertAlbum(album2, userTemp1.get.id.get)
//        albumDao.insertAlbum(album3, userTemp1.get.id.get)
//
//        albumDao.retrieveByUserId(userTemp1.get.id.get).foreach(println)
//        val albumSeq = albumDao.retrieveByUserId(userTemp1.get.id.get)
//
//        albumSeq.contains(("ARTIST", "TITLE")) mustBe true
//        albumSeq.contains(("ARTIST_ONE", "TITLE")) mustBe true
//        albumSeq.contains(("ARTIST", "TITLE_ONE")) mustBe true
//        albumSeq.contains(("ARTIST_UNKNOWN", "TITLE")) mustBe false
//
//      }
//
//      "should update albums" in {
//        val user1 = getNormalUser(EMAIL_NAME1)
//        userDao.insertUserWithUserInfo(user1) mustBe true
//
//        val userTemp1 = userDao.getUserByEmailAddress(EMAIL_NAME1);
//        userTemp1.isDefined mustBe true
//
//        val album1 = Album(Some(1), userTemp1.get.id, "ARTIST", "TITLE")
//        val album2 = Album(Some(2), userTemp1.get.id, "ARTIST_ONE", "TITLE")
//        val album3 = Album(Some(3), userTemp1.get.id, "ARTIST", "TITLE_ONE")
//
//        albumDao.insertAlbum(album1, userTemp1.get.id.get)
//        albumDao.insertAlbum(album2, userTemp1.get.id.get)
//        albumDao.insertAlbum(album3, userTemp1.get.id.get)
//        val ARTIST_NAME_TO_UPDATE = "UPDATED ARTIST NAME"
//        val TITLE_TO_UPDATE = "UPDATED TITLE"
//        val album1Update = Album(Some(4), userTemp1.get.id, ARTIST_NAME_TO_UPDATE, TITLE_TO_UPDATE)
//
//        albumDao.update(album1.id.get, album1Update, userTemp1.get.id.get)
//
//        albumDao.retrieveByUserId(userTemp1.get.id.get).foreach(println)
//        val albumSeq: Seq[(String, String)] = albumDao.retrieveByUserId(userTemp1.get.id.get)
//
//        albumSeq.contains(("ARTIST", "TITLE")) mustBe true
//        albumSeq.contains(("ARTIST_ONE", "TITLE")) mustBe true
//        albumSeq.contains(("ARTIST", "TITLE_ONE")) mustBe true
//        albumSeq.contains(("ARTIST_UNKNOWN", "TITLE")) mustBe false
//
//        val albumId = albumDao.retrieveAlbumId("ARTIST", "TITLE", userTemp1.get.id.get)
//        albumId.isDefined mustBe true
//        albumDao.update(albumId.get, album1Update, userTemp1.get.id.get)
//
//        albumDao.retrieveByUserId(userTemp1.get.id.get).contains((ARTIST_NAME_TO_UPDATE, TITLE_TO_UPDATE)) mustBe true
//        albumDao.retrieveByUserId(userTemp1.get.id.get).size mustBe 3 //we only inserted three albums
//
//        //retrieve album by user id
//        val albumSeqFuture = albumDao.retrieveAlbumByUserId(userTemp1.get.id.get)
//        val unknownAlbum = Album(Some(4), userTemp1.get.id, "UNKNOWN ARTIST", "UNKNOWN TITLE")
//
//        val allAlbums: Seq[Album] = Await.result(albumSeqFuture, 10 seconds)
//
//        //for updated album
//        val album1Id = albumDao.retrieveAlbumId(ARTIST_NAME_TO_UPDATE, TITLE_TO_UPDATE, userTemp1.get.id.get)
//        allAlbums.contains(Album(album1Id, userTemp1.get.id, ARTIST_NAME_TO_UPDATE, TITLE_TO_UPDATE)) mustBe true
//
//        //for non-updated album
//        val album4Id = albumDao.retrieveAlbumId("ARTIST", "TITLE_ONE", userTemp1.get.id.get)
//        allAlbums.contains(Album(album4Id, userTemp1.get.id, "ARTIST", "TITLE_ONE")) mustBe true
//
//      }
//
//    }
//  }
//
//  override def afterEach(): Unit = {
//    userDao.deleteUser(EMAIL_NAME1)
//    userDao.deleteUser(EMAIL_NAME2)
//    userDao.deleteUser(EMAIL_NAME3)
//    userDao.deleteUser(EMAIL_NAME4)
//    //adminToolDao.deleteAdminTool
//
//
//  }
//}