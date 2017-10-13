package controllers

import java.util.Date
import javax.inject.Inject

import com.github.tototoshi.csv.CSVReader
import com.mohiva.play.silhouette.api.Silhouette
import dao.{StudentDao, UserDao}
import forms.Forms
import models._
import play.api.i18n.{I18nSupport, Messages}
import play.api.mvc.{AbstractController, ControllerComponents, Flash}
import utils.Silhouette.{AuthController, MyEnv}
import utils.Utils

import scala.concurrent.{ExecutionContext, Future}

/**
  * Created by aknay on 5/12/2016.
  */
/** we need to use I18nSupport because we are using form helper
  * Without this --> Form: could not find implicit value for parameter messages: play.api.i18n.Messages
  * Ref: http://stackoverflow.com/questions/30799988/play-2-4-form-could-not-find-implicit-value-for-parameter-messages-play-api-i
  * */
class StudentController @Inject()(components: ControllerComponents,
                                  studentDao: StudentDao,
                                  userDao: UserDao)
                                 (val silhouette: Silhouette[MyEnv])(implicit exec: ExecutionContext)
  extends AbstractController(components) with I18nSupport with AuthController {
  /** we can use album form directly with Album case class by applying id as Option[Long] */

  def viewStudentForm = SecuredAction.async { implicit request =>
    val user = Some(request.identity)
    Future.successful(Ok(views.html.Student.add(user, Forms.studentForm)))
  }

  def submitStudentForm = SecuredAction.async { implicit request =>
    val user: User = request.identity
    val stduentForm = Forms.studentForm.bindFromRequest()
    stduentForm.fold(
      hasErrors = { form =>
        println("we are having error, try to check form data is matched with html")
        println(form.data)
        Future.successful(Redirect(routes.HomeController.index()))
      },
      success = {
        newAlbum =>
          studentDao.insertByUserId(newAlbum, request.identity.id.get).map {
            case true => Redirect(routes.StudentController.viewStudentForm()) flashing (
              "success" -> Messages("student.added.success"))

            case false => Redirect(routes.StudentController.viewStudentForm()) flashing (Flash(stduentForm.data) +
              ("error" -> Messages("student.alreadyExisted.error")))
          }
      })
  }

  def menu = SecuredAction.async { implicit request =>
    val user = Some(request.identity)
    Future.successful(Ok(views.html.Student.menu(user)))
  }

  def viewUploadForm = SecuredAction(parse.temporaryFile) { implicit request =>
    val user = Some(request.identity)
    Ok(views.html.Student.upload(user))
  }

  def selectEventToViewStudents = SecuredAction.async { implicit request =>
    val user = Some(request.identity)
    for {
      events <- studentDao.getUniqueEventList
      eventsMap <- Future.successful(events.map(e => (e, e)))
    } yield Ok(views.html.Student.EventFormToViewStudents(user, Forms.eventForm, eventsMap))
  }

  val country = "Country"
  val teamName = "Team Name"
  val institution = "Institution"
  val league = "League"
  val subLeague = "League (Sub-Category)"
  val studentName = "Student Name"
  val eventName = "Event"

  private def isFollowingFormat(map: Map[String, String]): Boolean = {
    map.get(country).isDefined &&
      map.get(teamName).isDefined &&
      map.get(institution).isDefined &&
      map.get(league).isDefined &&
      map.get(subLeague).isDefined &&
      map.get(studentName).isDefined &&
      map.get(eventName).isDefined
  }

  private def getStudentFromMap(userId: Long, map: Map[String, String]): Student = {
    Student(Some(1), map(studentName), map(teamName), map(institution), map(country), map(league),
      map(subLeague), map(eventName), Some(Utils.getTimeStampFromDate(new Date())), updateBy = Some(userId))
  }

  def submitUploadForm = SecuredAction.async(parse.multipartFormData) { implicit request =>
    val user = Some(request.identity)
    val students: Future[Seq[StudentWithStatus]] = request.body.file("csv").map { csv =>
      val reader = CSVReader.open(csv.ref.file)

      val v: Seq[Map[String, String]] = reader.allWithHeaders()

      val userId = user.get.id.get
      val students: Seq[Student] = v.filter(a => isFollowingFormat(a))
        .map(b => getStudentFromMap(userId, b))

      val studentWithStatusList: Seq[Future[StudentWithStatus]] = students.map {
        s =>
          studentDao.insertByUserId(s, userId)
            .map(a => StudentWithStatus(s.name, s.teamName, s.institution, s.country, s.league, s.subLeague, s.event, isExisted = !a))
      }
      Future.sequence(studentWithStatusList)
    }.get

    for {
      s <- students
    } yield Ok(views.html.Student.ListStudentsWithTableAfterUpload(user, s))

  }

  private def combineLeagueAndSubLeague(league: String, subLeague: String): String = {
    league + "," + subLeague
  }

  def view(event: String, combinedLeagueName: String) = SecuredAction.async { implicit request =>
    val user = Some(request.identity)
    val totalNumberOfCombinedLeague: Future[Seq[(String, String)]] = studentDao.getLeagueInfoListByEvent(event).map {
      a => a.map(b => (combineLeagueAndSubLeague(b.league, b.subLeague), combineLeagueAndSubLeague(b.league, b.subLeague)))
    }

    val submittedLeagueInfo: Option[LeagueInfo] = LeagueInfo.unapply(combinedLeagueName)
    if (submittedLeagueInfo.isEmpty) {
      for {
        t <- totalNumberOfCombinedLeague
        studentsPerEvent <- studentDao.getStudentsPerEvent(event)
        l <- studentDao.getLeagueInfoListByEvent(event)
        b <- studentDao.getStudentsPerCombinedLeague(event)
        totalSizeInfo <- studentDao.getTotalSizeInfoPerEvent(event)
      //we need to pre-fill the event//if we pass the event by value, it will only read first few words before space//strange
      //all is added to view 'All students from events"
      } yield Ok(views.html.Student.ListStudentPerEventPerCombinedLeague(user, Forms.leagueForm.fill(CombinedLeagueNameWithEvent("", event)), t :+ ("all", "All"), b, Some(totalSizeInfo)))
    }
    else {
      for {
      //we need to pre-fill the event
        studentsPerLeague <- studentDao.getStudentsPerCombinedLeague(event, submittedLeagueInfo.get)
        t <- totalNumberOfCombinedLeague
      } yield Ok(views.html.Student.ListStudentPerEventPerCombinedLeague(user, Forms.leagueForm.fill(CombinedLeagueNameWithEvent("", event)), t :+ ("all", "All"), Seq(studentsPerLeague), None))
    }
  }

  def submitLeagueForm = SecuredAction.async { implicit request =>
    val user = Some(request.identity)
    val leagueForm = Forms.leagueForm.bindFromRequest()
    leagueForm.fold(
      hasErrors = { form =>
        println("we are having error, try to check form data is matched with html")
        println(form.data)
        Future.successful(Redirect(routes.HomeController.index()))
      },
      success = {
        successfulLeagueForm =>
          val event = successfulLeagueForm.event
          val combinedLeagueName = successfulLeagueForm.combinedLeagueName
          Future.successful(Redirect(routes.StudentController.view(event = event, combinedLeagueName)))
      })
  }

  def update(id: Long) = SecuredAction.async { implicit request =>
    val user = Some(request.identity)
    studentDao.getStudentById(id).map {
      student => if (student.isDefined) Ok(views.html.Student.update(user, id, Forms.studentForm.fill(student.get))) else NotFound
    }
  }

  def submitUpdate(id: Long) = SecuredAction.async { implicit request =>
    val user = Some(request.identity)
    val studentForm = Forms.studentForm.bindFromRequest()

    studentForm.fold(
      hasErrors = {
        errorForm =>
          println("we are having error, try to check form data is matched with html")
          println(errorForm.data)
          Future.successful(Ok(views.html.Student.update(user, id, errorForm)))
      },
      success = { student =>

        for {
          _ <- studentDao.updateStudentById(id, user.get, student)
        } yield Redirect(routes.StudentController.view(combineLeagueAndSubLeague(student.league, student.subLeague)))
      })
  }

  def delete(id: Long) = SecuredAction.async { implicit request =>
    for {
      student <- studentDao.getStudentById(id) //we want to know which league student belonged to
      _ <- studentDao.deleteStudentById(id)
    } yield Redirect(routes.StudentController.view(combineLeagueAndSubLeague(student.get.league, student.get.subLeague)))
  }

  def selectEventToViewReport = SecuredAction.async { implicit request =>
    val user = Some(request.identity)
    for {
      eventList <- studentDao.getUniqueEventList
      mapEventList <- Future.successful(eventList.map(e => (e, e)))
    } yield Ok(views.html.Student.EventFormToViewReport(user, Forms.eventForm, mapEventList))
  }

  def selectEventToDeleteLeagues = SecuredAction.async { implicit request =>
    val user = Some(request.identity)
    for {
      eventList <- studentDao.getUniqueEventList
      mapEventList <- Future.successful(eventList.map(e => (e, e)))
    } yield Ok(views.html.Student.EventFormToDeleteLeagues(user, Forms.eventForm, mapEventList))
  }

  def submitEventToViewReport = SecuredAction.async { implicit request =>

    val user = Some(request.identity)
    val eventForm = Forms.eventForm.bindFromRequest()

    eventForm.fold(
      hasErrors = {
        errorForm =>
          println("we are having error, try to check form data is matched with html")
          println(errorForm.data)
          Future.successful(Ok)
      },
      success = { event =>
        for {
          _ <- studentDao.getStudentsPerEvent(event)
        } yield Redirect(routes.StudentController.overviewReport(event))
      })
  }

  def submitEventToViewStudents = SecuredAction.async { implicit request =>

    val user = Some(request.identity)
    val eventForm = Forms.eventForm.bindFromRequest()

    eventForm.fold(
      hasErrors = {
        errorForm =>
          println("we are having error, try to check form data is matched with html")
          println(errorForm.data)
          Future.successful(Ok)
      },
      success = { event =>
        for {
          _ <- studentDao.getStudentsPerEvent(event)
        } yield Redirect(routes.StudentController.view(event))
      })
  }

  def submitEventToDeleteLeagues = SecuredAction.async { implicit request =>

    val user = Some(request.identity)
    val eventForm = Forms.eventForm.bindFromRequest()

    eventForm.fold(
      hasErrors = {
        errorForm =>
          println("we are having error, try to check form data is matched with html")
          println(errorForm.data)
          Future.successful(Ok) //TODO
      },
      success = { event =>
        Future.successful(Redirect(routes.StudentController.viewLeagueListToDelete(event)))
      })
  }

  def viewLeagueListToDelete(event: String) = SecuredAction.async { implicit request =>
    val user = Some(request.identity)
    for {
      leagueList <- studentDao.getLeagueListByEvent(event)
    } yield Ok(views.html.Student.ListLeaguesWithTableToDelete(user, event, leagueList))
  }

  def askBeforeDeletingALeague(event: String, league: String) = SecuredAction.async { implicit request =>
    val user = Some(request.identity)
    Future.successful(Ok(views.html.Student.ConfirmationBeforeDeletingALeague(user, event, league)))
  }

  def confirmDeletingALeague(event: String, league: String) = SecuredAction.async { implicit request =>
    for {
      _ <- studentDao.deleteStudentByEventAndLeague(event, league)
    } yield Redirect(routes.StudentController.viewLeagueListToDelete(event)) flashing (
      "success" -> Messages("league.delete.success", league, event))
  }

  def overviewReport(event: String) = SecuredAction.async { implicit request =>
    val user = Some(request.identity)
    for {
      a <- studentDao.getStudentsPerLeagueInfo(event)
      totalSizeInfo <- studentDao.getTotalSizeInfoPerEvent(event)
    } yield Ok(views.html.Student.ReportPerEvent(user, a, totalSizeInfo))
  }

}