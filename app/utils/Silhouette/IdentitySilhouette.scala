package utils.Silhouette

/**
  * Created by aknay on 30/1/17.
  */

import com.mohiva.play.silhouette.api.{Identity, LoginInfo}
import Implicits._

trait IdentitySilhouette extends Identity {
  def key: String

  def loginInfo: LoginInfo = key
}