package controllers

import play.api._
import play.api.mvc._

import game._

class Application extends Controller {

  def home = Action { implicit request =>
    Ok(views.html.ascii(MapManager.generateWholeMap(20)))
  }

}
