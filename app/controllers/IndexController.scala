package controllers

import play.api.mvc._

class IndexController extends Controller{

  def toIndex = Action {
    Ok(views.html.English.index())
  }
}
