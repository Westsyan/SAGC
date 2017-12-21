package controllers

import javax.inject.Inject

import dao.{GeneIdDao, MRNAProfileDao, PasswordDao}
import play.api.data.Form
import play.api.data.Forms._
import play.api.libs.json.Json
import play.api.mvc.{Action, Controller}

import scala.concurrent.ExecutionContext.Implicits.global

class CheckController @Inject()(passwordDao: PasswordDao, geneIdDao: GeneIdDao, mRNAProfileDao: MRNAProfileDao) extends Controller {

  case class coData(id: String, rvalue: String)

  val coForm = Form(
    mapping(
      "id" -> text,
      "rvalue" -> text
    )(coData.apply)(coData.unapply)
  )

  def checkCo = Action.async { implicit request =>
    val data = coForm.bindFromRequest.get
    val id = data.id
    val rvalue = data.rvalue
    val geneid = id.split(",").map(_.trim).distinct
    geneIdDao.selectById(id).map{x=>
      val judge1 = x.size == geneid.size
      val judge2 = rvalue.toDouble >0 && rvalue.toDouble <1
      val judge = judge1 && judge2
      var valids = "false"
      if(judge == false){valids = "true"}
      var jsons = ""
      if(judge1 == false) {
        jsons = "The Gene Symbol:" + geneid.diff(x).mkString(",") + " not in database!"
      }else if(judge2 == false){
        jsons = "The r-value nonconformity"
      }
        val json=Json.obj("valids"->valids,"messages"->jsons)
      Ok(json)
    }
  }
}
