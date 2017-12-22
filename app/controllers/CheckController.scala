package controllers

import javax.inject.Inject

import dao.{GeneIdDao, MRNAProfileDao, PasswordDao}
import play.api.data.Form
import play.api.data.Forms._
import play.api.libs.json.Json
import play.api.mvc.{Action, Controller}

import scala.concurrent.ExecutionContext.Implicits.global

class CheckController @Inject()(passwordDao: PasswordDao, geneIdDao: GeneIdDao, mRNAProfileDao: MRNAProfileDao) extends Controller {

  case class sampleData(sampleName: String)

  val sampleForm = Form(
    mapping(
      "sampleName" -> text
    )(sampleData.apply)(sampleData.unapply)
  )

  def checkSample = Action.async { implicit request =>
    val data = sampleForm.bindFromRequest.get
    val sample = data.sampleName
    val samplename = sample.split(",").map(_.trim).distinct
    mRNAProfileDao.selectBySampleName(sample).map { x =>
      var valid = "true"
      var message = ""
      if (x.size != samplename.size) {
        valid = "false"
        val invalidSampleName = samplename.diff(x).mkString(",")
        message = "The " + invalidSampleName + " not in database!"
      }
      val json = Json.obj("valid" -> valid, "message" -> message)
      Ok(Json.toJson(json))
    }
  }

  case class geneIdData(id: String)

  val geneIdForm = Form(
    mapping(
      "id" -> text
    )(geneIdData.apply)(geneIdData.unapply)
  )

  def checkGeneId = Action.async { implicit request =>
    val data = geneIdForm.bindFromRequest.get
    val id = data.id
    val geneId = id.split(",").map(_.trim).distinct
    geneIdDao.selectById(id).map { x =>
      var valid = "true"
      var message = ""
      if (x.size != geneId.size) {
        valid = "false"
        val invalidGeneId = geneId.diff(x).mkString(",")
        message = "The " + invalidGeneId + " not in database!"
      }
      println(valid)
      println(x.size)
      println(geneId.size)
      val json = Json.obj("valid" -> valid, "message" -> message)
      Ok(Json.toJson(json))
    }
  }

  case class groupData1(group1:String)

  val group1Form = Form(
    mapping(
      "group1" -> text
    )(groupData1.apply)(groupData1.unapply)
  )

  def checkGroup1 = Action.async { implicit request =>
    val data = group1Form.bindFromRequest.get
    val sample = data.group1
    val samplename = sample.split(",").map(_.trim).distinct
    mRNAProfileDao.selectBySampleName(sample).map { x =>
      var valid = "true"
      var message = ""
      if (x.size != samplename.size) {
        valid = "false"
        val invalidSampleName = samplename.diff(x).mkString(",")
        message = "The " + invalidSampleName + " not in database!"
      }
      val json = Json.obj("valid" -> valid, "message" -> message)
      Ok(Json.toJson(json))
    }
  }

  case class groupData2(group2:String)

  val group2Form = Form(
    mapping(
      "group2" -> text
    )(groupData2.apply)(groupData2.unapply)
  )

  def checkGroup2 = Action.async { implicit request =>
    val data = group2Form.bindFromRequest.get
    val sample = data.group2
    val samplename = sample.split(",").map(_.trim).distinct
    mRNAProfileDao.selectBySampleName(sample).map { x =>
      var valid = "true"
      var message = ""
      if (x.size != samplename.size) {
        valid = "false"
        val invalidSampleName = samplename.diff(x).mkString(",")
        message = "The " + invalidSampleName + " not in database!"
      }
      val json = Json.obj("valid" -> valid, "message" -> message)
      Ok(Json.toJson(json))
    }
  }
}
