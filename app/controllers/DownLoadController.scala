package controllers

import javax.inject.Inject

import dao.{GeneIdDao, GeneInformationDao, MRNAProfileDao, PasswordDao}
import models.Tables._
import org.apache.commons.math3.stat.descriptive.moment.StandardDeviation
import play.api.data.Form
import play.api.data.Forms._
import play.api.libs.json.Json
import play.api.mvc.{Action, AnyContent, Controller}

import scala.concurrent.ExecutionContext.Implicits.global

case class downData(id:String,sampleName:String)
case class par(sampleName:String,value:Double)

class DownLoadController @Inject()(passwordDao: PasswordDao, geneIdDao: GeneIdDao, mRNAProfileDao: MRNAProfileDao,geneInformationDao:GeneInformationDao) extends Controller  {

  def toIndex = Action {
    Ok(views.html.download.index())
  }

  val form = Form(
    mapping(
      "id" -> text,
      "sampleName" -> text
    )(downData.apply)(downData.unapply)
  )

  def checkPostion = Action.async { implicit request =>
    val data = form.bindFromRequest.get
    val id = data.id
    val sampleName = data.sampleName
    if(id.isEmpty){
      val samStr = data.sampleName.split(",").map(_.trim).distinct
        mRNAProfileDao.selectBySampleName(sampleName).map{ y=>
          val judge =  y.size == samStr.length
          var valids = "false"
          if(judge == false){valids = "true"}
          val invalidSampleName = samStr.diff(y).mkString(",")
          val jsons = "The "+ invalidSampleName + " not in database!"
          val json=Json.obj("valids"->valids,"messages"->jsons)
          Ok(json)
      }
    }else {
      val idStr = data.id.split(",").map(_.trim).distinct
      val samStr = data.sampleName.split(",").map(_.trim).distinct
      geneIdDao.selectById(id).flatMap { x =>
        mRNAProfileDao.selectBySampleName(sampleName).map { y =>
          val judge = x.size == idStr.length && y.size == samStr.length
          var valids = "false"
          if (judge == false) {
            valids = "true"
          }
          val invalidGeneId = idStr.diff(x).mkString(",")
          val invalidSampleName = samStr.diff(y).mkString(",")
          val jsons = "The " + invalidGeneId + " " + invalidSampleName + " not in database!"
          val json = Json.obj("valids" -> valids, "messages" -> jsons)
          Ok(json)
        }
      }
    }
  }

  def getName(sampleName : String) :Action[AnyContent] = Action{implicit request =>
   val sam = sampleName.split(",").distinct.toSeq
    Ok(Json.toJson(sam))
  }

  def selectAllgene(id:String,sampleName:String): Action[AnyContent] = Action.async { implicit request =>
    if(id.isEmpty){
      mRNAProfileDao.selectAllBySampleName(sampleName).map { info =>
        val array = getArrayByGenotypes(info)
        Ok(Json.obj("array" -> array))
      }
    }else{
      mRNAProfileDao.selectByPosition(id,sampleName)map{info=>
        val array = getArrayByGenotypes(info)
        Ok(Json.obj("array" -> array))
      }
    }
  }

  def getArrayByGenotypes(x: Seq[MrnaprofileRow]) = {
    x.groupBy(_.geneid).map {
      case (geneid, sample) =>
        val genenameStr = "<a target='_blank' href='" + routes.GeneInformationController.getMoreInfo(geneid) + "'>" + geneid + "</a>"
        val map1 = Map("geneId" -> genenameStr)
        val map2 = sample.map(y => y.samplename -> y.value.toString).toMap
        val map = map1 ++ map2
        map
    }
  }
}
