package controllers

import javax.inject.Inject

import dao.{GeneIdDao, GeneInformationDao, MRNAProfileDao, PasswordDao}
import models.Tables._
import org.apache.commons.math3.stat.descriptive.moment.StandardDeviation
import play.api.libs.json.Json
import play.api.mvc.{Action, AnyContent, Controller}

import scala.concurrent.ExecutionContext.Implicits.global

class DownLoadController @Inject()(passwordDao: PasswordDao, geneIdDao: GeneIdDao, mRNAProfileDao: MRNAProfileDao,geneInformationDao:GeneInformationDao) extends Controller  {

  def toIndex = Action {
    Ok(views.html.download.index())
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
