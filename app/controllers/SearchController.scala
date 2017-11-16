package controllers

import javax.inject.Inject

import dao.{GeneIdDao, MRNAProfileDao, PasswordDao}
import play.api.libs.json.Json
import play.api.mvc._
import models.Tables._
import utils.Utils

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.math.log10

class SearchController  @Inject()(passwordDao: PasswordDao, geneIdDao: GeneIdDao, mRNAProfileDao: MRNAProfileDao)extends Controller {


  def toIndex = Action {implicit request =>
    Ok(views.html.search.index())
  }

//得到输入的基因ID
  def selectByPosition(id: String,sampleName:String) : Action[AnyContent]= Action { implicit request =>
    val idStr = id.split(",").distinct.mkString(",")
    val samStr = sampleName.split(",").distinct.mkString(",")
    val isGeneId = Await.result(geneIdDao.selectAll, Duration.Inf).map(_.id).mkString(",").contains(idStr)
    val isSampleName = Await.result(mRNAProfileDao.selectAllSampleName, Duration.Inf).mkString(",").contains(samStr)
    println(isGeneId,isSampleName)
    if(isGeneId && isSampleName){
      Ok(views.html.search.result(idStr, samStr))
    }else{
      Redirect(routes.SearchController.toIndex()).flashing("info" -> "The information database you entered does not exist!")
    }
  }

  def linear(id: String,sampleName:String): Action[AnyContent] = Action.async { implicit request =>
    //获得以逗号分隔和除去空格的id
    val genes = id.split(",").map(_.trim)
    val sams = sampleName.split(",").map(_.trim)
    //根据id得到信息
    val mRNAProfiles = mRNAProfileDao.selectByPosition(id,sampleName)
  //  println(mRNAProfiles.value)
    mRNAProfiles.map(tmpX => Ok {
      val infos = genes.map { gene =>
        val line = tmpX.filter(_.geneid.toUpperCase == gene.toUpperCase)
        val sampleNames = line.map(_.samplename).distinct
        val datas = line.map(_.value)
        val name = line.head.geneid
        (sampleNames, datas, name)
      }
      val maxCategory = infos.flatMap(_._1).distinct
      val jsons = infos.map(y => Json.obj("category" -> y._1, "data" -> y._2, "name" -> y._3))
      val finalJsons = Json.obj("infos" -> jsons, "maxCategory" -> maxCategory)
      Json.toJson(finalJsons)
    })
  }

  def heatmap(id: String,sampleName: String): Action[AnyContent] = Action.async { implicit request =>
    //获取文本的值
    val genes = id.split(",").map(_.trim)
    val sams = sampleName.split(",").map(_.trim)
    val mRNAProfiles = mRNAProfileDao.selectByPosition(id,sampleName)
    mRNAProfiles.map(tmpX => Ok {
      val samples = tmpX.map(_.samplename).distinct
      val newGenes = genes.map { gene =>
        tmpX.filter(_.geneid.toUpperCase == gene.toUpperCase).head.geneid
      }
      val expressions = for (i <- sams.indices; j <- genes.indices) yield {
        //找到符合id又符合样品名的值
        val mRNAProfile = tmpX.filter(_.geneid.toUpperCase == genes(j).toUpperCase).find(_.samplename == sams(i))
        mRNAProfile match {
          case Some(mRNAProfile1) => Array(i, j, log2(mRNAProfile1.value + 1))
          case _ => Array(i, j, log2(0.0 + 1))
        }
      }
      val jsons = Array(Json.obj("expression" -> expressions, "treatment" -> samples, "gt" -> newGenes))
      Json.toJson(jsons)
    })
  }

  //log10(x):10的多少次方等于x
  def log2(x: Double): Double = log10(x) / log10(2.0)

}