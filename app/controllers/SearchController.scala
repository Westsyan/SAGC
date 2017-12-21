package controllers

import javax.inject.Inject

import dao.{GeneIdDao, GeneInformationDao, MRNAProfileDao, PasswordDao}
import org.apache.commons.math3.stat.StatUtils
import play.api.data.Form
import play.api.data.Forms._
import play.api.libs.json.Json
import play.api.mvc._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.math.log10

case class checkPostionData(id:String,sampleName:String)

class SearchController  @Inject()(passwordDao: PasswordDao, geneIdDao: GeneIdDao, mRNAProfileDao: MRNAProfileDao,
                                  geneInformationDao : GeneInformationDao)extends Controller {


  def toIndex = Action {implicit request =>
    Ok(views.html.search.index())
  }

  val form = Form(
    mapping(
      "id" -> text,
      "sampleName" -> text
    )(checkPostionData.apply)(checkPostionData.unapply)
  )

  def checkPostion = Action.async { implicit request =>
    val data = form.bindFromRequest.get
    val id = data.id
    val sampleName = data.sampleName
    val idStr = data.id.split(",").map(_.trim).distinct
    val samStr = data.sampleName.split(",").map(_.trim).distinct
    geneIdDao.selectById(id).flatMap{x=>
      mRNAProfileDao.selectBySampleName(sampleName).map{ y=>
        val judge = x.size == idStr.length && y.size == samStr.length
        var valids = "false"
        if(judge == false){valids = "true"}
          val invalidGeneId = "Gene Symbol: " + idStr.diff(x).mkString(",")
          val invalidSampleName = "Sample Name:" + samStr.diff(y).mkString(",")
          val jsons = "The "+invalidGeneId + " | " + invalidSampleName + " not in database!"
          val json=Json.obj("valids"->valids,"messages"->jsons)
          Ok(json)
        }
      }
    }

//得到输入的基因ID
  def selectByPosition(id: String,sampleName:String) : Action[AnyContent]= Action.async { implicit request =>
    //获得以逗号分隔和除去空格的id
    val idStr = id.split(",").map(_.trim).distinct.mkString(",")
    geneInformationDao.selectById(idStr).map{x=>
      Ok(views.html.search.result(idStr, sampleName,x))
      }
    }

  def linear(id: String,sampleName:String): Action[AnyContent] = Action.async { implicit request =>
    //获得以逗号分隔和除去空格的id
    val genes = id.split(",").map(_.trim).distinct
    val sams = sampleName.split(",").map(_.trim).distinct
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

  def circBoxPlot(id:String , group1 : String , group2 : String): Action[AnyContent] = Action.async { implicit request =>
    mRNAProfileDao.selectValueByPosition(id,group1).flatMap{v1=>
      mRNAProfileDao.selectValueByPosition(id,group2).map { v2 =>
        val map = Map(("Group1",v1.toArray),("Group2",v2.toArray))
        val data = map.map{x=>
          val values = x._2
          val minValue = values.min
          val Q1 = StatUtils.percentile(values, 25)
          val Q2 = StatUtils.percentile(values, 50)
          val Q3 = StatUtils.percentile(values, 75)
          val maxValue = values.max
          Array(x._1, minValue.toString, Q1.toString, Q2.toString, Q3.toString, maxValue.toString)
        }
        val jsons = Json.obj("ev" -> data.map(_.drop(1).map(_.toDouble)), "tissue" -> data.map(_.head))
       Ok(Json.toJson(jsons))
      }
    }
  }

  //log10(x):10的多少次方等于x
  def log2(x: Double): Double = log10(x) / log10(2.0)

}