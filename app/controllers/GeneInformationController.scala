package controllers

import javax.inject.Inject

import dao.{GeneIdDao, GeneInformationDao, MRNAProfileDao}
import models.Tables._
import play.api.data.Form
import play.api.data.Forms._
import play.api.libs.json.Json
import play.api.mvc._

import scala.concurrent.ExecutionContext.Implicits.global

case class RegionData(chr: Int, start: Int, end: Int)

case class SampleRegionData(chr: Int, start: Int, end: Int, sampleName: String)

class GeneInformationController @Inject()(geneIdDao: GeneIdDao, mRNAProfileDao: MRNAProfileDao, geneInformationDao: GeneInformationDao) extends Controller {

  val regionForm = Form(
    mapping(
      "chr" -> number,
      "start" -> number,
      "end" -> number
    )(RegionData.apply)(RegionData.unapply)
  )

  val sampleRegionForm = Form(
    mapping(
      "chr" -> number,
      "start" -> number,
      "end" -> number,
      "sampleName" -> text
    )(SampleRegionData.apply)(SampleRegionData.unapply)
  )

  def regionIndex = Action {
    Ok(views.html.English.search.region())
  }

  def searchBySRegion = Action { implicit request =>
    Ok(views.html.English.search.searchBySampleRegion())
  }

  //处理cds,pep,cdna序列的方法
  def method(map: Array[(String, String)], id: String) = {
    val s1 = map.filter(_._1 == id)
    val s2 = s1.map(_._2).distinct
    val s3 = s1.map(_._2.size).max
    val s4 = s1.filter(_._2.size == s3).distinct.map(_._2).mkString
    Json.toJson(s4)
  }

  def getLongArray(y: GeneinformationRow) = {
    val genenameStr = "<a target='_blank' href='" + routes.GeneInformationController.getMoreInfo(y.geneId) + "'>" + y.geneId + "</a>"
    val cdna = ">" + y.geneId + "\n" + y.cdna
    val cds = ">" + y.geneId + "\n" + y.cds
    val pep = ">" + y.geneId + "\n" + y.pep
    Json.obj("geneId" -> genenameStr, "genename" -> y.geneName, "Chromosome" -> y.chromosome, "Gene_start" -> y.geneStart,
      "Gene_end" -> y.geneEnd, "Strand" -> y.strand, "FUNC" -> y.func, "GO" -> y.go, "KEGG" -> y.kegg, "IPR_ID" -> y.iprId,
      "IPR_Descrip" -> y.iprDescrip, "cDNA" -> cdna, "CDS" -> cds, "PEP" -> pep)
  }

  def selectByGeneId(id: String): Action[AnyContent] = Action.async { implicit request =>
    geneInformationDao.selectById(id).map { x =>
      val array = x.map { y =>
        getLongArray(y)
      }
      Ok(Json.toJson(array))
    }
  }

  def getMoreInfo(id: String): Action[AnyContent] = Action.async { implicit request =>
    val long = geneInformationDao.selectById(id)
    long.map { x =>
      val trueLong = x.head
      Ok(views.html.English.search.moreInfo(trueLong))
    }
  }

  def moreInfoBoxPlot(id: String, group1: String, group2: String): Action[AnyContent] = Action.async { implicit request =>
    val long = geneInformationDao.selectById(id)
    long.map { x =>
      val trueLong = x.head
      Ok(views.html.English.analyse.moreInfo(trueLong, group1, group2))
    }
  }

  def searchByRegion = Action.async { implicit request =>
    val data = regionForm.bindFromRequest.get
    geneInformationDao.selectByRegion(data).map { x =>
      val json = x.map { y =>
        getLongArray(y)
      }
      Ok(Json.toJson(json))
    }
  }

  def getSBySRegion = Action.async { implicit request =>
    val data = sampleRegionForm.bindFromRequest.get
    val sampleName = data.sampleName
    geneInformationDao.selectBySRegion(data).flatMap { geneId =>
      val idStr = geneId.mkString(",")
      geneInformationDao.selectById(idStr).map { x =>
        Ok(views.html.English.search.result(idStr, sampleName, x))
      }
    }
  }

  def getAllChr = Action.async { implicit request =>
    geneInformationDao.allChr.map { x =>
      Ok(Json.toJson(x.distinct))
    }
  }
}