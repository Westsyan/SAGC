package controllers

import javax.inject.Inject

import dao.{GeneIdDao, GeneInformationDao, MRNAProfileDao, PasswordDao}
import play.api.data.Form
import play.api.data.Forms._
import play.api.libs.json.Json
import play.api.mvc.{Action, Controller}

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

class CheckController @Inject()(passwordDao: PasswordDao, geneIdDao: GeneIdDao, mRNAProfileDao: MRNAProfileDao, geneInformationDao: GeneInformationDao) extends Controller {

  def checkPCA(sampleName: String) = Action { implicit request =>
    if (sampleName.isEmpty) {
      Ok(Json.obj("valid" -> "true"))
    } else {
      val result = checkSample(sampleName)
      val message = "The Sample Name : " + result._2
      val json = Json.obj("valid" -> result._1, "message" -> message)
      Ok(Json.toJson(json))
    }
  }

  def checkSamplename(sampleName: String) = Action { implicit request =>
    val result = checkSample(sampleName)
    val message = "The Sample Name : " + result._2
    val json = Json.obj("valid" -> result._1, "message" -> message)
    Ok(Json.toJson(json))
  }

  case class geneIdData(id: String)

  val geneIdForm = Form(
    mapping(
      "id" -> text
    )(geneIdData.apply)(geneIdData.unapply)
  )

  def checkGeneId = Action { implicit request =>
    val data = geneIdForm.bindFromRequest.get
    val id = data.id
    val result = checkId(id)
    val message = "The Gene Symbol : " + result._2
    val json = Json.obj("valid" -> result._1, "message" -> message)
    Ok(Json.toJson(json))
  }

  def checkId(id: String) = {
    if (id.isEmpty) {
      ("false", "Please input a value!")
    } else {
      val geneId = id.split(",").map(_.trim).distinct
      val x = Await.result(geneIdDao.selectById(id), Duration.Inf)
      var valid = "true"
      var message = ""
      if (x.size != geneId.size) {
        valid = "false"
        val invalidGeneId = geneId.diff(x).mkString(",")
        message = invalidGeneId
      }
      (valid, message)
    }
  }

  def checkSample(sample: String) = {
    if (sample.isEmpty) {
      ("false", "Please input a value!")
    } else {
      val samplename = sample.split(",").map(_.trim).distinct
      val x = Await.result(mRNAProfileDao.selectBySampleName(sample), Duration.Inf)
      var valid = "true"
      var message = ""
      if (x.size != samplename.size) {
        valid = "false"
        val invalidSampleName = samplename.diff(x).mkString(",")
        message = invalidSampleName
      }
      (valid, message)
    }
  }

  val ttestForm = Form(
    mapping(
      "group1" -> text,
      "group2" -> text,
      "c" -> text,
      "pval" -> text
    )(checkGroup.apply)(checkGroup.unapply)
  )

  def checkTtest = Action { implicit request =>
    val data = ttestForm.bindFromRequest.get
    val gro1 = data.group1
    val gro2 = data.group2
    val pValue = data.pval.toDouble
    val r1 = checkSample(gro1)
    val r2 = checkSample(gro2)
    var valid = "true"
    var message = ""
    if (r1._1 == "false") {
      valid = r1._1
      message = "The Group1:" + r1._2
    } else if (r2._1 == "false") {
      valid = r2._1
      message = "The Gourp2:" + r2._2
    } else if (pValue < 0) {
      valid = "false"
      message = "The q-value  must be greater than 0"
    }
    val json = Json.obj("valids" -> valid, "messages" -> message)
    Ok(json)
  }

  val keggForm = Form(
    mapping(
      "id" -> text,
      "m" -> text,
      "n" -> text,
      "c" -> text,
      "pval" -> text
    )(checkKeggData.apply)(checkKeggData.unapply)
  )

  def checkKegg = Action { implicit request =>
    val data = keggForm.bindFromRequest.get
    val geneId = data.id
    val cutoff = data.c.toDouble
    val pval = data.pval.toDouble
    val m = data.m
    val n = data.n
    val result = checkId(geneId)
    var valids = result._1
    var message = "The Gene Symbol: " + result._2
    if (cutoff <= 0) {
      valids = "false"
      message = "The threshold must be  positive number"
    } else if (pval <= 0) {
      valids = "false"
      message = "The p-value must be  positive number"
    }
    val json = Json.obj("valids" -> valids, "messages" -> message)
    Ok(json)

  }

  val goForm = Form(
    mapping(
      "id" -> text,
      "alpha" -> text,
      "pval" -> text
    )(checkGoData.apply)(checkGoData.unapply)
  )

  def checkGo = Action { implicit request =>
    val data = goForm.bindFromRequest.get
    val geneId = data.id
    val pval = data.pval.toDouble
    val result = checkId(geneId)
    var valids = result._1
    var message = "The Gene Symbol:" + result._2
    if (pval <= 0) {
      valids = "false"
      message = "The p-value must be  positive number"
    }
    val json = Json.obj("valids" -> valids, "messages" -> message)
    Ok(json)
  }

  case class checkSearchData(id: String, sampleName: String)

  val searchForm = Form(
    mapping(
      "id" -> text,
      "sampleName" -> text
    )(checkSearchData.apply)(checkSearchData.unapply)
  )

  def checkSearchPostion = Action { implicit request =>
    val data = searchForm.bindFromRequest.get
    val id = data.id
    val sampleName = data.sampleName
    val gene = checkId(id)
    val sample = checkSample(sampleName)
    var valid = "true"
    var message = ""
    if (gene._1 == "false") {
      valid = gene._1
      message = "The Gene Symbol:" + gene._2
    } else if (sample._1 == "false") {
      valid = sample._1
      message = "The Sample Name: " + sample._2
    }
    val json = Json.obj("valids" -> valid, "messages" -> message)
    Ok(json)
  }

  val sampleRegionForm = Form(
    mapping(
      "chr" -> number,
      "start" -> number,
      "end" -> number,
      "sampleName" -> text
    )(SampleRegionData.apply)(SampleRegionData.unapply)
  )

  def checkSampleRegion = Action.async { implicit request =>
    val data = sampleRegionForm.bindFromRequest.get
    val sampleName = data.sampleName
    val start = data.start
    val end = data.end
    val result = checkSample(sampleName)
    geneInformationDao.selectBySRegion(data).map { idStr =>
      val id = idStr.mkString(",")
      var valid = "true"
      var message = ""
      if (start >= end) {
        valid = "false"
        message = "The start must be samll than the end!"
      } else if (idStr.size == 0) {
        valid = "false"
        message = "There is no data in the current range!"
      } else if (result._1 == "false") {
        valid = result._1
        message = "The Sample Name:" + result._2
      }
      val json = Json.obj("valids" -> valid, "messages" -> message)
      Ok(json)
    }

  }

  case class downData(id: String, sampleName: String)

  val downloadForm = Form(
    mapping(
      "id" -> text,
      "sampleName" -> text
    )(downData.apply)(downData.unapply)
  )

  def checkDownload = Action { implicit request =>
    val data = downloadForm.bindFromRequest.get
    val id = data.id
    val sampleName = data.sampleName
    if (id.isEmpty) {
      val result = checkSample(sampleName)
      val message = "The Sample Name: " + result._2
      val json = Json.obj("valids" -> result._1, "messages" -> message)
      Ok(json)
    } else {
      val gene = checkId(id)
      val sample = checkSample(sampleName)
      var valid = "true"
      var message = ""
      if(gene._1 == "false"){
        valid = gene._1
        message = "The Gene Symbol: " + gene._2
      }else if(sample._1 == "false"){
        valid = sample._1
        message = "The Sample Name: " + sample._2
      }
      val json = Json.obj("valids" -> valid, "messages" -> message)
      Ok(json)


    }
  }

}
