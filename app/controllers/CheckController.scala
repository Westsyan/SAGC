package controllers

import javax.inject.Inject

import dao.{GeneIdDao, GeneInformationDao, MRNAProfileDao, PasswordDao}
import play.api.data.Form
import play.api.data.Forms._
import play.api.libs.json.Json
import play.api.mvc.{Action, Controller}
import utils.Utils

import scala.concurrent.Await
import scala.concurrent.duration.Duration

class CheckController @Inject()(passwordDao: PasswordDao, geneIdDao: GeneIdDao, mRNAProfileDao: MRNAProfileDao, geneInformationDao: GeneInformationDao) extends Controller {

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
        message = invalidGeneId + " is not in Database!"
      }
      (valid, message)
    }
  }

  def checkChId(id: String) = {
    if (id.isEmpty) {
      ("false", "请至少输入一个基因!")
    } else {
      val geneId = id.split(",").map(_.trim).distinct
      val x = Await.result(geneIdDao.selectById(id), Duration.Inf)
      var valid = "true"
      var message = ""
      if (x.size != geneId.size) {
        valid = "false"
        val invalidGeneId = geneId.diff(x).mkString(",")
        message = invalidGeneId + " 不在数据库中!"
      }
      (valid, message)
    }
  }

  def checkChSample(sample: String) = {
    if (sample.isEmpty) {
      ("false", "请至少输入一个样品!")
    } else {
      val samplename = sample.split(",").map(_.trim).distinct
      val x = Await.result(mRNAProfileDao.selectBySampleName(sample), Duration.Inf)
      var valid = "true"
      var message = ""
      if (x.size != samplename.size) {
        valid = "false"
        val invalidSampleName = samplename.diff(x).mkString(",")
        message = invalidSampleName + " 不在数据库中!"
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
        message = invalidSampleName + " is not in Database!"
      }
      (valid, message)
    }
  }

  def checkPCA(sampleName: String) = Action { implicit request =>
    if (sampleName.isEmpty) {
      Ok(Json.obj("valid" -> "true"))
    } else {
      val refer = Utils.refer(request)
      var valid = "true"
      var message = ""
      if (refer.contains("chinese")) {
        val result = checkChSample(sampleName)
        valid = result._1
        message = "样品名 : " + result._2
      } else {
        val result = checkSample(sampleName)
        valid = result._1
        message = "The Sample Name : " + result._2
      }
      val json = Json.obj("valid" -> valid, "message" -> message)
      Ok(Json.toJson(json))
    }
  }

  case class sampleData(sampleName: String)

  val sampleForm = Form(
    mapping(
      "sampleName" -> text
    )(sampleData.apply)(sampleData.unapply)
  )

  def checkSamplename = Action { implicit request =>
    val data = sampleForm.bindFromRequest.get
    val sampleName = data.sampleName
    val refer = Utils.refer(request)
    var valid = "true"
    var message = ""
    if (refer.contains("chinese")) {
      val result = checkChSample(sampleName)
      valid = result._1
      message = "样品 : " + result._2
    } else {
      val result = checkSample(sampleName)
      valid = result._1
      message = "The Sample Name : " + result._2
    }
    val json = Json.obj("valid" -> valid, "message" -> message)
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
    val refer = Utils.refer(request)
    var valid = "true"
    var message = ""
    if (refer.contains("chinese")) {
      val result = checkChId(id)
      valid = result._1
      message = "基因ID: " + result._2
    } else {
      val result = checkId(id)
      valid = result._1
      message = "The Gene ID : " + result._2
    }
    val json = Json.obj("valid" -> valid, "message" -> message)
    Ok(Json.toJson(json))
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
    var valid = "true"
    var message = ""
    val g1 = gro1.split(",").map(_.trim).size
    val g2 = gro2.split(",").map(_.trim).size
    val refer = Utils.refer(request)
    if (refer.contains("chinese")) {
      val r1 = checkChSample(gro1)
      val r2 = checkChSample(gro2)
      if (r1._1 == "false") {
        valid = r1._1
        message = "样品组1: " + r1._2
      } else if (r2._1 == "false") {
        valid = r2._1
        message = "样品组2: " + r2._2
      }else if(g1<2 || g2 < 2){
        valid = "false"
        message = "样品数必须大于两个"
      }
    } else {
      val r1 = checkSample(gro1)
      val r2 = checkSample(gro2)
      if (r1._1 == "false") {
        valid = r1._1
        message = "The Group1:" + r1._2
      } else if (r2._1 == "false") {
        valid = r2._1
        message = "The Gourp2:" + r2._2
      }else if(g1<2 || g2 < 2){
        valid ="false"
        message = "The sample number must be greater than 2"
      }
    }
    val json = Json.obj("valid" -> valid, "message" -> message)
    Ok(Json.toJson(json))
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
    var valid = "true"
    var message = ""
    val refer = Utils.refer(request)
    if (refer.contains("chinese")) {
      val gene = checkChId(id)
      val sample = checkChSample(sampleName)
      if (gene._1 == "false") {
        valid = gene._1
        message = "基因ID: " + gene._2
      } else if (sample._1 == "false") {
        valid = sample._1
        message = "样品名: " + sample._2
      }
    } else {
      val gene = checkId(id)
      val sample = checkSample(sampleName)
      if (gene._1 == "false") {
        valid = gene._1
        message = "The Gene ID: " + gene._2
      } else if (sample._1 == "false") {
        valid = sample._1
        message = "The Sample Name: " + sample._2
      }
    }
    val json = Json.obj("valid" -> valid, "message" -> message)
    Ok(json)
  }

  val regionForm = Form(
    mapping(
      "chr" -> number,
      "start" -> longNumber,
      "end" -> longNumber,
      "sampleName" -> text
    )(SampleRegionData.apply)(SampleRegionData.unapply)
  )

  def checkRegion = Action { implicit request =>
    var message1 = ""
    var message2 = ""
    var valid = "true"
    val refer = Utils.refer(request)
    try {
      val data = regionForm.bindFromRequest.get
      val start = data.start
      val end = data.end
      val sample = data.sampleName
      if (end < start) {
        valid = "false"
        message1 = "起始坐标必须比终止坐标小!"
        message2 = "Start must be smaller than end!"
      } else if (start < 0 || end > 42689080) {
        valid = "false"
        message1 = "起始坐标和终止坐标必须大于0小于42689080!"
        message2 = "The start and end must be greater than 0 and less than 42689080!"
      } else if (refer.contains("chinese")) {
        val result = checkChSample(sample)
        valid = result._1
        message1 = "样品名: " + result._2
      } else {
        val result = checkSample(sample)
        valid = result._1
        message2 = "The Sample Name: " + result._2
      }
    }
    catch {
      case x: NoSuchElementException =>
        valid = "false"
        message1 = "错误,起始坐标和终止坐标需要输入整数！"
        message2 = "The start and end must be an integer!"
    }
    referResult(refer,valid,message1,message2)
  }

  case class coData(id:String,rvalue:String)

  val coForm = Form(
    mapping(
      "id" -> text,
      "rvalue" -> text
    )(coData.apply)(coData.unapply)
  )

  def checkCo = Action{ implicit request=>
    var valid = "true"
    var message1 = ""
    var message2 = ""
    val refer = Utils.refer(request)
    try{
      val data = coForm.bindFromRequest.get
      val geneId = data.id
      val rvalue = data.rvalue.toDouble
      if(rvalue > 1 || rvalue <0.9){
        valid = "false"
        message1 = "r-value必须为0.9到1的正数"
        message2 = "r-value should be a positive number of 0.9 to 1"
      }else if (refer.contains("chinese")) {
        val result = checkChId(geneId)
        valid = result._1
        message1 = "基因ID: " + result._2
      } else {
        val result = checkId(geneId)
        valid = result._1
        message2 = "The Gene ID: " + result._2
      }
    }catch{
      case x : NumberFormatException =>
        valid = "false"
        message1 = "r-value必须为0到1的正数"
        message2 = "r-value must be a positive number of 0 to 1"
    }
    referResult(refer,valid,message1,message2)
  }

  def referResult(refer:String,valid:String,message1:String,message2:String) ={
    if (refer.contains("chinese")) {
      val json = Json.obj("valid" -> valid, "message" -> message1)
      Ok(Json.toJson(json))
    } else {
      val json = Json.obj("valid" -> valid, "message" -> message2)
      Ok(Json.toJson(json))
    }
  }
}
