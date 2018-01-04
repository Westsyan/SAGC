package controllers

import javax.inject.Inject

import dao.{GeneIdDao, GeneInformationDao, MRNAProfileDao, PasswordDao}
import play.api.data.Form
import play.api.data.Forms._
import play.api.libs.json.Json
import play.api.mvc.{Action, Controller}

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
      val header = request.headers.toMap
      val refer = header.filter(_._1 == "Referer").map(_._2).head.head
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
    val header = request.headers.toMap
    val refer = header.filter(_._1 == "Referer").map(_._2).head.head
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
    val header = request.headers.toMap
    val refer = header.filter(_._1 == "Referer").map(_._2).head.head
    var valid = "true"
    var message = ""
    if (refer.contains("chinese")) {
      val result = checkChId(id)
      valid = result._1
      message = "基因名: " + result._2
    } else {
      val result = checkId(id)
      valid = result._1
      message = "The Gene Symbol : " + result._2
    }
    println(valid)
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
    val header = request.headers.toMap
    val refer = header.filter(_._1 == "Referer").map(_._2).head.head
    if (refer.contains("chinese")) {
      val r1 = checkChSample(gro1)
      val r2 = checkChSample(gro2)
      if (r1._1 == "false") {
        valid = r1._1
        message = "样品组1: " + r1._2
      } else if (r2._1 == "false") {
        valid = r2._1
        message = "样品组2: " + r2._2
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
    val header = request.headers.toMap
    val refer = header.filter(_._1 == "Referer").map(_._2).head.head
    if (refer.contains("chinese")) {
      val gene = checkChId(id)
      val sample = checkChSample(sampleName)
      if (gene._1 == "false") {
        valid = gene._1
        message = "基因名: " + gene._2
      } else if (sample._1 == "false") {
        valid = sample._1
        message = "样品名: " + sample._2
      }
    } else {
      val gene = checkId(id)
      val sample = checkSample(sampleName)
      if (gene._1 == "false") {
        valid = gene._1
        message = "The Gene Symbol:" + gene._2
      } else if (sample._1 == "false") {
        valid = sample._1
        message = "The Sample Name: " + sample._2
      }
    }
    val json = Json.obj("valid" -> valid, "message" -> message)
    Ok(json)
  }

  case class coData(id: String, rvalue: String)

  val coForm = Form(
    mapping(
      "id" -> text,
      "rvalue" -> text
    )(coData.apply)(coData.unapply)
  )

  def checkCo = Action{ implicit request=>
    val data = coForm.bindFromRequest.get
    val id = data.id
    val rvalue = data.rvalue
    var valid = "true"
    var message = ""
    val header = request.headers.toMap
    val refer = header.filter(_._1 == "Referer").map(_._2).head.head
    if (refer.contains("chinese")) {
      val result = checkChId(id)
      if (result._1 == "false") {
        valid = result._1
        message = "基因名: " + result._2
      }else if(rvalue.toDouble > 1 || rvalue.toDouble <0){
        valid = "false"
        message = "r-value 必须在0到1之间"
      }
    } else {
      val result = checkId(id)
      if (result._1 == "false") {
        valid = result._1
        message = "Gene Symbol: " + result._2
      }else if(rvalue.toDouble > 1 || rvalue.toDouble <0){
        valid = "false"
        message = "The r-value must be between 0 and 1！"
      }
    }
    val json = Json.obj("valid" -> valid, "message" -> message)
    Ok(Json.toJson(json))
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
      if (gene._1 == "false") {
        valid = gene._1
        message = "The Gene Symbol: " + gene._2
      } else if (sample._1 == "false") {
        valid = sample._1
        message = "The Sample Name: " + sample._2
      }
      val json = Json.obj("valids" -> valid, "messages" -> message)
      Ok(json)


    }
  }

}
