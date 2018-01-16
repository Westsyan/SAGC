package controllers

import java.io.File
import javax.inject.Inject

import dao.{CorrelationDao, GeneInformationDao, PasswordDao}
import org.apache.commons.io.FileUtils
import play.api.data.Form
import play.api.data.Forms._
import play.api.mvc.{Action, AnyContent, Controller}
import utils.Utils
import scala.collection.JavaConverters._

import scala.concurrent.ExecutionContext.Implicits.global

class ChineseController@Inject()(passwordDao: PasswordDao,geneInformationDao: GeneInformationDao,correlationDao: CorrelationDao) extends Controller{

  def backgroundIndex = Action {
    Ok(views.html.Chinese.admin.index())
  }

  def loginBefore = Action { implicit request =>
    Ok(views.html.Chinese.admin.login())
  }

  def logout = Action {
    Redirect(routes.ChineseController.loginBefore()).flashing("info" -> "您已经成功退出登录!").withNewSession
  }

  def login(phone: String, password: String) = Action.async { implicit request =>
    passwordDao.selectPassword.map { x =>
      if (phone == "admin" && password == x) {
        Redirect(routes.ChineseController.backgroundIndex()).withSession("phone" -> phone)
      } else {
        Redirect(routes.ChineseController.loginBefore()).flashing("info" -> "账号或者密码错误!")
      }
    }
  }


  def changePasswordBefore() = Action { implicit request =>
    Ok(views.html.Chinese.admin.changePassword())
  }

  def addmRNABefore = Action { implicit request =>
    Ok(views.html.Chinese.admin.addmRNA())
  }

  def deletemRNABefore = Action {
    Ok(views.html.Chinese.admin.deletemRNA())
  }

  def updateCorrelation = Action{
    Ok(views.html.Chinese.admin.updatec())
  }

  def tTest = Action {
    Ok(views.html.Chinese.analyse.ttest())
  }

  def cHeatmap = Action {
    Ok(views.html.Chinese.analyse.correlation())
  }

  def kegg = Action {
    Ok(views.html.Chinese.analyse.kegg("NA"))
  }

  def go = Action {
    Ok(views.html.Chinese.analyse.go("NA"))
  }

  def toGo = Action {
    val buffer = FileUtils.readLines(new File(Utils.path, "getId.txt")).asScala
    Ok(views.html.Chinese.analyse.go(buffer(0)))
  }

  def toKegg = Action {
    val buffer = FileUtils.readLines(new File(Utils.path, "getId.txt")).asScala
    Ok(views.html.Chinese.analyse.kegg(buffer(0)))
  }

  def clusterIndex = Action {
    Ok(views.html.Chinese.analyse.cluster())
  }

  def scatterIndex = Action { implicit request =>
    Ok(views.html.Chinese.analyse.scatterplot())
  }

  def PCAIndex = Action { implicit request =>
    Ok(views.html.Chinese.analyse.PCA())
  }

  def browse = Action {
    Ok(views.html.Chinese.download.index())
  }

  def downloadIndex = Action{
    Ok(views.html.Chinese.download.download())
  }

  def searchByRegion = Action {
    Ok(views.html.Chinese.search.region())
  }

  def searchBySRegion = Action { implicit request =>
    Ok(views.html.Chinese.search.searchBySampleRegion())
  }

  val sampleRegionForm = Form(
    mapping(
      "chr" -> number,
      "start" -> longNumber,
      "end" -> longNumber,
      "sampleName" -> text
    )(SampleRegionData.apply)(SampleRegionData.unapply)
  )

  def getSBySRegion = Action.async { implicit request =>
    val data = sampleRegionForm.bindFromRequest.get
    val sampleName = data.sampleName
    geneInformationDao.selectBySRegion(data).flatMap { geneId =>
      val idStr = geneId.mkString(",")
      geneInformationDao.selectById(idStr).map { x =>
        Ok(views.html.Chinese.search.result(idStr, sampleName, x))
      }
    }
  }

  def mainIndex = Action {
    Ok(views.html.Chinese.index())
  }

  def searchByPostion = Action {implicit request =>
    Ok(views.html.Chinese.search.index())
  }

  //得到输入的基因ID
  def selectByPosition(id: String,sampleName:String) : Action[AnyContent]= Action.async { implicit request =>
    //获得以逗号分隔和除去空格的id
    val idStr = id.split(",").map(_.trim).distinct.mkString(",")
    geneInformationDao.selectById(idStr).map{x=>
      Ok(views.html.Chinese.search.result(idStr, sampleName,x))
    }
  }

  def seqIndex = Action {
    Ok(views.html.Chinese.tools.seqFetch())
  }

  def regionIndex = Action {
    Ok(views.html.Chinese.tools.multipleSeq())
  }

  def geneBlastn = Action {
    Ok(views.html.Chinese.tools.geneBlastn())
  }

  def genomeBlastn = Action {
    Ok(views.html.Chinese.tools.genomeBlastn())
  }

  def coIndex = Action {
    Ok(views.html.Chinese.tools.coIndex())
  }

  def coResult(id:String,rvalue:String):Action[AnyContent] = Action.async { implicit request=>
    correlationDao.selectByGeneid(id).map{x=>
      val size = x.filter(_.correlation >= rvalue.toDouble).distinct.size
      val total = size match {
        case i if i >0 => size.toString
        case _ => "没有结果！"
      }
      Ok(views.html.Chinese.tools.coResult(id,rvalue,total))
    }
  }

  def  getMoreInfo(id: String): Action[AnyContent] = Action.async { implicit request =>
    val long = geneInformationDao.selectById(id)
    long.map { x =>
      val trueLong = x.head
      Ok(views.html.Chinese.search.moreInfo(trueLong))
    }
  }

  def moreInfoBoxPlot(id: String, group1: String, group2: String): Action[AnyContent] = Action.async { implicit request =>
    val long = geneInformationDao.selectById(id)
    long.map { x =>
      val trueLong = x.head
      Ok(views.html.Chinese.analyse.moreInfo(trueLong, group1, group2))
    }
  }


}
