package controllers

import javax.inject.Inject

import dao.{CorrelationDao, GeneInformationDao, PasswordDao}
import play.api.data.Form
import play.api.data.Forms._
import play.api.mvc.{Action, AnyContent, Controller}

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
    Ok(views.html.Chinese.analyse.kegg())
  }

  def go = Action {
    Ok(views.html.Chinese.analyse.go())
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

  def downloadIndex = Action {
    Ok(views.html.Chinese.download.index())
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
      "start" -> number,
      "end" -> number,
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
      var total = ""
      val size = x.filter(_.correlation >= rvalue.toDouble).distinct.size
      if(size>0){
        total = size.toString
      }else{
        total = "没有结果!"
      }
      Ok(views.html.Chinese.tools.coResult(id,rvalue,total))
    }
  }

}
