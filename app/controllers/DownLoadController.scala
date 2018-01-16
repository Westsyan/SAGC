package controllers

import java.io.File
import java.text.SimpleDateFormat
import java.util.Date
import javax.inject.Inject

import dao.{GeneIdDao, GeneInformationDao, MRNAProfileDao, PasswordDao}
import models.Tables._
import org.apache.commons.io.FileUtils
import org.apache.commons.math3.stat.descriptive.moment.StandardDeviation
import play.api.libs.json.Json
import play.api.mvc.{Action, AnyContent, Controller}
import utils.Utils

import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext.Implicits.global

class DownLoadController @Inject()(passwordDao: PasswordDao, geneIdDao: GeneIdDao, mRNAProfileDao: MRNAProfileDao, geneInformationDao: GeneInformationDao) extends Controller {

  def toIndex = Action {
    Ok(views.html.English.download.index())
  }

  def downloadIndex = Action{
    Ok(views.html.English.download.download())
  }

  def getName(sampleName: String): Action[AnyContent] = Action { implicit request =>
    val sam = sampleName.split(",").distinct.toSeq
    Ok(Json.toJson(sam))
  }

  def selectAllgene(id: String, sampleName: String): Action[AnyContent] = Action.async { implicit request =>
    val refer = Utils.refer(request)
    if (id.isEmpty) {
      mRNAProfileDao.selectAllBySampleName(sampleName).map { info =>
        val array = getArrayByGenotypes(info, refer)
        Ok(Json.obj("array" -> array))
      }
    } else {
      mRNAProfileDao.selectByPosition(id, sampleName) map { info =>
        val array = getArrayByGenotypes(info, refer)
        Ok(Json.obj("array" -> array))
      }
    }
  }

  def getArrayByGenotypes(x: Seq[MrnaprofileRow], refer: String) = {
    x.groupBy(_.geneid).map {
      case (geneid, sample) =>
        val genenameStr = if (refer.contains("chinese")) {
          "<a target='_blank' href='" + routes.ChineseController.getMoreInfo(geneid) + "'>" + geneid + "</a>"
        } else {
          "<a target='_blank' href='" + routes.GeneInformationController.getMoreInfo(geneid) + "'>" + geneid + "</a>"
        }
        val map1 = Map("geneId" -> genenameStr)
        val map2 = sample.map(y => y.samplename -> y.value.toString).toMap
        val map = map1 ++ map2
        map
    }
  }

  def download(fileName: String) = Action { implicit request =>
    val file = new File(Utils.path, fileName)
    Ok.sendFile(file).withHeaders(
      CACHE_CONTROL -> "max-age=3600",
      CONTENT_DISPOSITION -> ("attachment; filename=" + file.getName),
      CONTENT_TYPE -> "application/x-download"
    )
  }


  def tIndex = Action {
    Ok(views.html.English.download.test())
  }

  def Test = Action {
    getTime
    val buffer = FileUtils.readLines(new File(Utils.path, "time.txt")).asScala
    val b = buffer.head
    Ok(Json.toJson(b))
  }

  def getTime = {
    val now: Date = new Date()
    val dateFormat: SimpleDateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
    val date = dateFormat.format(now)
    FileUtils.writeStringToFile(new File(Utils.path, "time.txt"), date)
  }
}
