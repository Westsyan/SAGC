package controllers

import java.io.File
import java.nio.file.Files

import org.apache.commons.io.FileUtils
import play.api.data.Form
import play.api.data.Forms._
import play.api.libs.json._
import play.api.mvc._
import utils._

case class seqData(chr: Int, start: Int, end: Int)

case class regData(region: String)

class ToolsController extends Controller {

  val path = "E:\\perl\\samtools-0.1.19\\samtools.exe faidx E:\\perl\\data.fa "

  val seqForm = Form(
    mapping(
      "chr" -> number,
      "start" -> number,
      "end" -> number
    )(seqData.apply)(seqData.unapply)
  )

  val regionForm = Form(
    mapping(
      "region" -> text
    )(regData.apply)(regData.unapply)
  )

  def seqIndex = Action {
    Ok(views.html.Tools.seqFetch())
  }

  def regionIndex = Action {
    Ok(views.html.Tools.multipleSeq())
  }

  def geneBlastn = Action {
    Ok(views.html.Tools.geneBlastn())
  }

  def genomeBlastn = Action {
    Ok(views.html.Tools.genomeBlastn())
  }

  def seqFetch = Action { implicit request =>
    val data = seqForm.bindFromRequest.get
    val tmpDir = Files.createTempDirectory("tmpDir").toString
    val outFile = new File(tmpDir, "data.txt")
    val execCommand = new ExecCommand
    val command = path + data.chr + ":" + data.start + "-" + data.end
    execCommand.exec(command, outFile)
    if (execCommand.isSuccess) {
      val dataStr = FileUtils.readFileToString(outFile)
      Ok(Json.toJson(dataStr))
    } else {
      Ok(Json.obj("valid" -> "false", "message" -> execCommand.getErrStr))
    }
  }

  def seqRegion = Action { implicit request =>
    val data = regionForm.bindFromRequest.get
    val tmpDir = Files.createTempDirectory("tmpDir").toString
    val outFile = new File(tmpDir, "data.txt")
    val execCommand = new ExecCommand
    val command = path + data.region
    execCommand.exec(command, outFile)
    if (execCommand.isSuccess) {
      val dataStr = FileUtils.readFileToString(outFile)
      Ok(Json.toJson(dataStr))
    } else {
      Ok(Json.obj("valid" -> "false", "message" -> execCommand.getErrStr))
    }
  }


}
