package controllers

import java.io.File
import java.nio.file.Files
import javax.inject.Inject

import dao.CorrelationDao
import org.apache.commons.io.FileUtils
import play.api.data.Form
import play.api.data.Forms._
import play.api.libs.json._
import play.api.mvc._
import utils._

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

case class seqData(chr: String, start: String, end: String)

case class regData(region: String)

case class blastData(method: String, query: String, evalue: String, wordSize: String, maxTargetSeqs: String)

class ToolsController @Inject()(correlationDao: CorrelationDao) extends Controller {

  val seqForm = Form(
    mapping(
      "chr" -> text,
      "start" -> text,
      "end" -> text
    )(seqData.apply)(seqData.unapply)
  )

  val regionForm = Form(
    mapping(
      "region" -> text
    )(regData.apply)(regData.unapply)
  )

  val blastForm = Form(
    mapping(
      "method" -> text,
      "query" -> text,
      "evalue" -> text,
      "wordSize" -> text,
      "maxTargetSeqs" -> text
    )(blastData.apply)(blastData.unapply)
  )

  def seqIndex = Action {
    Ok(views.html.English.tools.seqFetch())
  }

  def regionIndex = Action {
    Ok(views.html.English.tools.multipleSeq())
  }

  def geneBlastn = Action {
    Ok(views.html.English.tools.geneBlastn())
  }

  def genomeBlastn = Action {
    Ok(views.html.English.tools.genomeBlastn())
  }

  def seqFetch = Action { implicit request =>
    val data = seqForm.bindFromRequest.get
    val tmpDir = Files.createTempDirectory("tmpDir").toString
    val outFile = new File(tmpDir, "data.txt")
    val execCommand = new ExecCommand
   //  val command = Utils.path + "samtools-0.1.19/samtools.exe faidx "+  Utils.path +"data.fa "  + data.chr + ":" + data.start + "-" + data.end
    val command = "samtools faidx " + Utils.path + "data.fa " + data.chr + ":" + data.start + "-" + data.end
    execCommand.exec(command, outFile)
    if (execCommand.isSuccess) {
      val dataStr = FileUtils.readFileToString(outFile)
      Utils.deleteDirectory(tmpDir)
      Ok(Json.toJson(dataStr))
    } else {
      Utils.deleteDirectory(tmpDir)
      Ok(Json.obj("valid" -> "false", "message" -> execCommand.getErrStr))
    }
  }

  def seqRegion = Action { implicit request =>
    val data = regionForm.bindFromRequest.get
    val tmpDir = Files.createTempDirectory("tmpDir").toString
    val outFile = new File(tmpDir, "data.txt")
    val execCommand = new ExecCommand
    //val command =Utils.path + "samtools-0.1.19/samtools.exe faidx "+ Utils.path +"data.fa " + data.region
    val command = "samtools faidx " + Utils.path + "data.fa " + data.region
    execCommand.exec(command, outFile)
    if (execCommand.isSuccess) {
      val dataStr = FileUtils.readFileToString(outFile)
      Utils.deleteDirectory(tmpDir)
      Ok(Json.toJson(dataStr))
    } else {
      Utils.deleteDirectory(tmpDir)
      Ok(Json.obj("valid" -> "false", "message" -> execCommand.getErrStr))
    }
  }

  def geneBlastnRun = Action(parse.multipartFormData) { implicit request =>
    val data = blastForm.bindFromRequest.get
    val tmpDir = Files.createTempDirectory("tmpDir").toString
    val seqFile = new File(tmpDir, "seq.fa")
    data.method match {
      case "text" =>
        FileUtils.writeStringToFile(seqFile, data.query)
      case "file" =>
        val file = request.body.file("file").get
        file.ref.moveTo(seqFile, replace = true)
    }
    val outXml = new File(tmpDir, "out.xml")
    val outHtml = new File(tmpDir, "out.html")
    val execCommand = new ExecCommand
    val command1 = Utils.path + "ncbi-blast-2.6.0+/bin/blastn -query " + seqFile.getAbsolutePath + " -db " +
      Utils.path + "cds.fa " + "-outfmt 5 -evalue " + data.evalue + " -max_target_seqs " + data.maxTargetSeqs +
      " -word_size " + data.wordSize + " -out " + outXml.getAbsolutePath
    val command2 = "python " + Utils.path + "blast2html-82b8c9722996/blast2html.py -i " + outXml.getAbsolutePath + " -o " + outHtml.getAbsolutePath
    execCommand.exec(command1, command2)
    if (execCommand.isSuccess) {
      val html = FileUtils.readFileToString(outHtml)
      Utils.deleteDirectory(tmpDir)
      Ok(Json.obj("html" -> (html + "\n" + Utils.scriptHtml)))
    } else {
      Utils.deleteDirectory(tmpDir)
      Ok(Json.obj("valid" -> "false", "message" -> execCommand.getErrStr))
    }
  }

  def genomeBlastnRun = Action(parse.multipartFormData) { implicit request =>
    val data = blastForm.bindFromRequest.get
    val tmpDir = Files.createTempDirectory("tmpDir").toString
    val seqFile = new File(tmpDir, "seq.fa")
    data.method match {
      case "text" =>
        FileUtils.writeStringToFile(seqFile, data.query)
      case "file" =>
        val file = request.body.file("file").get
        file.ref.moveTo(seqFile, replace = true)
    }
    val outXml = new File(tmpDir, "out.xml")
    val outHtml = new File(tmpDir, "out.html")
    val execCommand = new ExecCommand
    val command1 = Utils.path + "ncbi-blast-2.6.0+/bin/blastn -query " + seqFile.getAbsolutePath + " -db " +
      Utils.path + "data.fa " + "-outfmt 5 -evalue " + data.evalue + " -max_target_seqs " + data.maxTargetSeqs +
      " -word_size " + data.wordSize + " -out " + outXml.getAbsolutePath
    val command2 = "python " + Utils.path + "blast2html-82b8c9722996/blast2html.py -i " + outXml.getAbsolutePath + " -o " + outHtml.getAbsolutePath
    execCommand.exec(command1, command2)
    if (execCommand.isSuccess) {
      val html = FileUtils.readFileToString(outHtml)
      Utils.deleteDirectory(tmpDir)
      Ok(Json.obj("html" -> (html + "\n" + Utils.scriptHtml)))
    } else {
      Utils.deleteDirectory(tmpDir)
      Ok(Json.obj("valid" -> "false", "message" -> execCommand.getErrStr))
    }
  }

  def coIndex = Action {
    Ok(views.html.English.tools.coIndex())
  }

  def coResult(id: String, rvalue: String): Action[AnyContent] = Action.async { implicit request =>
    correlationDao.selectByGeneid(id).map { x =>
      val size = x.filter(_.correlation >= rvalue.toDouble).distinct.size
      val total = size match {
        case i if i > 0 => size.toString
        case _ => "no matched results!"
      }
      Ok(views.html.English.tools.coResult(id, rvalue, total))
    }
  }

  def correlationInfo(id: String, rvalue: String): Action[AnyContent] = Action.async { implicit request =>
    val refer = Utils.refer(request)
    correlationDao.selectByGeneid(id).map { x =>
      val total = x.filter(_.correlation >= rvalue.toDouble).distinct
      val json = total.map { y =>
        val gene1 = refer match {
          case i if i.contains("chinese") => "<a target='_blank' href='" + routes.ChineseController.getMoreInfo(y.gene1) + "'>" + y.gene1 + "</a>"
          case _ => "<a target='_blank' href='" + routes.GeneInformationController.getMoreInfo(y.gene1) + "'>" + y.gene1 + "</a>"
        }
        val gene2 = refer match {
          case i if i.contains("chinese") => "<a target='_blank' href='" + routes.ChineseController.getMoreInfo(y.gene2) + "'>" + y.gene2 + "</a>"
          case _ => "<a target='_blank' href='" + routes.GeneInformationController.getMoreInfo(y.gene2) + "'>" + y.gene2 + "</a>"
        }
        Json.obj("gene1" -> gene1, "gene2" -> gene2, "correlation" -> y.correlation)
      }
      Ok(Json.toJson(json))
    }
  }

  def networkResult(id: String, rvalue: String): Action[AnyContent] = Action { implicit request =>
    val geneid = id.split(",").map(_.trim).distinct
    val x = Await.result(correlationDao.selectByGeneid(id), Duration.Inf)
    val result = x.filter(_.correlation >= rvalue.toDouble)
    val gene2 = result.map(_.gene2).distinct
    val expressByGene2 = Await.result(correlationDao.selectByGeneid(gene2.mkString(",")), Duration.Inf)
    val r = expressByGene2.filter(_.correlation >= rvalue.toDouble)
    val r1 = r.filter(x => geneid.contains(x.gene2))
    val r2 = r.diff(r1)
    val r3 = r2.map { x => (x.gene1, x.gene2) }
    var r4 = r2
    //去除重复的相关性系数
    for (i <- 0 until r3.size; j <- i + 1 until r3.size) {
      val x1 = r3(i)
      val x2 = r3(j)
      if (x1._1 == x2._2 && x2._1 == x1._2) {
        r4 = r4.diff(r2.filter(_.gene2 == x2._1))
      }
    }
    val all = result ++ r4
    val edges = all.map { z =>
      val value = "Value:" + z.correlation
      Json.obj("from" -> z.gene1, "title" -> value, "to" -> z.gene2)
    }
    val gene = gene2 ++ geneid
    val nodes = gene.distinct.map { z =>
      if (id.contains(z)) {
        Json.obj("id" -> z, "label" -> z, "group" -> "geneGroup", "title" -> z)
      } else {
        Json.obj("id" -> z, "title" -> z)
      }
    }
    val json = Json.obj("edges" -> edges, "nodes" -> nodes)
    Ok(Json.toJson(json))

  }

}
