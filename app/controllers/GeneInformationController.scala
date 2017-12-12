package controllers

import java.io.File
import javax.inject.Inject

import dao.{GeneIdDao, GeneInformationDao}
import models.Tables._
import org.apache.commons.io.FileUtils
import play.api.libs.json.Json
import play.api.mvc._
import play.api.data.Form
import play.api.data.Forms._
import utils.Utils

import scala.collection.JavaConverters._
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

case class RegionData(chr:Int,start:Int,end:Int)

class GeneInformationController @Inject()(geneIdDao : GeneIdDao, geneInformationDao:GeneInformationDao) extends Controller {

  val regionForm = Form(
    mapping(
      "chr" -> number,
      "start" -> number,
      "end" -> number
    )(RegionData.apply)(RegionData.unapply)
  )

  def save = Action { implicit request =>
    val funcBuffer = FileUtils.readLines(new File("D:\\file\\ref_gene.func.txt")).asScala.distinct
    val keggBuffer = FileUtils.readLines(new File("D:\\file\\ref_gene.pathway.txt")).asScala.distinct
    val goBuffer = FileUtils.readLines(new File("D:\\file\\ref_gene.Go.txt")).asScala.distinct
    val genomeBuffer = FileUtils.readLines(new File("D:\\file\\ref_genome.txt")).asScala.distinct
    val cdsBuffer = FileUtils.readLines(new File("D:\\file\\cds.txt")).asScala
    val cdnaBuffer = FileUtils.readLines(new File("D:\\file\\cdna.txt")).asScala
    val pepBuffer = FileUtils.readLines(new File("D:\\file\\pep.txt")).asScala
    val annoBuffer = FileUtils.readLines(new File("D:\\file\\rice_anno01.txt")).asScala.distinct

    val cdsBuf = cdsBuffer.toString.split(">")
    val cdnaBuf = cdnaBuffer.toString.split(">")
    val pepBuf = pepBuffer.toString.split(">")
    val annoBuf = annoBuffer.map(_.split("\t"))

    val allGeneIds = Await.result(geneIdDao.selectAll, Duration.Inf)

    val funcToBuffer = funcBuffer.map(_.split("\t"))
    val geneIds = funcToBuffer.map(x => x(0)).distinct
    val fMap = funcToBuffer.map(x => (x(0), x(1)))

    val keegToBuffer = keggBuffer.map(_.split("\t"))
    val kMap = keegToBuffer.map(x => (x(0), x(1)))
    val keggId = keegToBuffer.map(x => x(0))

    val goToBuffer = goBuffer.map(_.split("\t"))
    val gMap = goToBuffer.map(x => (x(0), x(1)))
    val goId = goToBuffer.map(x => x(0))

    val toBuffer01 = genomeBuffer.filter(_.split("\t")(2) == "mRNA")
    val toBuffer02 = genomeBuffer.filter(_.split("\t")(2) == "transcript")
    val toBuffer = toBuffer01 ++ toBuffer02
    val genomeMap =toBuffer.map{ x=>
      val bu = x.split("\t")
      (bu(0),bu(3),bu(4),bu(8).split(";")(1).split("=")(1),bu(6))
    }
    val genomeId = genomeMap.map(_._4)

    val annoMap =  annoBuf.map{x=>
      (x(0),x(1),x(2),x(3))
    }
    val annoId = annoMap.map(_._1)

    val cdnaMap = cdnaBuf.map { x =>
      val geneId = x.split(",")(0).split("gene").slice(1,2).mkString.split(":").slice(1,2).mkString.trim
      val cds = x.split(",").drop(1).map(_.trim).mkString
      (geneId,cds)
    }
    val cdnaId = cdnaMap.map(_._1)

    val pepMap = pepBuf.map { x =>
      val geneId = x.split(",")(0).split("gene:").slice(1,2).mkString.split(" ")(0)
      val cds = x.split(",").drop(1).map(_.trim).mkString
      (geneId,cds)
    }
    val pepId = pepMap.map(_._1)

    val cdsMap = cdsBuf.map { x =>
      val geneId = x.split(",")(0).split("gene").slice(1,2).mkString.split(":").slice(1,2).mkString.trim
      val cds = x.split(",").drop(1).map(_.trim).mkString
      (geneId,cds)
    }
    val cdsId = cdsMap.map(_._1).distinct

    val startTime = System.currentTimeMillis()
    println("--start--")
    //开始时间
    println(Utils.getTime(startTime))
    geneInformationDao.deleteAll
    println(Utils.getTime(startTime))
    val geId = allGeneIds.map(_.id)
/*//插入id
  geneIdDao.deleteAll
    val rows = cdsId.map{x=>
      GeneidRow(x)
    }
    Await.result(geneIdDao.insertAll(rows), Duration.Inf)*/


    val rows = allGeneIds.map { x =>
      val allGene = x.id
      var func = ""
      if (geneIds.contains(allGene)) {
        val func1 = fMap.filter(_._1 == allGene).map(_._2).mkString("//")
        func = func1
      } else {
        func = "NA"
      }
      val keeg1 = kMap.filter(_._1 == allGene).map(_._2).mkString
      var kegg = ""
      if (keggId.contains(allGene)) {
        kegg = keeg1
      } else {
        kegg = "NA"
      }
      val go1 = gMap.filter(_._1 == allGene).map(_._2).mkString
      var go = ""
      if (goId.contains(allGene)) {
        go = go1
      } else {
        go = "NA"
      }
      var Chr = ""
      var start = ""
      var end = ""
      var dir = ""
      if(genomeId.contains(allGene)) {
        Chr = genomeMap.filter(_._4 == allGene).map(_._1).distinct.head
        val max = genomeMap.filter(_._4 == allGene).map { y => (y._2, y._3, y._3.toInt - y._2.toInt) }
        start = max.filter(_._3 == max.map(_._3).max).map(_._1).head
        end = max.filter(_._3 == max.map(_._3).max).map(_._2).head
        dir = genomeMap.filter(_._4 == allGene).map(_._5).distinct.head
      }else{
        Chr = "NA"
        start = "NA"
        end = "NA"
        dir = "NA"
      }
      var genename = ""
      var interproId =""
      var interproDescription = ""
      if(annoId.contains(allGene)){
        genename = annoMap.filter(_._1 == allGene).map(_._2).distinct.mkString

        interproId = annoMap.filter(_._1 == allGene).map(_._3).distinct.mkString(",")
        interproDescription = annoMap.filter(_._1 == allGene).map(_._4).distinct.mkString(",")
      }else{
        genename = "NA"
        interproId = "NA"
        interproDescription = "NA"
      }
      var pep = ""
      if(pepId.contains(allGene)){
        val s1 = pepMap.filter(_._1 == allGene)
        val s2 = s1.map(_._2).distinct
        val s3 = s1.map(_._2.size).max
        pep = s1.filter(_._2.size == s3).distinct.map(_._2).mkString
      }else{
        pep = "NA"
      }
      var cds = ""
      if(cdsId.contains(allGene)){
        val s1 = cdsMap.filter(_._1 == allGene)
        val s2 = s1.map(_._2).distinct
        val s3 = s1.map(_._2.size).max
        cds = s1.filter(_._2.size == s3).distinct.map(_._2).mkString
      }else{
        cds = "NA"
      }
      var cdna = ""
      if(cdnaId.contains(allGene)){
        val s1 = cdnaMap.filter(_._1 == allGene)
        val s2 = s1.map(_._2).distinct
        val s3 = s1.map(_._2.size).max
        cdna = s1.filter(_._2.size == s3).distinct.map(_._2).mkString
      }else{
        cdna = "NA"
      }
      GeneinformationRow(allGene,genename,Chr.toInt,start.toInt,end.toInt,dir,func,go,kegg,interproId,interproDescription,cdna,cds,pep)
      }
    println("总长度：" + rows.size)
    println(Utils.getTime(startTime))

    rows.grouped(10000).foreach { x =>
      println(Utils.getTime(startTime))
      println(x(1))
      Await.result(geneInformationDao.insert(x), Duration.Inf)
    }
    println("插入结束")
    Ok(views.html.save())
  }

  def regionIndex = Action{
    Ok(views.html.search.region())
  }

  //处理cds,pep,cdna序列的方法
  def method(map : Array[(String,String)],id:String)={
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
    Json.obj("geneId" ->genenameStr, "genename" -> y.geneName, "Chromosome" -> y.chromosome, "Gene_start" -> y.geneStart,
      "Gene_end" -> y.geneEnd, "Strand" -> y.strand,"FUNC" -> y.func,"GO" -> y.go,"KEGG" -> y.kegg, "IPR_ID" -> y.iprId,
      "IPR_Descrip" -> y.iprDescrip,"cDNA" -> cdna,"CDS" -> cds , "PEP" -> pep )
  }

  def selectByGeneId(id: String): Action[AnyContent] = Action.async { implicit request =>
    geneInformationDao.selectById(id).map { x =>
      val array = x.map { y =>
        getLongArray(y)
      }
      Ok(Json.toJson(array))
    }
  }

  def getMoreInfo(id: String):Action[AnyContent] = Action.async {implicit request =>
    val long = geneInformationDao.selectById(id)
    long.map { x =>
      val trueLong = x.head
      Ok (views.html.search.moreInfo(trueLong))
    }
  }

  def moreInfoBoxPlot(id:String,group1:String,group2:String) : Action[AnyContent] = Action.async {implicit request =>
    val long = geneInformationDao.selectById(id)
    long.map { x =>
      val trueLong = x.head
      Ok (views.html.analyse.moreInfo(trueLong ,group1,group2))
    }
  }

  def searchByRegion = Action.async{ implicit request =>
    val data = regionForm.bindFromRequest.get
    geneInformationDao.selectByRegion(data).map{x=>
      val json = x.map{ y=>
        getLongArray(y)
      }
      Ok(Json.toJson(json))
    }
  }

  def getAllChr = Action.async{implicit request=>
     geneInformationDao.allChr.map{x=>
       Ok(Json.toJson(x.distinct))
     }
  }
}