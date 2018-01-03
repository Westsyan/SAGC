package controllers

import java.io.File
import javax.inject.Inject

import dao.{CorrelationDao, GeneIdDao, GeneInformationDao, MRNAProfileDao}
import models.Tables._
import org.apache.commons.io.FileUtils
import org.apache.commons.math3.stat.correlation.PearsonsCorrelation
import play.api.libs.json.Json
import play.api.mvc.{Action, Controller}
import utils.Utils

import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.JavaConverters._
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.sys.process.Process

class SaveDatabase @Inject()(geneIdDao: GeneIdDao, mRNAProfileDao: MRNAProfileDao, geneInformationDao: GeneInformationDao, correlationDao: CorrelationDao) extends Controller {

  def saveMRNAProfile = Action { implicit request =>
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
    val genomeMap = toBuffer.map { x =>
      val bu = x.split("\t")
      (bu(0), bu(3), bu(4), bu(8).split(";")(1).split("=")(1), bu(6))
    }
    val genomeId = genomeMap.map(_._4)

    val annoMap = annoBuf.map { x =>
      (x(0), x(1), x(2), x(3))
    }
    val annoId = annoMap.map(_._1)

    val cdnaMap = cdnaBuf.map { x =>
      val geneId = x.split(",")(0).split("gene").slice(1, 2).mkString.split(":").slice(1, 2).mkString.trim
      val cds = x.split(",").drop(1).map(_.trim).mkString
      (geneId, cds)
    }
    val cdnaId = cdnaMap.map(_._1)

    val pepMap = pepBuf.map { x =>
      val geneId = x.split(",")(0).split("gene:").slice(1, 2).mkString.split(" ")(0)
      val cds = x.split(",").drop(1).map(_.trim).mkString
      (geneId, cds)
    }
    val pepId = pepMap.map(_._1)

    val cdsMap = cdsBuf.map { x =>
      val geneId = x.split(",")(0).split("gene").slice(1, 2).mkString.split(":").slice(1, 2).mkString.trim
      val cds = x.split(",").drop(1).map(_.trim).mkString
      (geneId, cds)
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
      if (genomeId.contains(allGene)) {
        Chr = genomeMap.filter(_._4 == allGene).map(_._1).distinct.head
        val max = genomeMap.filter(_._4 == allGene).map { y => (y._2, y._3, y._3.toInt - y._2.toInt) }
        start = max.filter(_._3 == max.map(_._3).max).map(_._1).head
        end = max.filter(_._3 == max.map(_._3).max).map(_._2).head
        dir = genomeMap.filter(_._4 == allGene).map(_._5).distinct.head
      } else {
        Chr = "NA"
        start = "NA"
        end = "NA"
        dir = "NA"
      }
      var genename = ""
      var interproId = ""
      var interproDescription = ""
      if (annoId.contains(allGene)) {
        genename = annoMap.filter(_._1 == allGene).map(_._2).distinct.mkString

        interproId = annoMap.filter(_._1 == allGene).map(_._3).distinct.mkString(",")
        interproDescription = annoMap.filter(_._1 == allGene).map(_._4).distinct.mkString(",")
      } else {
        genename = "NA"
        interproId = "NA"
        interproDescription = "NA"
      }
      var pep = ""
      if (pepId.contains(allGene)) {
        val s1 = pepMap.filter(_._1 == allGene)
        val s2 = s1.map(_._2).distinct
        val s3 = s1.map(_._2.size).max
        pep = s1.filter(_._2.size == s3).distinct.map(_._2).mkString
      } else {
        pep = "NA"
      }
      var cds = ""
      if (cdsId.contains(allGene)) {
        val s1 = cdsMap.filter(_._1 == allGene)
        val s2 = s1.map(_._2).distinct
        val s3 = s1.map(_._2.size).max
        cds = s1.filter(_._2.size == s3).distinct.map(_._2).mkString
      } else {
        cds = "NA"
      }
      var cdna = ""
      if (cdnaId.contains(allGene)) {
        val s1 = cdnaMap.filter(_._1 == allGene)
        val s2 = s1.map(_._2).distinct
        val s3 = s1.map(_._2.size).max
        cdna = s1.filter(_._2.size == s3).distinct.map(_._2).mkString
      } else {
        cdna = "NA"
      }
      GeneinformationRow(allGene, genename, Chr.toInt, start.toInt, end.toInt, dir, func, go, kegg, interproId, interproDescription, cdna, cds, pep)
    }
    println("总长度：" + rows.size)
    println(Utils.getTime(startTime))

    rows.grouped(10000).foreach { x =>
      println(Utils.getTime(startTime))
      println(x(1))
      Await.result(geneInformationDao.insert(x), Duration.Inf)
    }
    println("插入结束")
    Ok(views.html.English.save())
  }

  def getArayy(x: Seq[MrnaprofileRow]) = {
    x.groupBy(_.samplename).map {
      case (samplename, geneid) =>
        val map1 = samplename
        val map2 = geneid.map(_.value.toString).toArray
        val map = map1 +: map2
        map
    }
  }

  def getArray(x: Seq[MrnaprofileRow]) = {
    x.groupBy(_.geneid).map {
      case (geneid, sample) =>
        val map1 = geneid
        val map2 = sample.map(_.value).toArray
        (map1, map2)
    }
  }

  def saveCorrelation = Action {
    val x = Await.result(geneIdDao.selectAllGeneId, Duration.Inf)
    val startTime = System.currentTimeMillis()
    val count = 500
    for (i <- 0 until(1000, count)) {
      val r1 = Await.result(mRNAProfileDao.selectByGeneId(x.slice(i, i + count).mkString(",")), Duration.Inf)
      println("读取数据成功:" + Utils.getTime(startTime))
      val array1 = getArayy(r1).toBuffer
      val header1 = r1.map(_.geneid).toArray.distinct
      val buffe1 = header1 +: array1
      FileUtils.writeLines(new File(Utils.path + "tmp1.txt"), buffe1.map(_.mkString("\t")).asJava)
      println("建立文件成功" + Utils.getTime(startTime))
      Process("Rscript " + Utils.path + "cmd1.r").!
      println("R程序运行成功" + Utils.getTime(startTime))
      val buffer1 = FileUtils.readLines(new File(Utils.path + "out_matrix1.txt")).asScala
      val buf1 = buffer1.drop(1).map(_.split("\t")).toArray
      val head1 = buffer1.head.split("\t")
      val row1 = for (i <- 0 until head1.size; j <- i + 1 until head1.size) yield {
        if (buf1(i)(j + 1) != "NA") {
          CorrelationRow(buf1(i)(0), buf1(j)(0), buf1(i)(j + 1).toDouble)
        } else {
          CorrelationRow(buf1(i)(0), buf1(j)(0), 0)
        }
      }
      correlationDao.insertAll(row1)
      for (j <- i + count until(x.size, count)) {
        println(Utils.getTime(startTime) + " :" + i)
        val result = Await.result(mRNAProfileDao.selectByGeneId(x.slice(i, i + count).mkString(",") + "," + x.slice(j, j + count).mkString(",")), Duration.Inf)
        println(j)
        println(x.slice(j, j + count).head, x.slice(j, j + count).last)
        println("读取数据成功:" + Utils.getTime(startTime))
        val array = getArayy(result).toBuffer
        val header = result.map(_.geneid).toArray.distinct
        val buffe = header +: array
        FileUtils.writeLines(new File(Utils.path + "tmp.txt"), buffe.map(_.mkString("\t")).asJava)
        println("建立文件成功" + Utils.getTime(startTime))
        Process("Rscript " + Utils.path + "cmd.r").!
        println("R程序运行成功" + Utils.getTime(startTime))
        val buffer = FileUtils.readLines(new File(Utils.path + "out_matrix.txt")).asScala
        val buf = buffer.drop(1).map(_.split("\t")).toArray
        val head = buffer.head.split("\t")
        println(buf(head.size - 1)(0), buf(head.size - 1)(0), buf(head.size - 1)(head.size))
        val row = for (i <- 0 until head.size; j <- i + 1 until head.size) yield {
          if (buf(i)(j + 1) != "NA") {
            CorrelationRow(buf(i)(0), buf(j)(0), buf(i)(j + 1).toDouble)
          } else {
            CorrelationRow(buf(i)(0), buf(j)(0), 0)
          }
        }
        val row2 = row.diff(row1)
        println(row2.size)
        correlationDao.insertAll(row2)
        println("数据库插入成功：" + Utils.getTime(startTime))
      }
    }
    Ok(Json.toJson("13"))
  }

  def saveCor = Action {
    val startTime = System.currentTimeMillis()
    println("开始得到数据：" + Utils.getTime(startTime))
    val q = Await.result(geneIdDao.selectAllGeneId, Duration.Inf)
    val all = for (i <- 0 until q.size) yield {
      println(i)
      val a = Await.result(mRNAProfileDao.selectByGeneId(q(i)), Duration.Inf)
      getArray(a)
    }
    println("开始处理数据：" + Utils.getTime(startTime))
    for (z <- 0 until(q.size, 500)) {
      val row = for (i <- z until (q.size ,500); j <- 0 until q.size) yield {
        val x1 = all(i).map(_._2).head
        val x2 = all(j).map(_._2).head
        val c = new PearsonsCorrelation().correlation(x1, x2)
        CorrelationRow(q(i), q(j), c)
      }
      println(row.head)
      println(row.size)
      println("row生成成功：" + Utils.getTime(startTime))
      val rows = row.filter(_.correlation >= 0.5).filter(_.correlation < 1)
      println(rows.size)
      println(rows.head)
      correlationDao.insertAll(rows)
      println("row插入成功：" + Utils.getTime(startTime))
    }
    Ok(Json.toJson("13"))
  }

  def saveCor2 = Action {
    val startTime = System.currentTimeMillis()
    println("开始得到数据：" + Utils.getTime(startTime))
    val q = Await.result(geneIdDao.selectAllGeneId, Duration.Inf)
    val all = for (i <- 0 until q.size) yield {
      println(i)
      val a = Await.result(mRNAProfileDao.selectByGeneId(q(i)), Duration.Inf)
      getArray(a)
    }
    println("开始处理数据：" + Utils.getTime(startTime))
    for (z <- 0 until(q.size, 500)) {
      val row = for (i <- z until (q.size ,500); j <- 0 until q.size) yield {
        val x1 = all(i).map(_._2).head
        val x2 = all(j).map(_._2).head
        val c = new PearsonsCorrelation().correlation(x1, x2)
        CorrelationRow(q(i), q(j), c)
      }
      println(row.head)
      println(row.size)
      println("row生成成功：" + Utils.getTime(startTime))
      val rows = row.filter(_.correlation >= 0.5).filter(_.correlation < 1)
      println(rows.size)
      println(rows.head)
      correlationDao.insertAll(rows)
      println("row插入成功：" + Utils.getTime(startTime))
    }
    Ok(Json.toJson("13"))
  }


  def saveCor1 = Action.async {
    val startTime = System.currentTimeMillis()
    println("开始得到数据：" + Utils.getTime(startTime))
    mRNAProfileDao.selectAll.map { all =>
      println("开始处理数据：" + Utils.getTime(startTime))
      val a = getArray(all).toArray
      var count = 0
      a.foreach { x =>
        val row = a.map { y =>
          val c = new PearsonsCorrelation().correlation(x._2, y._2)
          CorrelationRow(x._1, y._1, c)
        }
        if (count % 1000 == 0) {
          println(count + "   " + Utils.getTime(startTime))
        }
        count = count + 1
        val rows = row.filter(_.correlation >= 0.5).filter(_.correlation < 0.9999999999999999)
        correlationDao.insertAll(rows)
      }
      Ok(Json.toJson("success"))
    }
  }

  def saveCor3 = Action{
    val buffer = FileUtils.readLines(new File(Utils.path + "column.txt")).asScala
    val gene = buffer.drop(1)
    println(1)
    val b =for(i <- 0 until gene.size)yield{
      val x = gene(i).split("\t")
      (x.head,x.drop(1).map(_.toDouble))
    }
    println(2)
    for(i <- 0 until gene.size){
     val row =  for(j <- 0 until gene.size)yield{
        val x1 = b(i)._2
        val x2 = b(j)._2
        val c = new PearsonsCorrelation().correlation(x1, x2)
        CorrelationRow(b(i)._1,b(j)._1,c)
     }
      val rows = row.filter(_.correlation>=0.5).filter(_.correlation<1)
      println(i)
      correlationDao.insertAll(rows)
    }
    Ok(Json.obj("message" ->"Database update successfully"))
  }

  def saveCorByGene(moreGene: Seq[String]) = {
    val moreValue = Await.result(mRNAProfileDao.selectByGeneId(moreGene.mkString(",")), Duration.Inf)
    val allValue = Await.result(mRNAProfileDao.selectAll, Duration.Inf)
    val allGet = getArray(allValue).toArray
    val moreGet = getArray(moreValue).toArray
    moreGet.map { x =>
      val row = allGet.map { y =>
        val c = new PearsonsCorrelation().correlation(x._2, y._2)
        CorrelationRow(x._1, y._1, c)
      }
      val rows = row.filter(_.correlation >= 0.5).filter(_.correlation < 1)
      correlationDao.insertAll(rows)
    }
    val message = "success"
    (message)
  }


  def check = Action {
    val gene = Await.result(geneIdDao.selectAllGeneId, Duration.Inf)
    val sample = Await.result(mRNAProfileDao.selectAllSampleName, Duration.Inf)
    val geneSize = Await.result(correlationDao.selectById(gene.mkString(",")), Duration.Inf)
    val cGene = Await.result(correlationDao.selectAllGene, Duration.Inf)
    var message = ""
    if (gene.size != cGene.size) {
      val moreGene = cGene.diff(gene)
      message = saveCorByGene(moreGene)
    }

    Ok(Json.toJson(""))
  }
}
