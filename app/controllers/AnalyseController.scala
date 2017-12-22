package controllers


import java.io.{File, FileWriter}
import java.nio.file.Files
import javax.inject.Inject

import dao.{CorrelationDao, GeneIdDao, MRNAProfileDao}
import models.Tables._
import org.apache.commons.io.FileUtils
import org.apache.commons.math3.stat.StatUtils
import org.apache.commons.math3.stat.descriptive.moment.StandardDeviation
import play.api.data.Form
import play.api.data.Forms._
import play.api.libs.json.Json
import play.api.mvc.{Action, AnyContent, Controller}
import utils.Utils

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.io.Source
import scala.math._
import scala.sys.process.Process

case class checkGroup(group1: String, group2: String, c: String, pval: String)

case class checkSamplename(samplename: String, method: String)

case class checkKeggData(id: String, m: String, n: String, c: String, pval: String)

case class checkGoData(id: String, alpha: String, pval: String)

class AnalyseController @Inject()(geneIdDao: GeneIdDao, mRNAProfileDao: MRNAProfileDao, correlationDao: CorrelationDao) extends Controller {

  def tTest = Action {
    Ok(views.html.analyse.ttest())
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

  val goForm = Form(
    mapping(
      "id" -> text,
      "alpha" -> text,
      "pval" -> text
    )(checkGoData.apply)(checkGoData.unapply)
  )

  def cHeatmap = Action {
    Ok(views.html.analyse.correlation())
  }

  def kegg = Action {
    Ok(views.html.analyse.kegg())
  }

  def go = Action {
    Ok(views.html.analyse.go())
  }

  def clusterIndex = Action {
    Ok(views.html.analyse.cluster())
  }

  def selectAllgene(group1: String, group2: String, c: String, pval: String): Action[AnyContent] = Action { implicit request =>
    val g1 = standard(group1).sortBy(_._1)
    val g2 = standard(group2).sortBy(_._1)
    val allArray1 = ArrayBuffer[Array[Double]]()
    val allArray2 = ArrayBuffer[Array[Double]]()
    val json1 = {
      for (i <- 0 until g1.size) yield {
        val array1 = g1(i)._4
        allArray1 += array1
        val array2 = g2(i)._4
        allArray2 += array2
        val mean1 = g1(i)._2.toDouble
        val mean2 = g2(i)._2.toDouble
        var logFC = ""
        if (mean2 != 0.0) {
          logFC = log2(mean1 / mean2).formatted("%.2f")
        } else {
          logFC = "NA"
        }
        (g1(i)._1, mean1, g1(i)._3, mean2, g2(i)._3, logFC)
      }
    }.toArray
    getPQValues(allArray1, allArray2)
    val pQLines = Source.fromFile(Utils.path + "tmp.txt").getLines().map(_.split("\t").toBuffer).toBuffer
    val json2 = pQLines.map { x =>
      var p = ""
      // var q = ""
      if (x(0) != "NA") {
        p = x(0).toDouble.formatted("%.4f")
      } else {
        p = x(0)
      }
      (p /*, q*/)
    }.toArray
    val json = json1.zip(json2).map(x => (x._1, x._2))
    val json3 = json.sortBy(_._2).map { x =>
      ("geneId" -> x._1._1, "mean1" -> x._1._2, "stderr1" -> x._1._3, "mean2" -> x._1._4, "stderr2" -> x._1._5, "logFC" -> x._1._6, "p-value" -> x._2)
    }
    val json4 = json3.filter(_._6._2 != "NA").filter(_._6._2.toDouble <= c.toDouble).filter(_._7._2 != "NA").filter(_._7._2.toDouble <= pval.toDouble)
    val jsons = json4.map { x =>
      val genenameStr = "<a target='_blank' href='" + routes.GeneInformationController.moreInfoBoxPlot(x._1._2, group1, group2) + "'>" + x._1._2 + "</a>"
      Json.obj(x._1._1 -> genenameStr, x._2._1 -> x._2._2, x._3._1 -> x._3._2, x._4._1 -> x._4._2, x._5._1 -> x._5._2, x._6._1 -> x._6._2, x._7._1 -> x._7._2)
    }
    new File(Utils.path + "tmp.txt").delete()
    new File("cmd.r").delete()
    Ok(Json.toJson(jsons))
  }

  def standard(s1: String) = {
    val info1 = Await.result(mRNAProfileDao.selectAllBySampleName(s1), Duration.Inf)
    val array = getArrayByGenotypes(info1).toArray
    array.map { x =>
      val geneid = x.map(_._2).head
      val value = x.map(_._2).drop(1).toArray
      val values = {
        for (i <- 0 until value.size) yield
          value(i).toDouble
      }.toArray
      val standardDeviation = new StandardDeviation
      val mean = StatUtils.mean(values).formatted("%.2f")
      //标准误差
      val stderr = (standardDeviation.evaluate(values) / Math.sqrt(values.length)).formatted("%.2f")
      (geneid, mean, stderr, values)
    }
  }

  def getArrayByGenotypes(x: Seq[MrnaprofileRow]) = {
    x.groupBy(_.geneid).map {
      case (geneid, sample) =>
        val map1 = mutable.LinkedHashMap("geneId" -> geneid)
        val map2 = sample.map(y => (y.samplename -> y.value.toString)).toMap
        val map = map1 ++ map2
        map
    }
  }

  def getArray(x: Seq[MrnaprofileRow]) = {
    x.groupBy(_.geneid).map {
      case (geneid, sample) =>
        val map1 = geneid
        val map2 = sample.map(_.value.toString).toArray
        val map = map1 +: map2
        map
    }
  }

  def keggResult = Action { implicit request =>
    val data = keggForm.bindFromRequest.get
    val Id = data.id
    val c = data.c
    val m = data.m
    val n = data.n
    val pval = data.pval
    val population = Utils.path + "all_gene.txt"
    val association = Utils.path + "ref_gene.pathway.txt"
    val geneId = Id.split(",").map(_.trim).distinct.toBuffer
    val tmpDir = Files.createTempDirectory("tmpDir").toString
    //在临时的文件存放点钟建立一个新的文件
    val studies = new File(tmpDir, "tmp.txt")
    FileUtils.writeLines(studies, geneId.asJava)
    val study = studies.getAbsolutePath
    val output = new File(tmpDir, "KEGG_enrichment.txt")
    val o = output.getAbsolutePath
    // println(study,population,association,m,n,o,c,pval)
    //QVALUE在unix转译文本后可以使用
    val x1 = Process("perl " + Utils.path + "identify.pl -study=" + study + " -population=" + population + " -association=" + association +
      " -m=" + m + " -n=" + n + " -o=" + o + " -c=" + c + " -maxp=" + pval).!
    val keggInfo = FileUtils.readLines(output).asScala
    val buffer = keggInfo.drop(1)
    val json = buffer.map { x =>
      val all = x.split("\t")
      val l = all.size
      val hyper = "<a target='_blank' href='" + all(8) + "'>linked</a><a style='display: none'>: " + all(8) + "</a>"
      Json.obj("term" -> all.head, "database" -> all(1), "id" -> all(2), "input_num" -> all(3), "back_num" -> all(4),
        "p-value" -> all(5), "correct_pval" -> all(6), "input" -> all(7), "hyperlink" -> hyper)
    }
    studies.delete()
    output.delete()
    Ok(Json.toJson(json))
  }

  def goResult = Action { implicit request =>
    val startTime = System.currentTimeMillis()
    val data = goForm.bindFromRequest.get
    val alpha = data.alpha
    val pval = data.pval
    val Id = data.id
    val geneId = Id.split(",").map(_.trim).distinct.toBuffer
    val tmpDir = Files.createTempDirectory("tmpDir").toString
    //在临时的文件存放点钟建立一个新的文件
    val studies = new File(tmpDir, "tmp.txt")
    FileUtils.writeLines(studies, geneId.asJava)
    val study = studies.getAbsolutePath
    val population = Utils.path + "all_gene.txt"
    val association = Utils.path + "ref_gene.Go.txt"
    val o = new File(tmpDir, "GO_enrichment.txt")
    val output = o.getAbsolutePath
    println("goatools开始运行：" + Utils.getTime(startTime))
    val x1 = Process("python " + Utils.path + "goatools-0.5.7/scripts/find_enrichment.py --alpha=" + alpha + " --pval=" + pval +
      " --output " + output + " " + study + " " + population + " " + association).!
    println("goatools结束运行：" + Utils.getTime(startTime))
    val goInfo = FileUtils.readLines(o).asScala
    val buffer = goInfo.drop(1)
    val json = buffer.map { x =>
      val all = x.split("\t")
      Json.obj("id" -> all(0), "enrichment" -> all(1), "description" -> all(2), "ratio_in_study" -> all(3), "ratio_in_pop" -> all(4),
        "p_uncorrected" -> all(5), "p_bonferroni" -> all(6), "p_holm" -> all(7), "p_sidak" -> all(8), "p_fdr" -> all(9),
        "namespace" -> all(10), "genes_in_study" -> all(11))
    }
    o.delete()
    studies.delete()
    Ok(Json.toJson(json))

  }

  def correlationMethod(sampleName: String, method: String) = {
    val info = Await.result(mRNAProfileDao.selectAllBySampleName(sampleName), Duration.Inf)
    val array = getArray(info).toBuffer
    val sam = info.map(_.samplename).distinct.toArray
    val buffer = sam +: array
    FileUtils.writeLines(new File(Utils.path + "tmp.txt"), buffer.map(_.mkString("\t")).asJava)
    // val rFile=new File("D:\\workspace\\SAGCRiceDB\\cmd.r")
    var s = ArrayBuffer[String]()
    s = s :+ s"setwd('${Utils.path}')"
    s = s :+ "a <- read.table('tmp.txt', sep='\t', header=TRUE,fill=TRUE)"
    s = s :+ s"out <- cor(a,  use='pairwise.complete.obs', method='$method')"
    s = s :+ "write.table(out, 'out_matrix.txt', quote=F, sep='\t')"
    FileUtils.writeLines(new File("cmd.r"), s.asJava)
    Process("R --restore --no-save -f cmd.r").!
    //    val exitCode = Process("Rscript "+rFile.getAbsolutePath ).!
  }

  def CorrelationHeatmap(sampleName: String, method: String): Action[AnyContent] = Action { implicit request =>
    correlationMethod(sampleName, method)
    val correlation = Source.fromFile(Utils.path + "out_matrix.txt").getLines().map(_.split("\t").toBuffer).toBuffer
    val xAxis = correlation.head
    val yAxis = correlation.drop(1).map(_.head)
    val expressions = for (i <- 1 to xAxis.size; j <- 1 to yAxis.size) yield {
      //找到符合id又符合样品名的值
      val mRNAProfile = correlation(i)(j).toDouble
      Array(i - 1, j - 1, mRNAProfile)
    }
    val max = expressions.toArray.map(x => x(2)).max
    val min = expressions.toArray.map(x => x(2)).min
    val jsons = Array(Json.obj("expression" -> expressions, "treatment" -> xAxis, "gt" -> yAxis, "max" -> max, "min" -> min))
    new File(Utils.path + "out_matrix.txt").delete()
    new File("cmd.r").delete()
    Ok(Json.toJson(jsons))
  }

  def getPQValues(allArray1: ArrayBuffer[Array[Double]], allArray2: ArrayBuffer[Array[Double]]) = {
    val fw = new FileWriter(new File("cmd.r"))
    fw.write(s"setwd('${Utils.path}')\n")
    fw.write("pValues<-c()\n")
    for (i <- allArray1.indices) {
      val x = allArray1(i).mkString("c(", ",", ")")
      val y = allArray2(i).mkString("c(", ",", ")")
      fw.write(s"x<-$x\n")
      fw.write(s"y<-$y\n")
      fw.write("pValues<-c(pValues,t.test(x,y)$p.value)\n")
    }
    //   fw.write("library(qvalue)\n")
    //  fw.write("qValues<-qvalue(pValues)$qvalues\n")
    fw.write("matrix<-cbind(pValues)\n")
    fw.write("write.table(matrix,'tmp.txt',sep='\t',col.names=F,row.names=F)\n")
    fw.close()
    Process("R --restore --no-save -f cmd.r").!
  }

  def clusterRun(id: String, sampleName: String): Action[AnyContent] = Action.async { implicit request =>
    val sample = sampleName.split(",").map(_.trim).distinct
    mRNAProfileDao.selectByPosition(id, sampleName).map { x =>
      val array = getArray(x).toBuffer
      val sam = x.map { y => y.samplename }.distinct.toArray
      val buffer = sam +: array
      val tmp = Files.createTempDirectory("tmpDir").toFile
      val tmpDir = tmp.getAbsolutePath
      FileUtils.writeLines(new File(tmpDir, "data.txt"), buffer.map(_.mkString("\t")).asJava)
      val output = new File(tmpDir, "output.txt")
      val height = sample.size * 30 + 300
      rCluster(tmpDir, output)
      pyCluster(output, sampleName, height)
      val div = FileUtils.readLines(new File(tmpDir, "div.txt")).asScala.mkString
      val div1 = div.replaceAll(" \"text\": null,", "")
      val div2 = div1.replaceAll("\"hoverinfo\": \"text\",", "")
      val json = Json.obj("div" -> div2)
      println(tmpDir)
      // FileUtils.deleteDirectory(tmp)
      Ok(Json.toJson(json))
    }
  }

  def rCluster(tmpDir: String, output: File) = {
    val outputPath = output.getAbsolutePath
    val path = tmpDir.replaceAll("\\\\", "/")
    val rStr =
      s"""
         |setwd('$path')
         |dat <-read.table('data.txt',sep="\t",header=T,row.names=1)
         |data <- t(dat)
         |da <- scale(data,center=T,scale=T)
         |da<-t(da)
         |da <- na.omit(da)
         |da<-t(da)
         |write.table(da,'output.txt',sep='\t',quote=F)
      """.stripMargin
    val cmd = new File(tmpDir, "cmd.r")
    FileUtils.writeStringToFile(cmd, rStr)
    Process("Rscript " + cmd.getAbsolutePath).!
  }

  def pyCluster(file: File, sample: String, height: Int) = {
    val path = file.getParent.replaceAll("\\\\", "/")
    val fileName = file.getName
    val names = "['" + sample.split(",").map(_.trim).distinct.mkString("','") + "']"
    val pyStr =
      s"""
         |# coding=UTF-8
         |import plotly
         |import plotly.figure_factory as ff
         |from itertools import islice
         |import numpy as np
         |import os
         |os.chdir("$path")
         |data_lines = []
         |input_file = open("$fileName")
         |for line in islice(input_file, 1, None):
         |    line = line.strip("\\n").split("\\t")
         |    del line[0]
         |    for index in range(len(line)):
         |        line[index]=float(line[index])
         |    data_lines.append(line)
         |input_file.close()
         |X = np.mat(data_lines)
         |names=$names
         |fig = ff.create_dendrogram(X, orientation='left',labels=names)
         |fig['layout'].update({'width':1160, 'height':$height,'yaxis':{'fixedrange':True},'xaxis':{'fixedrange':True}})
         |fig['data'].update({'name':""})
         |config={}
         |config['displayModeBar']=False
         |a=plotly.offline.plot(fig, filename='simple_dendrogram.html',output_type='div',include_plotlyjs=False,show_link=False,config=config)
         |f=open('div.txt','w')
         |f.write(a)
         |f.close()
      """.stripMargin
    val pyFile = new File(path, "tmp.py")
    FileUtils.writeStringToFile(pyFile, pyStr)
    Process("python " + pyFile.getAbsolutePath).!
  }

  def getArray1(x: Seq[MrnaprofileRow]) = {
    val y = x.groupBy(_.geneid).map {
      case (geneid, sample) =>
        val map = sample.map(y => log2(y.value + 1).formatted("%.3f").toDouble)
        //   Json.obj("name" -> geneid, "color" -> color, "data" -> map)
        (geneid, map)
    }
    y.groupBy(_._2).map {
      case (data, all) =>
        val name = all.map(_._1).mkString(",")
        val color = "rgba(30,144,255, .5)"
        Json.obj("name" -> name, "color" -> color, "data" -> Iterable(data))
    }
  }

  def scatterIndex = Action { implicit request =>
    Ok(views.html.analyse.scatterplot())
  }

  def scatterPlot(sample1: String, sample2: String): Action[AnyContent] = Action.async { implicit request =>
    println(sample1, sample2)
    val sampleName = sample1 + "," + sample2
    mRNAProfileDao.selectAllBySampleName(sampleName).map { x =>
      correlationMethod(sampleName, "pearson")
      val correlation = Source.fromFile(Utils.path + "out_matrix.txt").getLines().map(_.split("\t").toBuffer).toBuffer
      val value = correlation(1)(2)
      val long = getArray1(x)
      println(long.size)
      val json = Json.obj("series" -> long, "pc1" -> sample1, "pc2" -> sample2, "length" -> long.size, "correlation" -> value)
      Ok(json)
    }
  }

  def log2(x: Double) = log10(x) / log10(2.0)

}
