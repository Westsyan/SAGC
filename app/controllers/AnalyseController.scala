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
import utils.{Utils, ExecCommand}

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
    Ok(views.html.English.analyse.ttest())
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
    Ok(views.html.English.analyse.correlation())
  }

  def kegg = Action {
    Ok(views.html.English.analyse.kegg("NA"))
  }

  def go = Action {
    Ok(views.html.English.analyse.go("NA"))
  }

  def clusterIndex = Action {
    Ok(views.html.English.analyse.cluster())
  }

  def selectAllgene(group1: String, group2: String, c: String, pval: String): Action[AnyContent] = Action { implicit request =>
    val g1 = standard(group1).sortBy(_._1)
    val g2 = standard(group2).sortBy(_._1)
    val allArray1 = ArrayBuffer[Array[Double]]()
    val allArray2 = ArrayBuffer[Array[Double]]()
    val tmpDir = Files.createTempDirectory("tmpDir").toString
    val path = tmpDir.replaceAll("\\\\", "/")
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
          logFC = log2(mean2 / mean1).formatted("%.2f")
        } else {
          logFC = "NA"
        }
        (g1(i)._1, mean1, g1(i)._3, mean2, g2(i)._3, logFC)
      }
    }.toArray
    val execCommand = new ExecCommand
    val command = getPQValues(allArray1, allArray2, path)
    execCommand.exec(command)
    if(execCommand.isSuccess) {
      val pQLines = Source.fromFile(tmpDir + "/tmp.txt").getLines().map(_.split("\t").toBuffer).toBuffer
      val json2 = pQLines.map { x =>
        var pvalue = ""
        if (x(0) != "NA") {
          pvalue = x(0).toDouble.formatted("%.4f")
        } else {
          pvalue = x(0)
        }
        pvalue
      }.toArray
      val json = json1.zip(json2).map(x => (x._1, x._2))
      val json3 = json.sortBy(_._2).map { x =>
        ("geneId" -> x._1._1, "mean1" -> x._1._2, "stderr1" -> x._1._3, "mean2" -> x._1._4, "stderr2" -> x._1._5, "logFC" -> x._1._6, "p-value" -> x._2)
      }
      val json4 = json3.filter(_._2._2.toDouble != 0).filter(_._6._2 != "NA").filter(_._6._2.toDouble.abs >= c.toDouble).filter(_._7._2 != "NA").filter(_._7._2.toDouble <= pval.toDouble)
      val header = request.headers.toMap
      val refer = header.filter(_._1 == "Referer").map(_._2).head.head
      val jsons = json4.map { x =>
        var genenameStr = ""
        if (refer.contains("chinese")) {
          genenameStr = "<a target='_blank' href='" + routes.ChineseController.moreInfoBoxPlot(x._1._2, group1, group2) + "'>" + x._1._2 + "</a>"
        } else {
          genenameStr = "<a target='_blank' href='" + routes.GeneInformationController.moreInfoBoxPlot(x._1._2, group1, group2) + "'>" + x._1._2 + "</a>"
        }
        Json.obj(x._1._1 -> genenameStr, x._2._1 -> x._2._2, x._3._1 -> x._3._2, x._4._1 -> x._4._2, x._5._1 -> x._5._2, x._6._1 -> x._6._2, x._7._1 -> x._7._2)
      }
      val getId = json4.map(_._1._2).mkString(",")
      new File(Utils.path, "getId.txt").delete()
      FileUtils.writeStringToFile(new File(Utils.path, "getId.txt"), getId)
      Utils.deleteDirectory(tmpDir)
      Ok(Json.toJson(jsons))
    }else{
      Utils.deleteDirectory(tmpDir)
      Ok(Json.obj("valid" -> "false", "message" -> execCommand.getErrStr))
    }
  }

  def toGo = Action {
    val buffer = FileUtils.readLines(new File(Utils.path, "getId.txt")).asScala
    Ok(views.html.English.analyse.go(buffer(0)))
  }

  def toKegg = Action {
    val buffer = FileUtils.readLines(new File(Utils.path, "getId.txt")).asScala
    Ok(views.html.English.analyse.kegg(buffer(0)))
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
    val execCommand = new ExecCommand
    val command = "perl " + Utils.path + "identify.pl -study=" + study + " -population=" + population + " -association=" + association +
      " -m=" + m + " -n=" + n + " -o=" + o + " -c=" + c + " -maxp=" + pval
    execCommand.exe(command, tmpDir)
    if (execCommand.isSuccess) {
      val keggInfo = FileUtils.readLines(output).asScala
      val buffer = keggInfo.drop(1)
      val json = buffer.map { x =>
        val all = x.split("\t")
        val l = all.size
        /*   val keggLink = "<a target-'_blank' href='http://www.kegg.jp/dbget-bin/www_bget?ko:"+ all(2) + "'>" + all(2) + "</a>"*/
        val hyper = "<a target='_blank' href='" + all(8) + "'>linked</a><a style='display: none'>: " + all(8) + "</a>"
        Json.obj("term" -> all.head, "database" -> all(1), "id" -> all(2), "input_num" -> all(3), "back_num" -> all(4),
          "p-value" -> all(5), "correct_pval" -> all(6), "input" -> all(7), "hyperlink" -> hyper)
      }
      Utils.deleteDirectory(tmpDir)
      Ok(Json.toJson(json))
    } else {
      Utils.deleteDirectory(tmpDir)
      Ok(Json.obj("valid" -> "false", "message" -> execCommand.getErrStr))
    }
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
    val execCommand = new ExecCommand
    val command = "python " + Utils.path + "goatools-0.5.7/scripts/find_enrichment.py --alpha=" + alpha + " --pval=" + pval +
      " --output " + output + " " + study + " " + population + " " + association
    execCommand.exe(command,tmpDir)
    if (execCommand.isSuccess) {
      val goInfo = FileUtils.readLines(o).asScala
      val buffer = goInfo.drop(1)
      val json = buffer.map { x =>
        val all = x.split("\t")
        val goLink = "<a target='_blank' href='http://amigo.geneontology.org/amigo/term/" + all(0) + "'>" + all(0) + "</a>"
        Json.obj("id" -> goLink, "enrichment" -> all(1), "description" -> all(2), "ratio_in_study" -> all(3), "ratio_in_pop" -> all(4),
          "p_uncorrected" -> all(5), "p_bonferroni" -> all(6), "p_holm" -> all(7), "p_sidak" -> all(8), "p_fdr" -> all(9),
          "namespace" -> all(10), "genes_in_study" -> all(11))
      }
      Utils.deleteDirectory(tmpDir)
      Ok(Json.toJson(json))
    } else {
      Utils.deleteDirectory(tmpDir)
      Ok(Json.obj("valid" -> "false", "message" -> execCommand.getErrStr))
    }
  }

  def correlationMethod(sampleName: String, method: String, tmpDir: String) = {
    val info = Await.result(mRNAProfileDao.selectAllBySampleName(sampleName), Duration.Inf)
    val array = getArray(info).toBuffer
    val sam = info.map(_.samplename).distinct.toArray
    val buffer = sam +: array
    FileUtils.writeLines(new File(tmpDir, "tmp.txt"), buffer.map(_.mkString("\t")).asJava)
    var s = ArrayBuffer[String]()
    s = s :+ s"setwd('$tmpDir')"
    s = s :+ "a <- read.table('tmp.txt', sep='\t', header=TRUE,fill=TRUE)"
    s = s :+ s"out <- cor(a,  use='pairwise.complete.obs', method='$method')"
    s = s :+ "write.table(out, 'out_matrix.txt', quote=F, sep='\t')"
    FileUtils.writeLines(new File(tmpDir, "cmd.r"), s.asJava)
    ("Rscript " + tmpDir + "/cmd.r")
  }

  def CorrelationHeatmap(sampleName: String, method: String): Action[AnyContent] = Action { implicit request =>
    val tmpDir = Files.createTempDirectory("tmpDir").toString
    val path = tmpDir.replaceAll("\\\\", "/")
    val execCommand = new ExecCommand
    val command = correlationMethod(sampleName, method, path)
    execCommand.exec(command)
    if(execCommand.isSuccess) {
      val correlation = Source.fromFile(tmpDir + "/out_matrix.txt").getLines().map(_.split("\t").toBuffer).toBuffer
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
      Utils.deleteDirectory(tmpDir)
      Ok(Json.toJson(jsons))
    }else{
      Utils.deleteDirectory(tmpDir)
      Ok(Json.obj("valid" -> "false", "message" -> execCommand.getErrStr))
    }
  }

  def getPQValues(allArray1: ArrayBuffer[Array[Double]], allArray2: ArrayBuffer[Array[Double]], tmpDir: String) = {
    val fw = new FileWriter(new File(tmpDir, "cmd.r"))
    fw.write(s"setwd('$tmpDir')\n")
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
    ("Rscript " + tmpDir + "/cmd.r")
  }

  def clusterRun(sampleName: String): Action[AnyContent] = Action.async { implicit request =>
    val sample = sampleName.split(",").map(_.trim).distinct
    mRNAProfileDao.selectAllBySampleName(sampleName).map { x =>
      val array = getArray(x).toBuffer
      val sam = x.map { y => y.samplename }.distinct.toArray
      val buffer = sam +: array
      val tmpDir = Files.createTempDirectory("tmpDir").toString
      FileUtils.writeLines(new File(tmpDir, "data.txt"), buffer.map(_.mkString("\t")).asJava)
      val output = new File(tmpDir, "output.txt")
      val height = sample.size * 30 + 300
      val execCommand = new ExecCommand
      val command1 = rCluster(tmpDir, output)
      val command2 = pyCluster(output, sampleName, height)
      execCommand.exec(command1,command2)
      if(execCommand.isSuccess) {
        val div = FileUtils.readLines(new File(tmpDir, "div.txt")).asScala.mkString
        val div1 = div.replaceAll(" \"text\": null,", "")
        val div2 = div1.replaceAll("\"hoverinfo\": \"text\",", "")
        val json = Json.obj("div" -> div2)
        Utils.deleteDirectory(tmpDir)
        Ok(Json.toJson(json))
      }else{
        Utils.deleteDirectory(tmpDir)
        Ok(Json.obj("valid" -> "false", "message" -> execCommand.getErrStr))
      }
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
    ("Rscript " + cmd.getAbsolutePath)
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
    ("python " + pyFile.getAbsolutePath)
  }

  def getArray1(x: Seq[MrnaprofileRow]) = {
    val y = x.groupBy(_.geneid).map {
      case (geneid, sample) =>
        val map = sample.map(y => log2(y.value + 1).formatted("%.3f").toDouble)
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
    Ok(views.html.English.analyse.scatterplot())
  }

  def scatterPlot(sample1: String, sample2: String): Action[AnyContent] = Action.async { implicit request =>
    val sampleName = sample1 + "," + sample2
    val tmpDir = Files.createTempDirectory("tmpDir").toString
    val path = tmpDir.replaceAll("\\\\", "/")
    mRNAProfileDao.selectAllBySampleName(sampleName).map { x =>
      val execCommand = new ExecCommand
      val command = correlationMethod(sampleName, "pearson", path)
      execCommand.exec(command)
      if(execCommand.isSuccess) {
        val correlation = Source.fromFile(tmpDir + "/out_matrix.txt").getLines().map(_.split("\t").toBuffer).toBuffer
        val value = correlation(1)(2)
        val long = getArray1(x)
        val json = Json.obj("series" -> long, "pc1" -> sample1, "pc2" -> sample2, "length" -> long.size, "correlation" -> value)
        Utils.deleteDirectory(tmpDir)
        Ok(json)
      }else{
        Utils.deleteDirectory(tmpDir)
        Ok(Json.obj("valid" -> "false", "message" -> execCommand.getErrStr))
      }
    }
  }

  def PCAIndex = Action { implicit request =>
    Ok(views.html.English.analyse.PCA())
  }

  def PCA(sampleName: String): Action[AnyContent] = Action { implicit request =>
    var buffer = mutable.Buffer[String]()
    val tmpDir = Files.createTempDirectory("tmpDir").toString
    val path = tmpDir.replaceAll("\\\\", "/")
    if (sampleName.isEmpty) {
      buffer = FileUtils.readLines(new File(Utils.path, "pca.txt")).asScala
    } else {
      getExpressionFile(sampleName, tmpDir)
      pcaR(path)
      buffer = FileUtils.readLines(new File(tmpDir, "pca.txt")).asScala
    }
    val value = buffer.drop(1).map(_.split("\t"))
    val pca = value.map { x => (x(0), x.drop(1)) }
    covR(pca, path)
    val buf = pca.map { x =>
      val data = x._2.map(_.toDouble).toSeq
      val color = "rgba(30,144,255, .5)"
      Json.obj("name" -> x._1, "color" -> color, "data" -> Iterable(data))
    }
    val b = FileUtils.readLines(new File(tmpDir, "cov.txt")).asScala
    val pc = b.map(_.split("\t"))
    val pc1 = pc(1)(1)
    val pc2 = pc(2)(2)
    Ok(Json.obj("series" -> buf, "pc1" -> pc1, "pc2" -> pc2))
  }

  def getExpressionFile(sampleName: String, tmpDir: String) = {
    val sample = sampleName.split(",").map(_.trim).distinct
    val express = Await.result(mRNAProfileDao.selectAllBySampleName(sampleName), Duration.Inf)
    val array = getArray(express).toBuffer
    val buffer = sample +: array
    FileUtils.writeLines(new File(tmpDir, "expression.txt"), buffer.map(_.mkString("\t")).asJava)
  }

  def pcaR(tmpDir: String) = {
    var rStr = s"setwd('$tmpDir')\n"
    rStr +=
      """
        |a <- read.table('expression.txt', sep='\t', header=TRUE,fill=TRUE)
        |pr <- prcomp(a)
        |prr <- pr$rotation
        |prs <- prr[,1:2]
        |write.table(prs,'pca.txt',quote=F,sep='\t')
      """.stripMargin
    FileUtils.writeStringToFile(new File(tmpDir, "cmd.r"), rStr)
    Process("Rscript " + tmpDir + "/cmd.r").!
  }

  def covR(pca: mutable.Buffer[(String, Array[String])], tmpDir: String) = {
    val file = new FileWriter(new File(tmpDir, "covoriginal .txt"))
    file.write("PCA1\tPCA2\n")
    pca.foreach { x =>
      file.write(x._1 + "\t" + x._2.mkString("\t") + "\n")
    }
    file.close()
    val newFile = new File(tmpDir, "cov.txt")
    var rStr = s"setwd('$tmpDir')"
    rStr +=
      """
        |a <- read.table("covoriginal .txt", sep='\t', header=TRUE,fill=TRUE)
        |cov <- cov(a)
        |write.table(cov,'cov.txt',quote=F,sep='\t')
      """.stripMargin
    FileUtils.writeStringToFile(new File(tmpDir, "covCmd.r"), rStr)
    Process("Rscript " + tmpDir + "/covCmd.r").!
  }

  def log2(x: Double) = log10(x) / log10(2.0)

}
