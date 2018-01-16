package controllers

import java.io.{File, FileWriter}
import java.nio.file.Files
import javax.inject.Inject

import play.api.data.Form
import play.api.data.Forms._
import dao._
import play.api.mvc._

import scala.concurrent.ExecutionContext.Implicits.global
import models.Tables._
import org.apache.commons.io.FileUtils
import play.api.libs.json._
import utils.Utils

import scala.collection.JavaConverters._
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.sys.process.Process

case class ChangePasswordData(account: String, password: String, newPassword: String)

class AdminController @Inject()(passwordDao: PasswordDao, geneIdDao: GeneIdDao, mRNAProfileDao: MRNAProfileDao) extends Controller {

  def toIndex = Action {
    Ok(views.html.English.admin.index())
  }

  def loginBefore() = Action { implicit request =>
    Ok(views.html.English.admin.login())
  }

  def updateCorrelation = Action{
    Ok(views.html.English.admin.updatec())
  }

  def login(phone: String, password: String) = Action.async { implicit request =>
    passwordDao.selectPassword.map { x =>
      if (phone == "admin" && password == x) {
        Redirect(routes.AdminController.toIndex()).withSession("phone" -> phone)
      } else {
        Redirect(routes.AdminController.loginBefore()).flashing("info" -> "Account or password is wrong!")
      }
    }
  }

  def logout = Action {
    Redirect(routes.AdminController.loginBefore()).flashing("info" -> "You've been logged out!").withNewSession
  }

  def changePasswordBefore() = Action { implicit request =>
    Ok(views.html.English.admin.changePassword())
  }

  val form = Form(
    mapping(
      "account" -> text,
      "password" -> text,
      "newPassword" -> text
    )(ChangePasswordData.apply)(ChangePasswordData.unapply)
  )

  def changePassword = Action.async { implicit request =>
    val data = form.bindFromRequest().get
    passwordDao.selectPassword.flatMap { x =>
      if (data.account == "admin" && data.password == x) {
        val passwordRow = PasswordRow(1, data.newPassword)
        passwordDao.updatePassword(passwordRow).map { y =>
          Redirect(routes.AdminController.loginBefore()).flashing("info" -> "Change password successfully!").withNewSession
        }
      } else {
        Future.successful(Redirect(routes.AdminController.changePasswordBefore()).flashing("info" -> "Account or password is wrong!"))
      }
    }
  }

  def addmRNABefore = Action { implicit request =>
    Ok(views.html.English.admin.addmRNA())
  }

  def addmRNA = Action(parse.multipartFormData) { implicit request =>
    //得到页面中上传的文件
    val file = request.body.file("file").get
    //创建一个临时的文件存放点
    val tmpDir = Files.createTempDirectory("tmpDir").toString
    //在临时的文件存放点钟建立一个新的文件
    val tmpTxtFile = new File(tmpDir, "tmp.txt")
    //将源文件的信息存入新建的文件中
    file.ref.moveTo(tmpTxtFile, true)
    //检查文件
    val bm = checkmRNAFile(tmpTxtFile)
    //如果文件检查出错 !bm._1=false
    if (!bm._1) {
      //删除新建的文件？
      Utils.deleteDirectory(new File(tmpDir))
      //bm.2 = false
      Ok(Json.obj("error" -> bm._2))
    } else {
      val buffer = FileUtils.readLines(tmpTxtFile).asScala
      /*      val geneIdRows = buffer.drop(1).map{x=>
              val columns = x.split("\t")
              GeneidRow(columns(0))
            }
            geneIdDao.insertAll(geneIdRows)*/
      val geneIds = Await.result(geneIdDao.selectAll, Duration.Inf)
      val geneIdMap = geneIds.map(x => (x.id, 1)).toMap
      val sampleNames = buffer.head.split("\t").drop(1)
      val startTime = System.currentTimeMillis()
      val rowsSize = geneIds.size * sampleNames.size
      var index = 0
      println("总长度" + rowsSize)
      println("--start--")
      //开始时间
      println(Utils.getTime(startTime))
      Await.result(mRNAProfileDao.deleteBySampleName(sampleNames.mkString(",")), Duration.Inf)
      println(Utils.getTime(startTime))
      sampleNames.zipWithIndex.grouped(2).foreach { x =>
        val rows = buffer.drop(1).flatMap { y =>
          val columns = y.split("\t")
          val indexs = x.map(_._2)
          if (geneIdMap.contains(columns(0))) {
            indexs.map { index =>
              MrnaprofileRow(columns(0), sampleNames(index), columns(index + 1).toDouble)
            }
          } else {
            None
          }
        }
        val validGeneIds = buffer.drop(1).map(_.split("\t")(0))
        val invalidGeneIds = geneIds.map(_.id).toBuffer.diff(validGeneIds)
        val invalidRow = for (geneid <- invalidGeneIds; sampleName <- x.map(_._1)) yield {
          MrnaprofileRow(geneid, sampleName, 0)
        }
        val finalRows = invalidRow ++ rows
        index = index + geneIds.size * 2
        finalRows.sortBy(_.samplename).grouped(40000).foreach { x =>
          Await.result(mRNAProfileDao.insertAll(x), Duration.Inf)
        }
        val percent = if ((index * 100) / rowsSize >= 100) "100%" else (index * 100) / rowsSize + "%"
        println(percent + "\t" + Utils.getTime(startTime))
      }
      saveFile
      pcaR
      println("insert table successfully!" + Utils.getTime(startTime))
      Utils.deleteDirectory(new File(tmpDir))
      Redirect(routes.AdminController.addmRNABefore())
    }
  }

  def saveFile = {
    val fw = new FileWriter(new File(Utils.path,"line.txt"))
    val sample = Await.result(mRNAProfileDao.selectAllSampleName,Duration.Inf)
    val gene = Await.result(geneIdDao.selectAllGeneId,Duration.Inf)
    fw.write(gene.sorted.mkString("\t")+"\n")
    for(i <- 0 until sample.size){
      val value = Await.result(mRNAProfileDao.selectAllBySampleName(sample(i)),Duration.Inf)
      val array = value.sortBy(_.geneid).map(_.value)
      fw.write(sample(i)+"\t"+array.mkString("\t")+"\n")
      println(i)
    }
    fw.close()
    new File(Utils.path,"acmd.r")
    val rStr =
      s"""
         |setwd("${Utils.path}")
         |a <- read.table('line.txt',sep='\t',header=TRUE,fill=TRUE)
         |out <- t(a)
         |write.table(out,'column.txt',quote=F,sep='\t')
      """.stripMargin
    FileUtils.writeStringToFile(new File(Utils.path,"acmd.r"),rStr)
    Process("Rscript " + Utils.path + "acmd.r").!
  }

  def pcaR = {
    var rStr = s"setwd('${Utils.path}')\n"
    rStr +=
      """
        |a <- read.table('column.txt', sep='\t', header=TRUE,fill=TRUE)
        |pr <- prcomp(a)
        |prr <- pr$rotation
        |prs <- prr[,1:2]
        |write.table(prs,'pca.txt',quote=F,sep='\t')
      """.stripMargin
    FileUtils.writeStringToFile(new File(Utils.path, "cmd.r"), rStr)
    Process("Rscript " + Utils.path + "cmd.r").!
  }

  /*检查文件，1.检查基因ID在文件中是否重复
              2.检查样品名在文件中是否重复
              3.检查表中除了第一行和第一列全是数据
  */
  def checkmRNAFile(file: File): (Boolean, String) = {
    //读取文件的文本内容
    val newBuffer = FileUtils.readLines(file).asScala
    //放弃第一行   //以空格分隔，选取第一个字符串
    val geneIds = newBuffer.drop(1).map(_.split("\t")(0))
    var error = ""
    /*    先将geneIds排重，然后用diff和原数组比较，剩下了之前被排重排除掉的元素，
        然后再次排重， 最后剩下的用headOption验证是否有重复，如果为none，则文
        件里没有重复，如果不为none，则显示错误*/
    val repeatid = geneIds.diff(geneIds.distinct).distinct.headOption
    repeatid match {
      //保留列表中符合条件的列表元素
      //该函数将RDD中的元素和这个元素在RDD中的ID（索引号）组合成键/值对
      //                                                  筛选出第一个元素等于x的元素，并将第二个元素+1
      case Some(x) => val nums = geneIds.zipWithIndex.filter(_._1 == x).map(_._2 + 1).mkString("(", "、", ")")
        //错误：基因ID在文件中重复
        error = "Gene ID:" + x + " repeat in " + nums + " row！"
        return (false, error)
      case None =>
    }
    //head：取得第一个元素，也就是表头，样品名
    val headers = newBuffer.head.split("\t")
    val headersNoHead = headers.drop(1)
    val repeatElement = headersNoHead.diff(headersNoHead.distinct).distinct.headOption
    repeatElement match {
      case Some(x) => val nums = headers.zipWithIndex.filter(_._1 == x).map(_._2 + 1).mkString("(", "、", ")")
        //错误：样品名重复
        error = "Sample name " + x + " repeat in" + nums + " row！"
        return (false, error)
      case None =>
    }
    //表头的长度
    val headerSize = headers.size
    //1到newBuffer.size之间的范围，但不包括newBuffer.size
    for (i <- 1 until newBuffer.size) {
      //设置一个列值，取newBuffer的第i行去掉空格
      val columns = newBuffer(i).split("\t")
      //如果那一列的长度和表头长度不一样
      if (columns.size != headerSize) {
        //错误:第i+1列的数字是不正确的
        error = "The columns number in " + (i + 1) + " row is incorrect!"
        return (false, error)
      }
      for (j <- 1 until columns.size) {
        //循环第i列的值
        val value = columns(j)
        if (!Utils.isDouble(value)) {
          //这个值在i+1行，j+2列是个数字
          error = "The value  in " + (i + 1) + " row " + (j + 2) + " col must is number!"
          return (false, error)
        }
      }
    }
    (true, error)
  }

  def getAllSampleName: Action[AnyContent] = Action.async { implicit request =>
    mRNAProfileDao.selectAllSampleName.map { x =>
      Ok(Json.toJson(x))
    }
  }

  def getAllSample: Action[AnyContent] = Action.async { implicit request =>
    mRNAProfileDao.selectAllSampleName.map { x =>
      Ok(Json.toJson(x))
    }
  }

  def getAllGeneId: Action[AnyContent] = Action.async { implicit request =>
    geneIdDao.selectAllGeneId.map { x =>
      Ok(Json.toJson(x))
    }
  }

  def deletemRNABefore = Action {
    Ok(views.html.English.admin.deletemRNA())
  }

  //删除基因数据
  def deletemRNABySamplename(sampleName: String) = Action.async { implicit request =>
    mRNAProfileDao.deleteBySampleName(sampleName).map(y =>
      Redirect(routes.AdminController.deletemRNABefore())
    )
  }

}

