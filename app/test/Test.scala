package test

import java.io.File
import java.nio.file.{Files, Paths}

import org.apache.commons.io.FileUtils
import utils.Utils
import models.Tables._

import collection.JavaConverters._

object Test {
  def main(args: Array[String]): Unit = {

    val startTime=System.currentTimeMillis()
   val buffer=FileUtils.readLines(new File("D:\\gene.fpkm.mat.txt")).asScala
    val sampleNames=buffer.head.split("\t").drop(1)

   sampleNames.zipWithIndex.grouped(2).foreach{x=>
     println("in")
      val rows=buffer.drop(1).flatMap{y=>
        val columns=y.split("\t")
        val indexs=x.map(_._2)
        indexs.map{index=>
          MrnaprofileRow(columns(0), sampleNames(index), columns(index+1).toDouble)
        }
      }
     val sortRows=rows.sortBy(_.samplename)


    }
//    val dim2Buffer=buffer.map(_.split("\t").toBuffer).transpose
//    FileUtils.writeLines(new File("E:\\tmp.txt"),dim2Buffer.map(_.mkString("\t")).asJava)
//    println(dim2Buffer)

    println(Utils.getTime(startTime))

  }

}
