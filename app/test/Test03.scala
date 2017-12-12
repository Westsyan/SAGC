package test

import java.io.File

import org.apache.commons.io.FileUtils

import scala.collection.JavaConverters._

object Test03 {

  def main(args: Array[String]): Unit = {
    val pepBuffer = FileUtils.readLines(new File("D:\\file\\pep.txt")).asScala

    val pepBuf = pepBuffer.toString.split(">")

    val pepMap = pepBuf.map { x =>
      val geneId = x.split(",")(0).split("gene:").slice(1,2).mkString.split(" ")(0)
      val cds = x.split(",").drop(1).map(_.trim).mkString
      (geneId,cds)
    }
    val pepId = pepMap.map(_._1).drop(1).distinct.toBuffer
    FileUtils.writeLines(new File("all_gene.txt"),pepId.asJava)
  }
}
