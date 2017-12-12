package test

import java.io.File

import org.apache.commons.io.FileUtils
import scala.collection.JavaConverters._

object Test02 {
  def main(args: Array[String]): Unit = {

    val buffer = FileUtils.readLines(new File("D:\\file\\ref_genome.txt")).asScala.distinct
    val toBuffer01 = buffer.filter(_.split("\t")(2) == "mRNA")
    val toBuffer02 = buffer.filter(_.split("\t")(2) == "transcript")
    val toBuffer = toBuffer01 ++ toBuffer02
    val geneId = toBuffer.map(_.split("\t")(8).split(";")(1).split("=")(1))
    val DNA = toBuffer.map(_.split("\t")(0))
    val aMap =toBuffer.map{ x=>
      val bu = x.split("\t")
      (bu(0),bu(2),bu(3),bu(4),bu(8).split(";")(1).split("=")(1))
    }
    val rows = geneId.map{ x=>
      val DNA = aMap.filter(_._5 == x).map(_._1).distinct.mkString(",")
      val max = aMap.filter(_._5 == x).map{x=>
        (x._3,x._4,x._4.toInt - x._3.toInt)
    }
    val start = max.filter(_._3 == max.map(_._3).max).map(_._1)
    val end = max.filter(_._3 == max.map(_._3).max).map(_._2)
    }
    println(geneId.distinct.size)
  }
}
