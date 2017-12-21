package test

import java.io.File

import org.apache.commons.io.FileUtils
import utils.Utils
import scala.collection.JavaConverters._

object test06 {

  def main(args: Array[String]): Unit = {
    val buffer = FileUtils.readLines(new File(Utils.path + "out_matrix.txt")).asScala
    val buf = buffer.map(_.split("\t")).toArray
    val head = buffer.head.split("\t")
/*    buf.zipWithIndex.map { x =>
      val row = buffer.drop(1).map { y =>
        val index = x._2
        println(index)
        val columns = y.split("\t")
        (columns(0), head(index),columns(index+1).toDouble)
      }
      println(row)
    }*/
    for (i <- 0 until head.size;j <- i + 1 until head.size) {
        println(buf(0)(i), buf(0)(j), buf(i + 1)(j + 1))
    }

  }
}
