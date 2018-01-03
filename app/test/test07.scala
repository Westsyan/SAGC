package test

import java.io.File

import org.apache.commons.io.FileUtils
import org.apache.commons.math3.stat.correlation.PearsonsCorrelation
import utils.Utils

import scala.collection.JavaConverters._

object test07 {
  def main(args: Array[String]): Unit = {
    val buffer = FileUtils.readLines(new File(Utils.path + "column.txt")).asScala
    val gene = buffer.drop(1).map(_.split("\t"))
    val value = gene.map(x=>(x.head,x.drop(1).map(_.toDouble)))

    for(i <- 0 until gene.size;j <- 0 until gene.size){
      val x1 = value(i)._2
      val x2 = value(j)._2
      val c = new PearsonsCorrelation().correlation(x1,x2)
      if(i+j ==35679){
        println(i)
      }
    }
  }
}
