package test

import java.io.File

import org.apache.commons.io.FileUtils

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Test {
  def main(args: Array[String]): Unit = {

    val buffer=mutable.Buffer(ArrayBuffer("a","b"),ArrayBuffer("c","d"))

    FileUtils.writeLines(new File("E:\\tmp.txt"),buffer.map(_.mkString(",")).asJava)


//   sampleNames.zipWithIndex.grouped(2).foreach{x=>
//     println("in")
//      val rows=buffer.drop(1).flatMap{y=>
//        val columns=y.split("\t")
//        val indexs=x.map(_._2)
//        indexs.map{index=>
//          MrnaprofileRow(columns(0), sampleNames(index), columns(index+1).toDouble)
//        }
//      }
//     val sortRows=rows.sortBy(_.samplename)


//    }
//    val dim2Buffer=buffer.map(_.split("\t").toBuffer).transpose
//    FileUtils.writeLines(new File("E:\\tmp.txt"),dim2Buffer.map(_.mkString("\t")).asJava)
//    println(dim2Buffer)

//    println(Utils.getTime(startTime))

  }

}
