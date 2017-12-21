package test

object Test {
  def main(args: Array[String]): Unit = {
     val xmx = "1345 6789"
    val t1 = xmx.replaceAll("1","xxx")
    println(t1)

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
