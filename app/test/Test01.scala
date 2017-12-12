package test

import java.io.File

import org.apache.commons.io.FileUtils

import scala.collection.JavaConverters._


object Func  {

  def main(args: Array[String]): Unit = {

    val funcBuffer=FileUtils.readLines(new File("D:\\file\\ref_gene.func.txt")).asScala.distinct
    val keggBuffer=FileUtils.readLines(new File("D:\\file\\ref_gene.pathway.txt")).asScala.distinct
    val goBuffer=FileUtils.readLines(new File("D:\\file\\ref_gene.Go.txt")).asScala.distinct

    val funcToBuffer = funcBuffer.map(_.split("\t"))
    val geneIds = funcToBuffer.map(x=>x(0)).distinct
    val funcGeneId = funcToBuffer.map(x=>x(0))
    val fMap = funcToBuffer.map(x=>(x(0),x(1)))
    val mGeneId = funcGeneId.diff(geneIds)

    val keegToBuffer = keggBuffer.map(_.split("\t"))
    val kMap = keegToBuffer.map(x=> (x(0),x(1)))
    val keggId = keegToBuffer.map(x=> x(0))

    val goToBuffer = keggBuffer.map(_.split("\t"))
    val gMap = keegToBuffer.map(x=> (x(0),x(1)))
    val goId = goToBuffer.map(x=>x(0))

    println(1)
    val rows =geneIds.map{ x=>
      var func = ""
       if(mGeneId.contains(x)) {
         val func1 = fMap.filter(_._1 == x).map(_._2).mkString("//")
         func = func1
       }else{ func = "-" }
          val keeg1 = kMap.filter(_._1 == x).map(_._2).mkString
          var kegg = ""
          if(keggId.contains(x)) {kegg=keeg1 }else{ kegg = "-"}
          val go1 = gMap.filter(_._1 == x).map(_._2).mkString
          var go =""
          if(goId.contains(x)){ go = go1 }else{ go = "-" }

    }
  }
}
