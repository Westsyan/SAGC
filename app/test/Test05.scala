package test

import scala.sys.process.Process

object Test05 {
  def main(args: Array[String]): Unit = {

    val study = "D:\\file\\in_gene.txt"
    val population = "D:\\file\\all_gene.txt"
    val association = "D:\\file\\ref_gene.pathway.txt"
    val m = "b"
    val n = "QVALUE"
    val o = "E:\\perl\\KEGG_enrichment.txt"
    val c= "5"
    val maxp = "1"

    val x1 = Process("perl E:\\perl\\identify.pl -study="+study+" -population="+population+" -association="+association+
      " -m="+m+" -n="+n+" -o="+o+" -c="+c+" -maxp="+maxp).!
    print(x1)
  }

}
