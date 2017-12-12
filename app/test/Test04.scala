package test

import scala.sys.process.Process


object Test04 {

  def main(args: Array[String]): Unit = {

    val study = "D:\\file\\in_gene.txt"
    val population = "D:\\file\\all_gene.txt"
    val association = "D:\\file\\ref_gene.Go.txt"
    val alpha = "0.05"
    val output = "E:\\perl\\GO_enrichment.txt"
    val pval = "0.05"
    val x1 = Process("python E:\\perl\\goatools-0.5.7\\scripts\\find_enrichment.py --alpha="+ alpha + " --pval=" + pval +
                      " --output " + output + " "+ study + " "+ population + " "+ association).!
    print(x1)
  }

}
