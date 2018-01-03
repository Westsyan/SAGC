package test


import java.io.File

import org.apache.commons.io.FileUtils
import utils.Utils


object test06 {

  def main(args: Array[String]): Unit = {

    var rStr = s"setwd('${Utils.path}')\n"
    rStr +=
      """
        |a <- read.table('rpca.txt', sep='\t', header=TRUE,fill=TRUE)
        |x < t(a)
        |pr <- prcomp(x)
        |prr <- pr$rotation
        |prs <- pr[,1:2]
        |write.table(prs,'result.txt',quote=F,sep='\t')
      """.stripMargin
    FileUtils.writeStringToFile(new File(Utils.path, "c.r"), rStr)

  }
}
