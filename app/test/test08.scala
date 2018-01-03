package test

import java.io.File
import java.text.SimpleDateFormat
import java.util.Date

import org.apache.commons.io.FileUtils
import utils.Utils


object test08 {


  def main(args: Array[String]): Unit = {
    def NowDate= {
      val now: Date = new Date()
      val dateFormat: SimpleDateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
      val date = dateFormat.format(now)
      date
    }
    println(NowDate)

    FileUtils.writeStringToFile(new File(Utils.path,"time.txt"),NowDate)

  }
}
