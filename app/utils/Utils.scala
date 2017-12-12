package utils

import java.io.File

import org.apache.commons.io.FileUtils

/**
  * Created by yz on 2017/6/16.
  */
object Utils {
  
  def getTime(startTime: Long) = {
    val endTime = System.currentTimeMillis()
    (endTime - startTime) / 1000.0
  }

  def deleteDirectory(direcotry: File) = {
    try {
      FileUtils.deleteDirectory(direcotry)
    } catch {
      case _ =>
    }
  }

  def isDouble(value: String): Boolean = {
    try {
      value.toDouble
    } catch {
      case _: Exception =>
        return false
    }
    true
  }

  val path = "E:\\data\\"
  val tmpFile = new File(path, "tmp.txt")
}
