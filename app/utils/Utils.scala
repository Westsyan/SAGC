package utils

import java.io.{File, FileInputStream}
import java.text.SimpleDateFormat

import play.api.data.Form
import play.api.data.Forms._
import play.api.libs.json.Json
import models.Tables._
import org.apache.commons.io.FileUtils

import scala.collection.mutable.ArrayBuffer
import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.sys.process.Process

/**
  * Created by yz on 2017/6/16.
  */
object Utils {

  val array = Array[String]("N1", "N2", "N3", "N4", "N5", "N6", "N7", "N8", "N9", "N10", "N11", "N12", "N13", "N14",
    "N15", "N16", "N17", "N18", "N19", "N20", "N21", "N22", "N23", "N24", "N25", "N26", "N27", "N28", "N29", "N30",
    "N31", "N32", "CHD1", "CHD2", "CHD3", "CHD4", "CHD5", "CHD6", "CRC1", "CRC2", "CRC3", "CRC4", "CRC5", "CRC6",
    "CRC7", "CRC8", "CRC9", "CRC10", "CRC11", "CRC12", "HCC1", "HCC2", "HCC3", "HCC4", "HCC5", "HCC6", "HCC7", "HCC8",
    "HCC9", "HCC10", "HCC11", "HCC12", "HCC13", "HCC14", "HCC15", "HCC16", "HCC17", "HCC18", "HCC19", "HCC20", "HCC21",
    "PAAD1", "PAAD2", "PAAD3", "PAAD4", "PAAD5", "PAAD6", "PAAD7", "PAAD8", "PAAD9", "PAAD10", "PAAD11", "PAAD12",
    "PAAD13", "PAAD14", "WhB1", "WhB2", "WhB3", "WhB4", "WhB5")



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
