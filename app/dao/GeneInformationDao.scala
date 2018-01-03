package dao

import javax.inject.Inject

import controllers.{RegionData, SampleRegionData}
import models.Tables
import models.Tables._
import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfigProvider}
import slick.jdbc.JdbcProfile

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future


class GeneInformationDao @Inject()(protected val dbConfigProvider: DatabaseConfigProvider) extends
  HasDatabaseConfigProvider[JdbcProfile] {
  import profile.api._

  def deleteAll: Future[Unit] = db.run(Geneinformation.delete).map(_ => ())

  def insert(rows : Seq[GeneinformationRow]) : Future[Unit] = db.run(Geneinformation ++= rows).map(_ =>())

  def selectById(id : String) : Future[Seq[Tables.GeneinformationRow]] = {
    val geneId = id.split(",").map(_.trim).distinct
    db.run(Geneinformation.filter(_.geneId.inSetBind(geneId)).result)
  }

  def selectByRegion(data: RegionData) : Future[Seq[Tables.GeneinformationRow]] = {
    db.run(Geneinformation.filter(_.chromosome === data.chr).filter(_.geneStart >= data.start).
      filter(_.geneEnd <= data.end).result)
  }

  def selectByCRegion(data: RegionData) : Future[Seq[Tables.GeneinformationRow]] = {
    db.run(Geneinformation.filter(_.chromosome === data.chr).filter(_.geneStart >= data.start).
      filter(_.geneEnd <= data.end).result)
  }

  def selectBySRegion(data: SampleRegionData) : Future[Seq[String]] = {
    db.run(Geneinformation.filter(_.chromosome === data.chr).filter(_.geneStart >= data.start).
      filter(_.geneEnd <= data.end).map(_.geneId).result)
  }

  def allChr: Future[Seq[Int]] = {db.run(Geneinformation.map(_.chromosome).distinct.result)}

}
