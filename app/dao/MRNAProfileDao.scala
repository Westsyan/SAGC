package dao

import javax.inject.Inject

import models.Tables
import models.Tables._
import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfigProvider}
import slick.jdbc.JdbcProfile

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global



class MRNAProfileDao @Inject()(protected val dbConfigProvider: DatabaseConfigProvider) extends
  HasDatabaseConfigProvider[JdbcProfile] {

  import profile.api._

  def insertAll(rows: Seq[MrnaprofileRow]): Future[Unit] = db.run(Mrnaprofile ++= rows).map(_ => ())

  def insertOrUpdate(row: MrnaprofileRow): Future[Unit] = db.run(Mrnaprofile
    .insertOrUpdate(row)).map(_ => ())

  def deleteAll: Future[Unit] = db.run(Mrnaprofile.delete).map(_ => ())


  def selectAll:Future[Seq[MrnaprofileRow]] = {db.run(Mrnaprofile.result)}

  def selectAllGeneId(): Future[Seq[String]] = db.run(Mrnaprofile.map(_.geneid).distinct.result)

  def selectAllSampleName(): Future[Seq[String]] = db.run(Mrnaprofile.map(_.samplename).distinct.result)

  def selectByGeneId(id: String): Future[Seq[Tables.MrnaprofileRow]] = {
    val geneId = id.split(",").map(_.trim).distinct
    db.run(Mrnaprofile.filter(_.geneid.inSetBind(geneId)).result)
  }

  def selectByPosition(id:String , sampleName:String): Future[Seq[Tables.MrnaprofileRow]]={
    val geneId = id.split(",").map(_.trim).distinct
    val sample = sampleName.split(",").map(_.trim).distinct
    db.run(Mrnaprofile.filter(_.samplename.inSetBind(sample)).filter(_.geneid.inSetBind(geneId)).result)
  }

  def selectAllBySampleName(sampleName: String) : Future[Seq[Tables.MrnaprofileRow]]={
    val sample = sampleName.split(",").map(_.trim).distinct
    db.run(Mrnaprofile.filter(_.samplename.inSetBind(sample)).result)
  }

  def selectValueByPosition(id:String , sampleName:String): Future[Seq[Double]]={
    val geneId = id.split(",").map(_.trim).distinct
    val sample = sampleName.split(",").map(_.trim).distinct
    db.run(Mrnaprofile.filter(_.samplename.inSetBind(sample)).filter(_.geneid.inSetBind(geneId)).map(_.value).result)
  }


  def selectBySampleName(sampleName: String): Future[Seq[String]] = {
    val samples = sampleName.split(",").map(_.trim).distinct
    db.run(Mrnaprofile.filter(_.samplename.inSetBind(samples)).map(_.samplename).distinct.result)
  }

  def selectValueByGeneid(geneId : String) : Future[Seq[Double]] = {
    db.run(Mrnaprofile.filter(_.geneid === geneId).map(_.value).result)
  }


  def deleteBySampleName(sampleName: String): Future[Unit] = {
    val samples = sampleName.split(",").map(_.trim).distinct
    db.run(Mrnaprofile.filter(_.samplename.inSetBind(samples)).delete).map(_ => ())
  }
}