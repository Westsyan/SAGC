package dao

import javax.inject.Inject

import models.Tables._
import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfigProvider}
import slick.jdbc.JdbcProfile

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class CorrelationDao @Inject()(protected val dbConfigProvider: DatabaseConfigProvider) extends
  HasDatabaseConfigProvider[JdbcProfile] {

  import profile.api._

  def insertAll(rows: Seq[CorrelationRow]): Future[Unit] = db.run(Correlation ++=
    rows).map(_ => ())

  def selectByGeneid(geneid:String) : Future[Seq[CorrelationRow]] = {
    val id = geneid.split(",").map(_.trim).distinct
    db.run(Correlation.filter(x=> x.gene1.inSetBind(id)).result)
  }

  def selectById(geneid:String) : Future[Seq[String]] = {
    val id = geneid.split(",").map(_.trim).distinct
    db.run(Correlation.filter(x=> x.gene1.inSetBind(id)).map(_.gene1).distinct.result)
  }

  def selectAllGene : Future[Seq[String]] = db.run(Correlation.map(_.gene1).distinct.result)

}
