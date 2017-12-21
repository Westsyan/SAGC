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
    db.run(Correlation.filter(x=> x.gene1.inSetBind(id) || x.gene2.inSetBind(id)).result)
  }

}
