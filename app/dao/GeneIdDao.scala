package dao

import javax.inject.Inject

import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfigProvider}
import slick.jdbc.JdbcProfile
import models.Tables._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class GeneIdDao @Inject()(protected val dbConfigProvider: DatabaseConfigProvider) extends
  HasDatabaseConfigProvider[JdbcProfile] {

  import profile.api._

  def insertAll(rows: Seq[GeneidRow]): Future[Unit] = db.run(Geneid ++=
    rows).map(_ => ())


  def selectAllGeneId : Future[Seq[String]] = db.run(Geneid.map(_.id).distinct.result)

  def selectAll: Future[Seq[GeneidRow]] = {db.run(Geneid.result)}

  def selectById(geneId : String) :Future[Seq[String]] = {
    val id = geneId.split(",").map(_.trim).distinct
    db.run(Geneid.filter(_.id.inSetBind(id)).map(_.id).result)
  }

  def deleteAll: Future[Unit] = db.run(Geneid.delete).map(_ => ())

}
