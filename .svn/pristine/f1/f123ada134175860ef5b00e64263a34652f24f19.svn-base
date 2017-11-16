package dao

import javax.inject.Inject

import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfigProvider}
import slick.jdbc.JdbcProfile

import scala.concurrent.Future
import models.Tables._
import scala.concurrent.ExecutionContext.Implicits.global

class PasswordDao @Inject()(protected val dbConfigProvider: DatabaseConfigProvider) extends
  HasDatabaseConfigProvider[JdbcProfile] {

  import profile.api._

  def updatePassword(row: PasswordRow): Future[Unit] = {
    db.run(Password.filter(_.id === 1).update(row)).map(_ => ())
  }

  def selectPassword: Future[String] = {
    val q=Password.filter(_.id === 1).map(_.content).result
    val q1=Password.filter(_.id === 2).map(_.content).result
    q.flatMap(_=>q1)
    db.run(q.transactionally)
    db.run(Password.filter(_.id === 1).map(_.content).result.head)
  }

}
