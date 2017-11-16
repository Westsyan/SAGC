package models
// AUTO-GENERATED Slick data model
/** Stand-alone Slick data model for immediate use */
object Tables extends {
  val profile = slick.jdbc.MySQLProfile
} with Tables

/** Slick data model trait for extension, choice of backend or usage in the cake pattern. (Make sure to initialize this late.) */
trait Tables {
  val profile: slick.jdbc.JdbcProfile
  import profile.api._
  import com.github.tototoshi.slick.MySQLJodaSupport._
  import org.joda.time.DateTime
  import slick.model.ForeignKeyAction
  // NOTE: GetResult mappers for plain SQL are only generated for tables where Slick knows how to map the types of all columns.
  import slick.jdbc.{GetResult => GR}

  /** DDL for all tables. Call .create to execute. */
  lazy val schema: profile.SchemaDescription = Geneid.schema ++ Mrnaprofile.schema ++ Password.schema
  @deprecated("Use .schema instead of .ddl", "3.0")
  def ddl = schema

  /** Entity class storing rows of table Geneid
   *  @param id Database column id SqlType(VARCHAR), PrimaryKey, Length(255,true) */
  final case class GeneidRow(id: String)
  /** GetResult implicit for fetching GeneidRow objects using plain SQL queries */
  implicit def GetResultGeneidRow(implicit e0: GR[String]): GR[GeneidRow] = GR{
    prs => import prs._
    GeneidRow(<<[String])
  }
  /** Table description of table geneid. Objects of this class serve as prototypes for rows in queries. */
  class Geneid(_tableTag: Tag) extends profile.api.Table[GeneidRow](_tableTag, Some("sagc"), "geneid") {
    def * = id <> (GeneidRow, GeneidRow.unapply)
    /** Maps whole row to an option. Useful for outer joins. */
    def ? = Rep.Some(id).shaped.<>(r => r.map(_=> GeneidRow(r.get)), (_:Any) =>  throw new Exception("Inserting into ? projection not supported."))

    /** Database column id SqlType(VARCHAR), PrimaryKey, Length(255,true) */
    val id: Rep[String] = column[String]("id", O.PrimaryKey, O.Length(255,varying=true))
  }
  /** Collection-like TableQuery object for table Geneid */
  lazy val Geneid = new TableQuery(tag => new Geneid(tag))

  /** Entity class storing rows of table Mrnaprofile
   *  @param geneid Database column geneId SqlType(VARCHAR), Length(255,true)
   *  @param samplename Database column sampleName SqlType(VARCHAR), Length(255,true)
   *  @param value Database column value SqlType(DOUBLE) */
  final case class MrnaprofileRow(geneid: String, samplename: String, value: Double)
  /** GetResult implicit for fetching MrnaprofileRow objects using plain SQL queries */
  implicit def GetResultMrnaprofileRow(implicit e0: GR[String], e1: GR[Double]): GR[MrnaprofileRow] = GR{
    prs => import prs._
    MrnaprofileRow.tupled((<<[String], <<[String], <<[Double]))
  }
  /** Table description of table mrnaprofile. Objects of this class serve as prototypes for rows in queries. */
  class Mrnaprofile(_tableTag: Tag) extends profile.api.Table[MrnaprofileRow](_tableTag, Some("sagc"), "mrnaprofile") {
    def * = (geneid, samplename, value) <> (MrnaprofileRow.tupled, MrnaprofileRow.unapply)
    /** Maps whole row to an option. Useful for outer joins. */
    def ? = (Rep.Some(geneid), Rep.Some(samplename), Rep.Some(value)).shaped.<>({r=>import r._; _1.map(_=> MrnaprofileRow.tupled((_1.get, _2.get, _3.get)))}, (_:Any) =>  throw new Exception("Inserting into ? projection not supported."))

    /** Database column geneId SqlType(VARCHAR), Length(255,true) */
    val geneid: Rep[String] = column[String]("geneId", O.Length(255,varying=true))
    /** Database column sampleName SqlType(VARCHAR), Length(255,true) */
    val samplename: Rep[String] = column[String]("sampleName", O.Length(255,varying=true))
    /** Database column value SqlType(DOUBLE) */
    val value: Rep[Double] = column[Double]("value")

    /** Primary key of Mrnaprofile (database name mrnaprofile_PK) */
    val pk = primaryKey("mrnaprofile_PK", (geneid, samplename))
  }
  /** Collection-like TableQuery object for table Mrnaprofile */
  lazy val Mrnaprofile = new TableQuery(tag => new Mrnaprofile(tag))

  /** Entity class storing rows of table Password
   *  @param id Database column id SqlType(INT), PrimaryKey
   *  @param content Database column content SqlType(VARCHAR), Length(255,true) */
  final case class PasswordRow(id: Int, content: String)
  /** GetResult implicit for fetching PasswordRow objects using plain SQL queries */
  implicit def GetResultPasswordRow(implicit e0: GR[Int], e1: GR[String]): GR[PasswordRow] = GR{
    prs => import prs._
    PasswordRow.tupled((<<[Int], <<[String]))
  }
  /** Table description of table password. Objects of this class serve as prototypes for rows in queries. */
  class Password(_tableTag: Tag) extends profile.api.Table[PasswordRow](_tableTag, Some("sagc"), "password") {
    def * = (id, content) <> (PasswordRow.tupled, PasswordRow.unapply)
    /** Maps whole row to an option. Useful for outer joins. */
    def ? = (Rep.Some(id), Rep.Some(content)).shaped.<>({r=>import r._; _1.map(_=> PasswordRow.tupled((_1.get, _2.get)))}, (_:Any) =>  throw new Exception("Inserting into ? projection not supported."))

    /** Database column id SqlType(INT), PrimaryKey */
    val id: Rep[Int] = column[Int]("id", O.PrimaryKey)
    /** Database column content SqlType(VARCHAR), Length(255,true) */
    val content: Rep[String] = column[String]("content", O.Length(255,varying=true))
  }
  /** Collection-like TableQuery object for table Password */
  lazy val Password = new TableQuery(tag => new Password(tag))
}
