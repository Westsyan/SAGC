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
  lazy val schema: profile.SchemaDescription = Correlation.schema ++ Geneid.schema ++ Geneinformation.schema ++ Mrnaprofile.schema ++ Password.schema
  @deprecated("Use .schema instead of .ddl", "3.0")
  def ddl = schema

  /** Entity class storing rows of table Correlation
   *  @param gene1 Database column gene1 SqlType(VARCHAR), Length(255,true)
   *  @param gene2 Database column gene2 SqlType(VARCHAR), Length(255,true)
   *  @param correlation Database column correlation SqlType(DOUBLE) */
  final case class CorrelationRow(gene1: String, gene2: String, correlation: Double)
  /** GetResult implicit for fetching CorrelationRow objects using plain SQL queries */
  implicit def GetResultCorrelationRow(implicit e0: GR[String], e1: GR[Double]): GR[CorrelationRow] = GR{
    prs => import prs._
    CorrelationRow.tupled((<<[String], <<[String], <<[Double]))
  }
  /** Table description of table correlation. Objects of this class serve as prototypes for rows in queries. */
  class Correlation(_tableTag: Tag) extends profile.api.Table[CorrelationRow](_tableTag, Some("sagc"), "correlation") {
    def * = (gene1, gene2, correlation) <> (CorrelationRow.tupled, CorrelationRow.unapply)
    /** Maps whole row to an option. Useful for outer joins. */
    def ? = (Rep.Some(gene1), Rep.Some(gene2), Rep.Some(correlation)).shaped.<>({r=>import r._; _1.map(_=> CorrelationRow.tupled((_1.get, _2.get, _3.get)))}, (_:Any) =>  throw new Exception("Inserting into ? projection not supported."))

    /** Database column gene1 SqlType(VARCHAR), Length(255,true) */
    val gene1: Rep[String] = column[String]("gene1", O.Length(255,varying=true))
    /** Database column gene2 SqlType(VARCHAR), Length(255,true) */
    val gene2: Rep[String] = column[String]("gene2", O.Length(255,varying=true))
    /** Database column correlation SqlType(DOUBLE) */
    val correlation: Rep[Double] = column[Double]("correlation")

    /** Primary key of Correlation (database name correlation_PK) */
    val pk = primaryKey("correlation_PK", (gene1, gene2))
  }
  /** Collection-like TableQuery object for table Correlation */
  lazy val Correlation = new TableQuery(tag => new Correlation(tag))

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

  /** Entity class storing rows of table Geneinformation
   *  @param geneId Database column Gene_ID SqlType(VARCHAR), PrimaryKey, Length(255,true)
   *  @param geneName Database column Gene_Name SqlType(TEXT)
   *  @param chromosome Database column Chromosome SqlType(INT)
   *  @param geneStart Database column Gene_start SqlType(INT)
   *  @param geneEnd Database column Gene_end SqlType(INT)
   *  @param strand Database column Strand SqlType(TEXT)
   *  @param func Database column FUNC SqlType(TEXT)
   *  @param go Database column GO SqlType(TEXT)
   *  @param kegg Database column KEGG SqlType(VARCHAR), Length(255,true)
   *  @param iprId Database column IPR_Id SqlType(TEXT)
   *  @param iprDescrip Database column IPR_Descrip SqlType(TEXT)
   *  @param cdna Database column cDNA SqlType(TEXT)
   *  @param cds Database column CDS SqlType(TEXT)
   *  @param pep Database column PEP SqlType(TEXT) */
  final case class GeneinformationRow(geneId: String, geneName: String, chromosome: Int, geneStart: Int, geneEnd: Int, strand: String, func: String, go: String, kegg: String, iprId: String, iprDescrip: String, cdna: String, cds: String, pep: String)
  /** GetResult implicit for fetching GeneinformationRow objects using plain SQL queries */
  implicit def GetResultGeneinformationRow(implicit e0: GR[String], e1: GR[Int]): GR[GeneinformationRow] = GR{
    prs => import prs._
    GeneinformationRow.tupled((<<[String], <<[String], <<[Int], <<[Int], <<[Int], <<[String], <<[String], <<[String], <<[String], <<[String], <<[String], <<[String], <<[String], <<[String]))
  }
  /** Table description of table geneinformation. Objects of this class serve as prototypes for rows in queries. */
  class Geneinformation(_tableTag: Tag) extends profile.api.Table[GeneinformationRow](_tableTag, Some("sagc"), "geneinformation") {
    def * = (geneId, geneName, chromosome, geneStart, geneEnd, strand, func, go, kegg, iprId, iprDescrip, cdna, cds, pep) <> (GeneinformationRow.tupled, GeneinformationRow.unapply)
    /** Maps whole row to an option. Useful for outer joins. */
    def ? = (Rep.Some(geneId), Rep.Some(geneName), Rep.Some(chromosome), Rep.Some(geneStart), Rep.Some(geneEnd), Rep.Some(strand), Rep.Some(func), Rep.Some(go), Rep.Some(kegg), Rep.Some(iprId), Rep.Some(iprDescrip), Rep.Some(cdna), Rep.Some(cds), Rep.Some(pep)).shaped.<>({r=>import r._; _1.map(_=> GeneinformationRow.tupled((_1.get, _2.get, _3.get, _4.get, _5.get, _6.get, _7.get, _8.get, _9.get, _10.get, _11.get, _12.get, _13.get, _14.get)))}, (_:Any) =>  throw new Exception("Inserting into ? projection not supported."))

    /** Database column Gene_ID SqlType(VARCHAR), PrimaryKey, Length(255,true) */
    val geneId: Rep[String] = column[String]("Gene_ID", O.PrimaryKey, O.Length(255,varying=true))
    /** Database column Gene_Name SqlType(TEXT) */
    val geneName: Rep[String] = column[String]("Gene_Name")
    /** Database column Chromosome SqlType(INT) */
    val chromosome: Rep[Int] = column[Int]("Chromosome")
    /** Database column Gene_start SqlType(INT) */
    val geneStart: Rep[Int] = column[Int]("Gene_start")
    /** Database column Gene_end SqlType(INT) */
    val geneEnd: Rep[Int] = column[Int]("Gene_end")
    /** Database column Strand SqlType(TEXT) */
    val strand: Rep[String] = column[String]("Strand")
    /** Database column FUNC SqlType(TEXT) */
    val func: Rep[String] = column[String]("FUNC")
    /** Database column GO SqlType(TEXT) */
    val go: Rep[String] = column[String]("GO")
    /** Database column KEGG SqlType(VARCHAR), Length(255,true) */
    val kegg: Rep[String] = column[String]("KEGG", O.Length(255,varying=true))
    /** Database column IPR_Id SqlType(TEXT) */
    val iprId: Rep[String] = column[String]("IPR_Id")
    /** Database column IPR_Descrip SqlType(TEXT) */
    val iprDescrip: Rep[String] = column[String]("IPR_Descrip")
    /** Database column cDNA SqlType(TEXT) */
    val cdna: Rep[String] = column[String]("cDNA")
    /** Database column CDS SqlType(TEXT) */
    val cds: Rep[String] = column[String]("CDS")
    /** Database column PEP SqlType(TEXT) */
    val pep: Rep[String] = column[String]("PEP")
  }
  /** Collection-like TableQuery object for table Geneinformation */
  lazy val Geneinformation = new TableQuery(tag => new Geneinformation(tag))

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
