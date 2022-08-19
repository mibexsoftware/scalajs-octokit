package laughedelic.sbt.octokit

import upickle.default._
import upickle.default.{Reader => R, macroR}
import java.io.File

case class MethodType(
  name: String,
  id: MethodName,
  scope: String,
  method: String,
  parameters: Seq[ParamType]
) {
  def paramTypes: Map[ParamName, ParamType] = parameters.collect {
    // FIXME: handle sub-parameters instead of filtering them out
    case pt if !pt.name.contains('.') && pt.alias == null && pt.name != "*" && pt.tpe != null =>
      escapeScalaIdent(pt.name) -> pt
  }.toMap
  def defaultHeaders: String =  "js.undefined"
}

object MethodType { implicit def r: R[MethodType] = macroR }

trait Param
object Param {
  implicit def r: R[Param] = eitherOf(
    reader[ParamType],
    reader[ParamAlias],
  )
}

case class ParamType(
  @upickle.implicits.key("type") tpe: String,
  name: ParamName,
  alias: String,
  required: Boolean = false,
  allowNull: Boolean = false,
) extends Param {
  def scalaType: String = {
    val t = jsTypeToScala(tpe)
    val tOrNull = if(allowNull) s"${t} | Null" else t
    if (required) tOrNull else s"js.UndefOr[${tOrNull}]"
  }
}
object ParamType { implicit def r: R[ParamType] = macroR }

// Aliases are used for deprecated parameters, so we just ignore them
case class ParamAlias(
  alias: String,
  // deprecated: Either[String, Boolean] = "", // this doesn't work, but it doesn't matter if we skip it anyway
) extends Param
object ParamAlias { implicit def r: R[ParamAlias] = macroR }



object ParseRoutesTypes {
  def apply(file: File): RoutesTypes =  {
     read[RoutesTypes](file)
      .filter(m => !(m.name.startsWith("Download a repository archive ") && m.id.equals("downloadArchive")))
  }
    
  def apply(): RoutesTypes = apply(new File("routes.json"))
}
