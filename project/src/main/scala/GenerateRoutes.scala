package laughedelic.sbt.octokit

import upickle.default._
import java.io.File
import java.nio.file.{ Files, Paths }
import scala.collection.JavaConverters._

object Generator {

  // TODO: see https://github.com/octokit/rest.js/pull/732
  private val returnType = "Octokit.AnyResponse"

  def methodSignature(methodName: MethodName, methodType: MethodType): Lines = {
    Seq(
      Seq(s"def ${methodName}("),
      methodType.paramTypes.map { case (paramName, param) =>
        val default: String = if (param.required) "" else " = js.undefined"
        s"  ${paramName}: ${param.scalaType}${default},"
      },
      Seq(
        s"  headers: js.UndefOr[Octokit.Headers] = ${methodType.defaultHeaders}",
        s"): Future[${returnType}] = ",
      ),
    ).flatten
  }

  def methodBody(
    namespace: Namespace,
    methodName: MethodName,
    methodType: MethodType
  ): Lines = {
    Seq(
      Seq(
        s"octokitJS.${namespace}.${methodName}(",
        "  js.Dynamic.literal(",
      ),
      methodType.paramTypes.map { case (paramName, param) =>
        val paramValue: String =
          if (param.scalaType.contains('|'))
            s"$paramName.asInstanceOf[js.Any]"
          else paramName
        s"""    "${paramName}" -> ${paramValue}.asInstanceOf[js.Any],"""
      },
      Seq(
        s"""    "headers" -> headers.asInstanceOf[js.Any]""",
         "  )",
        s").asInstanceOf[js.Promise[${returnType}]].toFuture",
      )
    ).flatten
  }

  def methodDefinition(
    namespace: Namespace,
    methodName: MethodName,
    methodType: MethodType
  ): Lines = Seq(
    // scaladoc(methodDoc),
    methodSignature(methodName, methodType),
    methodBody(namespace, methodName, methodType).indent(),
  ).flatten

  def template(content: Lines): Lines = Seq(
    Seq(
      "package laughedelic.octokit.rest", "",
      "import scala.scalajs.js, js.|",
      "import scala.concurrent.Future", "",
      "class OctokitGeneratedRoutes(private val octokitJS: js.Dynamic) {",
    ),
    content.indent(),
    Seq("", "}"),
  ).flatten

  def routeObjects(types: RoutesTypes): Lines = {
    types.groupBy(_.scope).toSeq.flatMap { case (namespace, methods) =>
        Seq(
          Seq("", s"object ${namespace} {"),
          methods.map(methodType => {
            "" +: methodDefinition(namespace, methodType.id, methodType)
          }).flatten.indent(),
          Seq("}")
        ).flatten
      }
  }

  def generateRoutes(routes: RoutesTypes): Lines =
    template(routeObjects(routes))

  def main(args: Array[String]): Unit = {
    // val routesDocs  = ParseRoutesDocs()
    val routesTypes = ParseRoutesTypes(new File("routes.json"))

    args.toList match {
      case Nil => {
        Files.write(
          Paths.get("target/routes.scala"),
          generateRoutes(routesTypes).asJava
        )
      }
      case List("types") => {
        val types = for {
          (nmspcN, nmspc) <- routesTypes.groupBy(_.scope).toSeq
          mtd <- nmspc
          (paramN, param) <- mtd.paramTypes.toSeq
          if (param.tpe.startsWith("object"))
        } yield {
          s"${nmspcN}.${mtd.id}.${paramN}${param.tpe.stripPrefix("object")}"
        }
        types.sorted.foreach(println)
      }
      case namespace :: methodName :: _ => {
        val methodType = routesTypes.find(x => x.scope == namespace && x.id == methodName).get
        println(
          methodDefinition(namespace, methodName, methodType).mkString("\n")
        )
      }
    }
  }
}
