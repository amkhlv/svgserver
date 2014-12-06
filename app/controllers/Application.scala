package controllers

import scala.language.postfixOps
import play.api._
import play.api.mvc._
import play.api.libs.iteratee._
import play.api.libs.concurrent.Execution.Implicits._
import java.io.{File, FileReader, FileWriter, BufferedReader}
import java.nio.file.{Paths, Files, StandardOpenOption, Path, StandardWatchEventKinds}
import scala.collection.JavaConversions._
import play.api.Play
import scala.sys.process._

object Application extends Controller {
  val separator = sys.props("line.separator")

  def readFromFilePath(filePath: Path): String =
    java.nio.file.Files.readAllLines(filePath, java.nio.charset.StandardCharsets.UTF_8).mkString(separator)

  def statusFile =
    Play.current.configuration.getString("svgserver.statusfile") match {
      case Some(s) => s
      case None => throw new RuntimeException("ERROR: have to define svgserver.statusfile in Configuration")
    }

  def svgRoot =
    Play.current.configuration.getString("svgserver.svgroot") match {
      case Some(s) => s
      case None => throw new RuntimeException("ERROR: have to define svgserver.svgroot in Configuration")
    }

  def isSecure:Boolean =
    Play.current.configuration.getString("svgserver.secure") match {
      case Some(s) => s.toBoolean
      case None => throw new RuntimeException("ERROR: have to define svgserver.secure in Configuration")
    }

  def index = Action {
    request => Ok(views.html.index(request))
  }

  def dropheader(x:String):String = {
    val lines = x  split("\\r?\\n")
    val firstSvgTag = """.*<svg""".r
    (lines.drop(3)).mkString(separator)
  }

  def readFromWS(s: String) = {
    val fw = new FileWriter(statusFile, true)
    try {
      fw.write(s + "\n")
    }
    finally fw.close()
  }

  def webSock = WebSocket.using[String] {
    request =>
      val in = Iteratee.foreach[String](readFromWS).map {
        x => {
          println("Disconnected");
        }
      }
      var needToSendFile = true;
      var previouslySent = ""
      val out = Enumerator.repeatM(
        scala.concurrent.Future {
          // Thread.sleep(2)
          // "current time %s".format((new java.util.Date()))
          val fsys = Paths.get(svgRoot).getFileSystem()
          val watcher = fsys.newWatchService()
          val myDir = Paths.get(svgRoot)
          myDir.register(watcher,
            StandardWatchEventKinds.ENTRY_CREATE,
            StandardWatchEventKinds.ENTRY_DELETE,
            StandardWatchEventKinds.ENTRY_MODIFY)
          println("Activating watcher")
          val watchKey = watcher.take()
          val event = watchKey.pollEvents().get(0)
          val evCont = event.context()
          val evKind = event.kind()
          watcher.close()
          println("Detected event in context: " + evCont + " of the kind: " + evKind)
          val changedFilePath = evCont match {
            case p: Path => p
            case _ => throw new RuntimeException("ERROR: event context not a path!")
          }
          val mimetype = ("file --mime-type -b " + svgRoot + "/" + evCont) !!
          val message = if (mimetype contains "image/svg+xml") {
            println("detected  " + mimetype)
            Thread.sleep(300)
            Process("cp " + svgRoot + "/" + evCont + " public/images/incoming.svg") run;
            val svgtext = readFromFilePath(Paths.get(svgRoot + "/" + evCont))
            if (needToSendFile) {
              needToSendFile = false
              previouslySent = svgtext
              "FILE\n" + svgtext
            }
            else {
              val dmp = new diff_match_patch
              val patch = dmp.patch_make(previouslySent, svgtext)
              val patchString = dmp.patch_toText(patch)
              previouslySent = svgtext
              "PTCH\n" + patchString
            }
          } else {
            println("going to ignore mimetype: " + mimetype)
            "unknown"
          }
          message
        }
      )
      (in, out)
  }
}

object Global extends GlobalSettings {
  override def onStop(app: Application) {

  }

}