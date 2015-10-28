package controllers

import java.io.{File, FileWriter}
import java.nio.file.{Path, Paths, StandardWatchEventKinds}
import java.security.MessageDigest

import play.api.{Play, _}
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.iteratee._
import play.api.mvc._

import scala.collection.JavaConversions._
import scala.language.postfixOps
import scala.sys.process._


object Application extends Controller {
  val separator = sys.props("line.separator")

  def readFromFilePath(filePath: Path): String =
    java.nio.file.Files.readAllLines(filePath, java.nio.charset.StandardCharsets.UTF_8).mkString(separator)

  def writeToFile(p: String, s: String): Unit = {
    val pw = new java.io.PrintWriter(new File(p))
    try pw.write(s) finally pw.close()
  }

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
  val fsys = Paths.get(svgRoot).getFileSystem()
  val watcher = fsys.newWatchService()
  val myDir = Paths.get(svgRoot)
  myDir.register(watcher,
        StandardWatchEventKinds.ENTRY_CREATE,
        StandardWatchEventKinds.ENTRY_DELETE,
        StandardWatchEventKinds.ENTRY_MODIFY)

  def md5(s: String) = {
    MessageDigest.getInstance("MD5").digest(s.getBytes).map("%02X".format(_)).mkString
}

  def webSock = WebSocket.using[String] {
    request =>
      println("Processing request")
      val in = Iteratee.foreach[String](readFromWS).map {
        x => {
          println(x)
          writeToFile(svgRoot + "/" + "stopwatcher", "stop")
          println("Disconnected");
        }
      }
      var needToSendFile = true;
      var previouslySent = ""
      val out = Enumerator.generateM(
        scala.concurrent.Future {
          // Thread.sleep(2)
          // "current time %s".format((new java.util.Date()))
          println("======================================")
          val watchKey = watcher.take()
          val pevents = watchKey.pollEvents()
          val event = pevents.get(0)
          println("got " + pevents.toArray.length + " filesystem events")
          val evCont = event.context()
          val evKind = event.kind()
          println("the first event is in context: " + evCont + " of the kind: " + evKind)
          val mimetype = ("file --mime-type -b " + svgRoot + "/" + evCont) !!
          val message = if (mimetype contains "image/svg+xml") {
            println("detected  " + mimetype)
            val oldMD5 =  md5(previouslySent)
            Thread.sleep(50)
            var j = 3
            var newtext = readFromFilePath(Paths.get(svgRoot + "/" + evCont))
            while ((j > 0) && (md5(newtext) == oldMD5)) {
              j = j - 1
              Thread.sleep(50)
              newtext = readFromFilePath(Paths.get(svgRoot + "/" + evCont))
            }
            if ((j > 0) || (md5(newtext) != oldMD5)) {
              println("SENDING [" + j + "]")
              //Process("cp " + svgRoot + "/" + evCont + " public/images/incoming.svg") run;
              val svgtext = newtext
              if (needToSendFile) {
                needToSendFile = false
                previouslySent = svgtext
                Some("FILE\n" + svgtext)
              }
              else {
                val dmp = new diff_match_patch
                val patch = dmp.patch_make(previouslySent, svgtext)
                val patchString = dmp.patch_toText(patch)
                previouslySent = svgtext
                Some("PTCH\n" + patchString)
              }
            } else {
              println("(nochange)")
              Some("NADA")
            }
          } else {
            if (("/" + evCont) == "/stopwatcher") { if ((evKind + "_") == "ENTRY_CREATE_") {
              println("stopping")
              java.nio.file.Files.delete(java.nio.file.Paths.get(svgRoot + "/stopwatcher"))
              None } else Some("NADA")
            } else {
              println("==== ERROR ====")
              Some("NADA")
            }
          }
          watchKey.reset()
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