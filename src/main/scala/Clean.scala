import java.io.PrintWriter

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.Random

object Clean {

  val YourCompanyName = "replace-me-"
  val NameAttribute = """(name|pipelineName|pipeline)="([^"]+)"""".r.unanchored
  val NameAndTemplateAttributes = """name="([^"]+)".*template="([^"]+)"""".r.unanchored
  val Url = """(ssh|http|https)://([^"<]+)""".r.unanchored

  val cleanNames = mutable.HashMap[String, String]()
  val cleanUrls = mutable.HashMap[String, String]()
  val replacements = new Replacements()

  def main(args: Array[String]) {
    val in = Source.fromFile("config.xml")
    val out = new PrintWriter("clean.xml")

    in.getLines().map(clean).foreach(out.println)
    in.close()
    out.close()

    println(s"Used ${replacements.wordsUsed} words")

    println("Mappings:")
    cleanNames.foreach { case (original, safe) =>
      println(s"$original -> $safe")
    }
  }

  def clean(s: String) = (s match {
    case line@NameAndTemplateAttributes(name, templateName) =>
      line.replaceAttribute(name, safeNameFor(name))
          .replaceAttribute(templateName, safeNameFor(templateName))
    case line@NameAttribute(attributeName, name) if !line.contains("<stage ") && !line.contains("<job ") =>
      line.replaceAttribute(name, safeNameFor(name))
    case line@Url(_, sensitive) if !line.startsWith("<cruise ") =>
      val replacement = cleanUrls.getOrElseUpdate(sensitive, replacements.next())
      line.replaceUrl(sensitive, replacement)
    case line => line
  }).replace(YourCompanyName, "acme-")

  def safeNameFor(sensitive: String): String = cleanNames.getOrElseUpdate(sensitive, replacements.next())

  implicit class SpecialString(val line: String) extends AnyVal {

    def replaceAttribute(original: String, clean: String): String = {
      line.replace(s"""="$original"""", s"""="$clean"""")
    }

    def replaceUrl(original: String, clean: String): String = {
      line.replace(s"""://$original""", s"""://$clean""")
    }
  }

}

/**
 * Make silly replacements for sensitive data.
 */
class Replacements {
  val words = {
    val in = Source.fromFile("/usr/share/dict/words")
    try {
      ArrayBuffer() ++= in.getLines().filter(_.length > 4).map(_.toLowerCase)
    }
    finally {
      in.close()
    }
  }
  val originalSize = words.length

  def next(): String = randomWord + "-" + randomWord

  def randomWord: String = words.remove(Random.nextInt(words.length))

  def wordsUsed: Int = originalSize - words.length
}
