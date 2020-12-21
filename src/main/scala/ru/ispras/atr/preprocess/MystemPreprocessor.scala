package ru.ispras.atr.preprocess

import ru.ispras.atr.datamodel.Word
import ru.ispras.atr.utils.MystemAnalyzer
import ru.stachek66.nlp.mystem.holding.Request
import ru.stachek66.nlp.mystem.model.Info
import MystemPreprocessor._
import play.api.libs.json.Json

import scala.util.Try

class MystemPreprocessor extends NLPPreprocessor {
  override def extractWords(text: String): Seq[Word] = {
    var an = MystemAnalyzer.mystemAnalyzer
    val a = text.split('.').toSeq
    a.zipWithIndex.flatMap { case (sent, ind) =>
      Try {
        an
          .analyze(Request(sent))
          .info
          .flatMap { info =>
            info.morphology.map(Word(info.lex.getOrElse(info.initial), _))
          }.toSeq
      }.getOrElse{
        an = MystemAnalyzer.mystemAnalyzer
        Seq.empty[Word]
      }
    }
  }
}

object MystemPreprocessor {
  case class Morphology(posTag: String, gender: String, cs: String, number: String)

  implicit class InfoOps(info: Info) {
    def morphology: Option[String] = Try {
      (Json.parse(info.rawResponse) \ "analysis" \\ "gr")
        .head
        .as[String]
    }.toOption
  }
}

case class MystemPreprocessorConfig() extends NLPPreprocessorConfig {
  override def build(): NLPPreprocessor = new MystemPreprocessor
}

object MystemPreprocessorConfig {
  /** constructor for Java, since it doesn't support parameters with default values */
  def make() = MystemPreprocessorConfig()
}
