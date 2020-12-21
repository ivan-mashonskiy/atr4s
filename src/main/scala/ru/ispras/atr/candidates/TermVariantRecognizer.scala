package ru.ispras.atr.candidates

import ru.ispras.atr.datamodel.{MorphemmeInfo, TermCandidate, TermOccurrence}

import scala.sys.process.Process

trait TermVariantRecognizer {
  def recognize(termOccurrences: Seq[TermOccurrence]): Seq[TermCandidate]
}

trait TermVariantRecognizerConfig {
  def build(): TermVariantRecognizer
}

object TermVariantRecognizerConfig {
  val subclasses = List(
    classOf[BagOfWordsTermVariantRecognizerConfig],
    classOf[CanonicalReprTermVariantRecognizerConfig],
    classOf[MorphemesTermVariantRecognizerConfig]
  )
}

class BagOfWordsTermVariantRecognizer extends TermVariantRecognizer {
  override def recognize(termOccurrences: Seq[TermOccurrence]): Seq[TermCandidate] = {
    termOccurrences.groupBy(TermOccurrence.bagOfWordsRepresentation).toSeq
      .map(tc => TermCandidate(tc._2))
  }
}

case class BagOfWordsTermVariantRecognizerConfig() extends TermVariantRecognizerConfig {
  override def build(): TermVariantRecognizer = new BagOfWordsTermVariantRecognizer
}

object BagOfWordsTermVariantRecognizerConfig {
  /** constructor for Java, since it doesn't support parameters with default values */
  def make() = BagOfWordsTermVariantRecognizerConfig()
}

class CanonicalReprTermVariantRecognizer extends TermVariantRecognizer {
  override def recognize(termOccurrences: Seq[TermOccurrence]): Seq[TermCandidate] = {
    termOccurrences.groupBy(TermOccurrence.canonicalRepresentation).toSeq
      .map(tc => TermCandidate(tc._2))
  }
}

case class CanonicalReprTermVariantRecognizerConfig() extends TermVariantRecognizerConfig {
  override def build(): TermVariantRecognizer = new CanonicalReprTermVariantRecognizer
}

object CanonicalReprTermVariantRecognizerConfig {
  /** constructor for Java, since it doesn't support parameters with default values */
  def make() = CanonicalReprTermVariantRecognizerConfig()
}

class MorphemesTermVariantRecognizer(morphemmerPath: String) extends BagOfWordsTermVariantRecognizer {
  override def recognize(termOccurrences: Seq[TermOccurrence]): Seq[TermCandidate] = {
    val (singleWord, multiWord) = termOccurrences.partition(_.lemmas.size == 1)

    val input = singleWord.flatMap(_.lemmas.headOption).mkString("\\n")
    val result = Process(s"echo -e $input").#|(Process(morphemmerPath)).lineStream.toList
    val termToMorphs = singleWord.zip(result).map { case (termOccurrence, morphRaw) =>
      val tagToMorph = morphRaw.split('\t')(1).split('/').map { morph =>
        val splitted = morph.split(':')
        splitted(0) -> splitted(1)
      }.toList.groupBy(_._2)
      termOccurrence -> MorphemmeInfo(
        prefs = tagToMorph.get("PREF").toList.flatten.map(_._2).toSet,
        roots = tagToMorph.get("ROOT").toList.flatten.map(_._2).toSet,
        suffixes = tagToMorph.get("SUFF").toList.flatten.map(_._2).toSet
      )
    }

    termToMorphs.groupBy { case (_, morphemmeInfo) =>
      morphemmeInfo.prefs -> morphemmeInfo.roots
    }.toSeq.map { case (_, termsWithMorphemmeInfo) =>
      TermCandidate(termsWithMorphemmeInfo.map(_._1))
    } ++ super.recognize(multiWord)
  }
}

case class MorphemesTermVariantRecognizerConfig(morphemmerPath: String = "./morphemmer") extends TermVariantRecognizerConfig {
  override def build(): TermVariantRecognizer = new MorphemesTermVariantRecognizer(morphemmerPath)
}

object MorphemesTermVariantRecognizerConfig {
  /** constructor for Java, since it doesn't support parameters with default values */
  def make() = MorphemesTermVariantRecognizerConfig()
}