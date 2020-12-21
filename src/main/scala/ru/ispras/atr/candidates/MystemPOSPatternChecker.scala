package ru.ispras.atr.candidates

import ru.ispras.atr.candidates.MystemPOSPatternChecker.MorphInfo
import MystemPOSPatternChecker._

import scala.annotation.tailrec
import scala.io.Source
import scala.util.matching.Regex

class MystemPOSPatternChecker(pattern: Regex) extends POSPatternChecker {

  override def satisfy(nGram: Seq[String]): Boolean = {
    val morphInfos = nGram.map(parse).toVector
    val variants = genAllVariants(morphInfos, Vector(Vector()))
    variants.map(_.map(_.toString)
      .mkString("_"))
      .exists(pattern.pattern.matcher(_).matches)
  }

  private def extract(morphInfoRaw: Vector[String], dict: Set[String]): Option[String] =
    morphInfoRaw.find(dict.contains)

  private def processPosTag(posRaw: String, morphInfoRaw: Vector[String], dict: Set[String]): String = {
    if (posRaw == "V") {
      morphInfoRaw.find(dict.contains).map(_.toUpperCase).getOrElse(posRaw)
    } else {
      posRaw
    }
  }

  private def parse(str: String): Vector[MorphInfo] = {
    val concrete = str.takeWhile(_ != '=')
    val posRaw = concrete.takeWhile(_ != ',')
    val concreteInfo = concrete.dropWhile(_ != ',').split(',').filter(_.nonEmpty).toVector
    val variantInfos = str.dropWhile(_ != '=')
      .filter(c => c != '=' && c != '(' && c != ')')
      .split('|')
      .map(_.split(',').toVector)
      .toVector
    val variants = variantInfos.map(concreteInfo ++ _)
    variants.map { morphInfoRaw =>
      MorphInfo(
        processPosTag(posRaw, morphInfoRaw, VerbForms),
        extract(morphInfoRaw, Genders),
        extract(morphInfoRaw, Cases),
        extract(morphInfoRaw, Numbers))
    }
  }

  @tailrec
  private def genAllVariants(morphInfos: Seq[Seq[MorphInfo]], acc: Seq[Seq[MorphInfo]]): Seq[Seq[MorphInfo]] = morphInfos.toList match {
    case Nil => acc
    case head::tail =>
      val newAcc = head.flatMap { morphInfo =>
        acc.map(_ :+ morphInfo)
      }
      genAllVariants(tail, newAcc)
  }
}

object MystemPOSPatternChecker {

  val Genders: Set[String] = Set("m", "f", "n")
  val Cases: Set[String] = Set("nom", "gen", "dat", "acc", "ins", "abl", "part", "loc", "voc")
  val Numbers: Set[String] = Set("sg", "pl")
  val VerbForms: Set[String] = Set("partcp")

  case class MorphInfo(pos: String,
                       gender: Option[String] = None,
                       cs: Option[String] = None,
                       number: Option[String] = None) {

    override def toString: String =
      s"$pos-${gender.getOrElse("#")}-${cs.getOrElse("#")}-${number.getOrElse("#")}"

    def toPatternString: String =
      s"$pos-${gender.getOrElse(".*")}-${cs.getOrElse(".*")}-${number.getOrElse(".*")}"
  }
}

case class MystemPOSPatternCheckerConfig(patternsFilePath: String = "./patterns.txt") extends POSPatternCheckerConfig {
  override def build(): POSPatternChecker = {
    val source = Source.fromFile(patternsFilePath)
    val patternStr = source.getLines
      .toList
      .flatMap(genPatterns)
      .map(pattern => s"($pattern)")
      .mkString("|")
    source.close()
    new MystemPOSPatternChecker(patternStr.r)
  }

  private def genPatterns(rawPattern: String): Set[String] = {
    val parts = rawPattern.split('|')
    val tags = parts(0).split(' ')
      .collect { case rawTag if rawTag.nonEmpty =>
        (rawTag, MorphInfo(rawTag.takeWhile(_.isLetter)))
      }
    val caseInfo = parts(1).replace("#", "")
      .split(',')
      .toList
      .collect { case m if m.nonEmpty =>
        val caseInfoParts = m.split('=')
        (caseInfoParts(0), caseInfoParts(1))
      }.toMap
    val tagsWithCase = tags.map { case (rawTag, morphInfo) =>
      (rawTag, morphInfo.copy(cs = caseInfo.get(rawTag)))
    }
    val tagsWithCaseMap = tagsWithCase.toMap
    val formEqualTags = parts(2).replace("#", "")
      .split(',')
      .collect { case m if m.nonEmpty => m.split('=')}
      .toSet
      .flatten
    val fixedCase = formEqualTags.flatMap(tagsWithCaseMap.get(_).flatMap(_.cs)).headOption
    val possibleCases = fixedCase match {
      case Some(cs) => Set(cs)
      case _ => Cases
    }
    val patterns = for {
      cs <- possibleCases
      number <- Numbers
      gender <- Genders
    } yield {
      tagsWithCase.map { case (rawTag, morphInfo) =>
        if (formEqualTags.contains(rawTag)) {
          morphInfo.copy(cs = Some(cs), number = Some(number), gender = Some(gender))
        } else {
          morphInfo
        }
      }
    }
    patterns.map { morphInfos =>
      morphInfos.map(_.toPatternString).mkString("_")
    }
  }
}

object MystemPOSPatternCheckerConfig {
  /** constructor for Java, since it doesn't support parameters with default values */
  def make() = MystemPOSPatternCheckerConfig()
}