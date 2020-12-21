package ru.ispras.atr.utils

import java.io.File

import ru.stachek66.nlp.mystem.holding.{Factory, MyStem, Request}

object MystemAnalyzer {
  def mystemAnalyzer: MyStem =
    new Factory("-igd --eng-gr --format json -e utf-8")
      .newMyStem(
        "3.0",
        Option(new File("./mystem"))).get
}