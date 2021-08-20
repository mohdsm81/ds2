package edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability.history.parsers

import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability.history.flat.HistoryTypes.Event
import edu.utah.cs.gauss.ds2.core.MyTestSpecs
import edu.utah.cs.gauss.serialization.IO
import java.io.File

class PorcupineDataSpec extends MyTestSpecs {
  test("Porcupine Log Parser c01-bad.txt") {
    val content = IO.readLinesFromFile(new File("data/c01-bad.txt"))

    val parsed = PorcupineData.parseKV(content)

    assert(parsed.size == content.size)
    assert(Event.makeHistory(parsed).toPorcupineLog.trim == content.mkString("\n").trim)
  }

  test("Porcupine Log Parser c10-bad.txt") {
    val content = IO.readLinesFromFile(new File("data/c10-bad.txt"))

    val parsed = PorcupineData.parseKV(content)

    assert(parsed.size == content.size)
    assert(Event.makeHistory(parsed).toPorcupineLog.trim == content.mkString("\n").trim)
  }

  test("Porcupine Log Parser c50-bad.txt") {
    val content = IO.readLinesFromFile(new File("data/c50-bad.txt"))

    val parsed = PorcupineData.parseKV(content)

    assert(parsed.size == content.size)
    assert(Event.makeHistory(parsed).toPorcupineLog.trim == content.mkString("\n").trim)
  }

  test("Porcupine Log Parser c50-ok.txt") {
    val content = IO.readLinesFromFile(new File("data/c50-ok.txt"))

    val parsed = PorcupineData.parseKV(content)

    assert(parsed.size == content.size)
    assert(Event.makeHistory(parsed).toPorcupineLog.trim == content.mkString("\n").trim)
  }

  test("Porcupine Parser2 - c01-bad.txt"){
    val content = IO.readLinesFromFile(new File("data/c01-bad.txt"))

    val parsed = PorcupineData.parseKV2(content)

    assert(parsed.size == content.size)
    assert(Event.makeHistory(parsed).toPorcupineLog.trim == content.mkString("\n").trim)
  }

  test("Porcupine Parser2 - c01-ok.txt"){
    val content = IO.readLinesFromFile(new File("data/c01-ok.txt"))

    val parsed = PorcupineData.parseKV2(content)

    assert(parsed.size == content.size)
    assert(Event.makeHistory(parsed).toPorcupineLog.trim == content.mkString("\n").trim)
  }

  test("Porcupine Parser2 - c10-bad.txt"){
    val content = IO.readLinesFromFile(new File("data/c10-bad.txt"))

    val parsed = PorcupineData.parseKV2(content)

    assert(parsed.size == content.size)
    assert(Event.makeHistory(parsed).toPorcupineLog.trim == content.mkString("\n").trim)
  }

  test("Porcupine Parser2 - c10-ok.txt"){
    val content = IO.readLinesFromFile(new File("data/c10-ok.txt"))

    val parsed = PorcupineData.parseKV2(content)

    assert(parsed.size == content.size)
    assert(Event.makeHistory(parsed).toPorcupineLog.trim == content.mkString("\n").trim)
  }

  test("Porcupine Parser2 - c50-bad.txt"){
    val content = IO.readLinesFromFile(new File("data/c50-bad.txt"))

    val parsed = PorcupineData.parseKV2(content)

    assert(parsed.size == content.size)
    assert(Event.makeHistory(parsed).toPorcupineLog.trim == content.mkString("\n").trim)
  }

  test("Porcupine Parser2 - c50-ok.txt"){
    val content = IO.readLinesFromFile(new File("data/c50-ok.txt"))

    val parsed = PorcupineData.parseKV2(content)

    assert(parsed.size == content.size)
    assert(Event.makeHistory(parsed).toPorcupineLog.trim == content.mkString("\n").trim)
  }
}
