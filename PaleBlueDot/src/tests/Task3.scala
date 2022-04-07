package tests

import org.scalatest._
import pbd.PaleBlueDot

import java.util

class Task3 extends FunSuite {

  val countriesFile: String = "data/countries.txt"
  val citiesFilename: String = "data/cities.csv"

  test("1 wrong country") {
    val testCases: Map[String, Map[String,Int]] = Map(
      "ADAOCNAOlBania" -> Map(),
      "AnDOrOXAJCAJKDra"-> Map()
    )
    for ((input, expectedOutput) <- testCases) {
      val computedOutput: Map[String,Int] = PaleBlueDot.cityPopulations(countriesFile,citiesFilename,input)
      assert(computedOutput == expectedOutput, input + " -> " + computedOutput)
    }
  }


  test("1 correct country") {
    val testCases: Map[String, Map[String,Int]] = Map(
      "AlBania" -> Map("burrel" -> 15539, "fushe-kruje" -> 10458, "mamurras" -> 8282, "kruje" -> 21286, "libohove" -> 3651, "bajram curri" -> 7967, "leskovik" -> 2655, "peshkopi" -> 15261, "polican" -> 10663, "kucove" -> 18166, "roskovec" -> 6657, "tepelene" -> 11955, "bulqize" -> 11212, "perrenjas" -> 4015, "rreshen" -> 10064, "shijak" -> 14138, "lezhe" -> 18695, "selenice" -> 6912, "bilisht" -> 7114, "librazhd" -> 12691, "sarande" -> 15147, "cerrik" -> 14269, "himare" -> 4596, "shengjin" -> 2698, "fierze" -> 742, "permet" -> 10686, "orikum" -> 2604, "durres" -> 122034, "shkoder" -> 89102, "milot" -> 2157, "kamze" -> 11026, "corovode" -> 14046, "rrogozhine" -> 5620, "koplik" -> 4078, "erseke" -> 7890, "memaliaj" -> 4951, "delvine" -> 3916, "kurbnesh" -> 1429, "vore" -> 4973, "gjirokaster" -> 23437, "elbasan" -> 100903, "lushnje" -> 41423, "gramsh" -> 11556, "ballsh" -> 10361, "pogradec" -> 25758, "vlore" -> 89528, "konispol" -> 1349, "kerrabe" -> 1177, "peqin" -> 7513, "ulze" -> 600, "patos" -> 22679, "puke" -> 6495, "berat" -> 47606, "klos" -> 683, "tirana" -> 374801, "fier" -> 59719, "kavaje" -> 29354, "kraste" -> 1168, "fushe-arrez" -> 2438, "kelcyre" -> 2486, "kukes" -> 17972, "rubik" -> 2332, "lac" -> 24825, "maliq" -> 3735, "korce" -> 58259),
      "AnDOrra"->Map("la massana" -> 7211, "les escaldes" -> 15854, "ordino" -> 2553, "sant julia de loria" -> 8020)
    )
    for ((input, expectedOutput) <- testCases) {
      val computedOutput: Map[String,Int] = PaleBlueDot.cityPopulations(countriesFile,citiesFilename,input)
      assert(computedOutput == expectedOutput, input + " -> " + computedOutput)
    }
  }
  test("2 wrong country") {
    val testCases: Map[String,List[String]] = Map(
      "AnDOrrADAIa"->List(),

    )
    for ((input, expectedOutput) <- testCases) {
      val computedOutput: List[String] = PaleBlueDot.aboveAverageCities(countriesFile,citiesFilename,input)
      assert(computedOutput == expectedOutput, input + " -> " + computedOutput)
    }
  }


  test("2 correct country") {
    val testCases: Map[String,List[String]] = Map(
      "AnDOrra"->List("les escaldes").sorted,
      "Angola"-> List("luanda", "lobito", "benguela", "huambo").sorted,
      "Jamaica"-> List("kingston", "spanish town", "montego bay", "portmore").sorted,
      "Thailand"->List("khon kaen", "sakhon nakhon", "bangkok", "samut sakhon", "chon buri", "pattaya", "lampang", "nakhon pathom", "krathum baen", "hat yai", "chanthaburi", "thanyaburi", "phuket", "ratchaburi", "nonthaburi", "nong khai", "surat thani", "pak kret", "ayutthaya", "nakhon ratchasima", "chiang mai", "kanchanaburi", "phra pradaeng", "rayong", "yala", "nakhon si thammarat", "trang", "chiang rai", "khlong luang", "si racha", "udon thani", "songkhla", "ubon ratchathani", "phitsanulok", "sattahip", "nakhon sawan", "samut prakan", "bang kruai").sorted,
      "Aruba"->List()

    );

    for ((input, expectedOutput) <- testCases) {
      val computedOutput: List[String] = PaleBlueDot.aboveAverageCities(countriesFile,citiesFilename,input)
      assert(computedOutput.sorted == expectedOutput.sorted, input + " -> " + computedOutput.sorted)
    }
  }


}
