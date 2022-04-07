package tests

import org.scalatest._
import pbd.PaleBlueDot

class Task2 extends FunSuite {

  val countriesFile: String = "data/countries.txt"
  val citiesFilename: String = "data/cities.csv"

  test("no country") {
    val testCases: Map[String, Double] = Map(
      "Ugandaa" -> 0.0,
      "Japaan" -> 0.0,
      "SouthJapaan Africa" -> 0.0,
      "PeJapaanru" -> 0.0,
      "BJapaanelgium" -> 0.0,
      "AlbaJapaannia" -> 0.0
    )

    for ((input, expectedOutput) <- testCases) {
      val computedOutput: Double = PaleBlueDot.averagePopulation(countriesFile,citiesFilename,input)
      assert(computedOutput == expectedOutput, input + " -> " + computedOutput)
    }

  }
  test("correct country"){
    val testCases: Map[String, Double] = Map(
      "hEaRd IsLaNd AnD mCdOnAlD iSlAnDs" -> 0.0,
      "UGANDA" -> 44471.1975308642,
      "south africa" -> 142352.0202020202,
      "jAPAn" -> 134539.0834437086,
    )

    for ((input, expectedOutput) <- testCases) {
      val computedOutput: Double = PaleBlueDot.averagePopulation(countriesFile,citiesFilename,input)
      assert(computedOutput == expectedOutput, input + " -> " + computedOutput)
    }
  }





}
