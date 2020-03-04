// Copyright (c) 2019 EPITA Research and Development Laboratory
//
// Permission is hereby granted, free of charge, to any person obtaining
// a copy of this software and associated documentation
// files (the "Software"), to deal in the Software without restriction,
// including without limitation the rights to use, copy, modify, merge,
// publish, distribute, sublicense, and/or sell copies of the Software,
// and to permit persons to whom the Software is furnished to do so,
// subject to the following conditions:
//
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
// LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
// OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
// WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

package bdd

object EuropeGraph extends PoliticalMap {
  val allStates: Set[String] = Set("Albania",
                                   "Andora",
                                   "Austria",
                                   "Belarus",
                                   "Belgium",
                                   "Bosnia",
                                   "Bulgaria",
                                   "Croatia",
                                   "Czech Republic",
                                   "Denmark",
                                   "Estonia",
                                   "Finland",
                                   "France",
                                   "Germany",
                                   "Greece",
                                   "Hungary",
                                   "Ireland",
                                   "Italy",
                                   "Kosovo",
                                   "Latvia",
                                   "Liechtenstein",
                                   "Lithuania",
                                   "Luxembourg",
                                   "Moldova",
                                   "Monaco",
                                   "Montenegro",
                                   "Netherlands",
                                   "North Macedonia",
                                   "Norway",
                                   "Poland",
                                   "Portugal",
                                   "Romania",
                                   "Russia",
                                   "Serbia",
                                   "Slovakia",
                                   "Slovenia",
                                   "Spain",
                                   "Sweden",
                                   "Switzerland",
                                   "UK",
                                   "Ukraine"
                                   )
  val symbols: Map[String,String] = Map("Albania"->"AL",
                                   "Andora"->"AD",
                                   "Austria" -> "AT",
                                   "Belarus" -> "BY",
                                   "Belgium" -> "BE",
                                   "Bosnia" -> "BA",
                                   "Bulgaria" -> "BG",
                                   "Croatia" -> "HR",
                                   "Czech Republic" -> "CZ",
                                   "Denmark" -> "DK",
                                   "Estonia" -> "EE",
                                   "Finland" -> "FI",
                                   "France" -> "FR",
                                   "Germany" -> "DE",
                                   "Greece" -> "GR",
                                   "Hungary" -> "HU",
                                   "Ireland" -> "IE",
                                   "Italy" -> "IT",
                                   "Kosovo" -> "KO",
                                   "Latvia" -> "LV",
                                   "Liechtenstein" -> "LI",
                                   "Lithuania" -> "LT",
                                   "Luxembourg" -> "LU",
                                   "Moldova" -> "MD",
                                   "Monaco" -> "MC",
                                   "Montenegro" -> "ME",
                                   "Netherlands" -> "NL",
                                   "North Macedonia" -> "MK",
                                   "Norway" -> "NO",
                                   "Poland" -> "PL",
                                   "Portugal" -> "PT",
                                   "Romania" -> "RO",
                                   "Russia" -> "RU",
                                   "Serbia" -> "RS",
                                   "Slovakia" -> "SK",
                                   "Slovenia" -> "SI",
                                   "Spain" -> "ES",
                                   "Sweden" -> "SE",
                                   "Switzerland" -> "CH",
                                   "UK" -> "GB",
                                   "Ukraine" -> "UA"
                                   )

  val statePositions:Map[String,(Double,Double)] = Map("Albania" -> (41.31666667,	19.816667),
                                                       "Andora" -> (43.958292,2.0),
                                                       "Austria" -> (48.2,	13.4),
                                                       "Belarus" -> (53.9,	19.0),
                                                       "Belgium" -> (52.83333333,	4.333333),
                                                       "Bosnia" -> (43.86666667,	18.416667),
                                                       "Bulgaria" -> (42.68333333,	23.316667),
                                                       "Croatia" -> (45.0,	16),
                                                       "Czech Republic" -> (50.08333333, 14.466667),
                                                       "Denmark" -> (54.66666667,	11.0),
                                                       "Estonia" -> (57.5,	16.5),
                                                       "Finland" -> (59.0,	16.5),
                                                       "France" -> (49.0,	2.333333),
                                                       "Germany" -> (52.51666667,	11.0),
                                                       "Greece" -> (41.0,	23.0),
                                                       "Hungary" -> (47.5,	19.083333),
                                                       "Ireland" -> (53.31666667,	-1.0),
                                                       "Italy" -> (43.0,	11.0),
                                                       "Kosovo" -> (42.66666667,	20.0),
                                                       "Liechtenstein" -> (46.0,	9.516667),
                                                       "Latvia" -> (56.0,	16.0),
                                                       "Lithuania" -> (54.68333333,	16.0),
                                                       "Luxembourg" -> (51.6,	6.116667),
                                                       "Moldova" -> (48,	23.0),
                                                       "Monaco" -> (43.73333333,	4.0),
                                                       "Montenegro" -> (42.43333333,	18.0),
                                                       "Netherlands" -> (54.35,	4.916667),
                                                       "North Macedonia" -> (42,21.433333),
                                                       "Norway" -> (62.0,	19.0),
                                                       "Poland" -> (52.25,	16.0),
                                                       "Portugal" -> (45.0,-1.0),
                                                       "Romania" -> (46.0,	23.0),
                                                       "Russia" -> (58.0,	22.0),
                                                       "Serbia" -> (44.83333333,	20.5),
                                                       "Slovakia" -> (49.0,	17.116667),
                                                       "Slovenia" -> (46.05,	14.516667),
                                                       "Spain" -> (46.0,0.0),
                                                       "Sweden" -> (58.0,	12.0),
                                                       "Switzerland" -> (46.91666667,	7.466667),
                                                       "UK" -> (51.507222,-0.1275),
                                                       "Ukraine" -> (50.43333333,	20.0)
                                                       )


  val stateUniGraph:Map[String,Set[String]] = Map("Andora"-> Set("France"),
                                                  "Albania" -> Set("Montenegro","Kosovo","Greece"),
                                                  "Austria" -> Set("Liechtenstein","Italy","Slovakia"),
                                                  "Belgium" -> Set("Luxembourg","Germany","Netherlands"),
                                                  "Belarus" -> Set("Latvia","Ukraine"),
                                                  "Bosnia" -> Set("Croatia", "Montenegro"),
                                                  "Bulgaria" -> Set("Greece","North Macedonia"),
                                                  "Czech Republic" -> Set("Poland", "Slovakia", "Austria"),
                                                  "Denmark" -> Set("Sweden"),
                                                  "Estonia" -> Set("Russia"),
                                                  "France" -> Set("Belgium","Luxembourg","Germany","Switzerland","Italy","Monaco"),
                                                  "Germany" -> Set("Denmark","Switzerland","Austria","Poland","Czech Republic"),
                                                  "Hungary" -> Set("Slovenia","Ukraine","Austria","Croatia"),
                                                  "Ireland" -> Set(),
                                                  "Italy" -> Set("Slovenia"),
                                                  "Latvia" -> Set("Estonia", "Russia"),
                                                  "Lithuania" -> Set("Belarus","Latvia"), // also Russia, but ignore this one
                                                  "Luxembourg" -> Set("Germany"),
                                                  "Moldova" -> Set("Ukraine","Romania"),
                                                  "Montenegro" -> Set("Kosovo","Croatia"),
                                                  "North Macedonia" -> Set("Greece","Albania","Kosovo"),
                                                  "Netherlands" -> Set("Germany"),
                                                  "Norway" -> Set("Finland","Russia"),
                                                  "Poland" -> Set("Slovakia","Belarus","Ukraine","Lithuania"), // also to Russia but ignore that
                                                  "Portugal" -> Set("Spain"),
                                                  "Romania" -> Set("Ukraine","Hungary","Bulgaria","Serbia"),
                                                  "Russia" -> Set("Finland","Belarus","Ukraine"),
                                                  "Serbia" -> Set("Hungary","Bosnia","Montenegro","Kosovo","Bulgaria","North Macedonia","Croatia"),
                                                  "Slovakia" ->Set("Hungary","Ukraine"),
                                                  "Slovenia" -> Set("Austria","Croatia"),
                                                  "Spain" -> Set("Andora","France","UK"),
                                                  "Sweden"-> Set("Norway", "Finland"),
                                                  "Switzerland" -> Set("Liechtenstein","Austria","Italy"),
                                                  "UK" -> Set("Ireland")
                                                  )
  checkUniMap(allStates,stateUniGraph)

  // bi-directional graph so that if x is a member of stateBiGraph(y)
  //   then y is ALSO a member of stateUniGraph(x)
  val stateBiGraph:Map[String,Set[String]] = uniMapToBiMap(allStates,stateUniGraph)
  //biGraphToDot(stateBiGraph,statePositions,"europe-political")(symbols=symbols)
  // assert that stateBiGraph does not contain any isolates vertices
  assert (allStates.forall{state => stateBiGraph.get(state).nonEmpty})

  def main(argv:Array[String]) = {
    println("-------------------")
    for{ (st,states) <- stateBiGraph}
      println(s"$st -> $states")
  }
}
