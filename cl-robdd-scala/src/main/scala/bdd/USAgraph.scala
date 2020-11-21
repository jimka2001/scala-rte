// Copyright (c) 2019,20 EPITA Research and Development Laboratory
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

object USAgraph extends PoliticalMap {
  // this object contains connectivity information about the states in the USA.

  // set of state IDs excluding Alaska and Hawaii but including Washington DC
  //   as Alaska and Hawaii do not connect to any other state, and DC does
  //   connect to some.
  val allStates:Set[String] = Set("AL",
                                  //"AK",
                                  "AZ",
                                  "AR",
                                  "CA",
                                  "CO",
                                  "CT",
                                  "DC",
                                  "DE",
                                  "FL",
                                  "GA",
                                  //"HI",
                                  "ID",
                                  "IL",
                                  "IN",
                                  "IA",
                                  "KS",
                                  "KY",
                                  "LA",
                                  "ME",
                                  "MD",
                                  "MA",
                                  "MI",
                                  "MN",
                                  "MS",
                                  "MO",
                                  "MT",
                                  "NE",
                                  "NV",
                                  "NH",
                                  "NJ",
                                  "NM",
                                  "NY",
                                  "NC",
                                  "ND",
                                  "OH",
                                  "OK",
                                  "OR",
                                  "PA",
                                  "RI",
                                  "SC",
                                  "SD",
                                  "TN",
                                  "TX",
                                  "UT",
                                  "VT",
                                  "VA",
                                  "WA",
                                  "WV",
                                  "WI",
                                  "WY"
                                  )
  val statePositions:Map[String,(Double,Double)] = Map()
  val symbols:Map[String,String] = allStates.map{ st => (st -> st)}.toMap
  // Uni-directional graph so that if x is a member of stateUniGraph(y)
  //   then y is NOT a member of stateUniGraph(x)
  val stateUniGraph:Map[String,Set[String]] = Map(
    "CA" -> Set("OR","NV","AZ"),
    "OR" -> Set("WA","ID","NV"),
    "NV" -> Set("AZ","UT","ID"),
    "WA" -> Set("ID"),
    "ID" -> Set("MT","WY","UT"),
    "UT" -> Set("WY","CO","AZ"),
    "AZ" -> Set("NM"),
    "MT" -> Set("ND","SD","WY"),
    "WY" -> Set("SD","NE","CO"),
    "CO" -> Set("NE","KS","OK","NM"),
    "NM" -> Set("OK","TX"),
    "ND" -> Set("SD","MN"),
    "SD" -> Set("MN","IA","NE"),
    "NE" -> Set("IA","MO","KS"),
    "KS" -> Set("MO","OK"),
    "OK" -> Set("MO","AR","TX"),
    "TX" -> Set("AR","LA"),
    "MN" -> Set("WI","IA"),
    "IA" -> Set("WI","IL","MO"),
    "MO" -> Set("IL","KY","TN","AR"),
    "AR" -> Set("TN","MS","LA"),
    "LA" -> Set("MS"),
    "WI" -> Set("MI","IL"),
    "IL" -> Set("IN","KY"),
    "MS" -> Set("TN","AL"),
    "MI" -> Set("OH","IN"),
    "IN" -> Set("OH","KY"),
    "KY" -> Set("OH","WV","VA","TN"),
    "TN" -> Set("VA","NC","GA","AL"),
    "AL" -> Set("GA","FL"),
    "OH" -> Set("PA","WV"),
    "WV" -> Set("PA","MD","VA"),
    "VA" -> Set("MD","DC","NC"),
    "GA" -> Set("NC","SC","FL"),
    "PA" -> Set("NY","NJ","DE","MD"),
    "MD" -> Set("DE","DC"),
    "NC" -> Set("SC"),
    "VT" -> Set("NH","MA","NY"),
    "NY" -> Set("MA","CT","NJ"),
    "NJ" -> Set("DE"),
    "NH" -> Set("ME","MA"),
    "MA" -> Set("RI","CT"),
    "CT" -> Set("RI")
    )

  checkUniMap(allStates,stateUniGraph)

  // bi-directional graph so that if x is a member of stateBiGraph(y)
  //   then y is ALSO a member of stateUniGraph(x)
  val stateBiGraph:Map[String,Set[String]] = uniMapToBiMap(allStates,stateUniGraph)

  assert (stateBiGraph("ID") == Set("WA","OR","NV","UT","WY","MT")) // just a quick check
  assert(stateBiGraph("ME") == Set("NH"))
  checkBiMap(allStates,stateBiGraph)

}