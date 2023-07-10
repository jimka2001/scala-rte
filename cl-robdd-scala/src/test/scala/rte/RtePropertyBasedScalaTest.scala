package rte

import adjuvant.GnuPlot.gnuPlot
import adjuvant.MyFunSuite
import org.scalacheck.Prop.{classify, forAll}
import org.scalacheck.{Arbitrary, Properties, Test}
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.Configuration
import rte.RteSpecifications.naiveGenRte
import xymbolyco.mystats.{brz, brzmin, thmp, thmpmin}
import xymbolyco.{GraphViz, RTEStatistics, mystats}

import java.lang.System.console
import scala.collection.immutable.{Map, Set}
import scala.Predef.->

object RtePropertyBasedScalaTest extends App {
  def classifyRte(n: Int, size: Int, stdDepth: Int)= {
    val boltzmann = new RteBoltzmann(size, stdDepth)
    implicit lazy val arbitraryGen: Arbitrary[Rte] = Arbitrary(boltzmann.RteBoltzmannGen())


    // Classification of the generated output
    var empty = 0
    var satisfiable = 0
    var indeterminate = 0

    for {
      i <- Range(0, n)
    } {
      val rte = boltzmann.RteBoltzmannGen().sample.get
      val dfa = rte.toDfa(42)

      if (dfa.vacuous().getOrElse(false))
        empty += 1
      else dfa.spanningPath match {
        case Some(Left(_)) => indeterminate += 1
        case Some(Right(_)) => satisfiable += 1
      }
    }

    empty = empty * 100 / n
    satisfiable = satisfiable * 100 / n
    indeterminate = indeterminate * 100 / n

    Console.println(s"classification of ${n} RTEs of approximate size ${size}")
    Console.println(s"empty=${empty}%")
    Console.println(s"satisfiable=${satisfiable}%")
    Console.println(s"indeterminate=${indeterminate}%")
    Console.println(s"total=${indeterminate + empty + satisfiable}% (the difference between the total and 100% is due to floating point imprecisions")
  }

  def viewRte(n: Int, size: Int, stdDepth: Int) = {
    val boltzmann = new RteBoltzmann(size, stdDepth)
    implicit lazy val arbitraryGen: Arbitrary[Rte] = Arbitrary(boltzmann.RteBoltzmannGen())

    for {
      i <- Range(0, n)
    } {
      val rte = boltzmann.RteBoltzmannGen().sample.get
      val dfa = rte.toDfa(42)
      GraphViz.dfaView(dfa)
    }
  }

  def printRte(n: Int, size: Int, stdDepth: Int) = {
    val boltzmann = new RteBoltzmann(size, stdDepth)
    implicit lazy val arbitraryGen: Arbitrary[Rte] = Arbitrary(boltzmann.RteBoltzmannGen())

    for {
      i <- Range(0, n)
    } {
      val rte = boltzmann.RteBoltzmannGen().sample.get
      Console.println(rte.toMachineReadable())
    }
  }

  viewRte(10, 25, 1)
//  printRte(100, 25, 1)
//  classifyRte(100, 25, 1)
}