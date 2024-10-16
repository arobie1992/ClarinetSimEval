import java.math.BigDecimal as BD
import java.math.RoundingMode.*
import scala.annotation.targetName

case class TrustPcts(average: Double,
                     standardDeviation: Double,
                     min: Double,
                     max: Double,
                     pctTrusted: Double,
                     pctUntrusted: Double,
                     assessedPct: Double) {
  def altCsv: String =
    s"${min.trim},${average.trim},${max.trim},${standardDeviation.trim},${pctTrusted.trim},${pctUntrusted.trim},${assessedPct.trim}"
}
object TrustPcts {
  def from(ri: ReputationInformation, totalNodes: Double): TrustPcts = if(ri == null) null else TrustPcts(
    ri.average,
    ri.standardDeviation,
    ri.min,
    ri.max,
    ri.numTrusted/ri.totalPeers,
    ri.numUntrusted/ri.totalPeers,
    ri.totalPeers/totalNodes
  )
}

case class TrustPctsList(average: List[Double],
                         standardDeviation: List[Double],
                         min: List[Double],
                         max: List[Double],
                         pctTrusted: List[Double],
                         pctUntrusted: List[Double],
                         assessedPct: List[Double]) {
  @targetName("append")
  def +(rhs: TrustPctsList): TrustPctsList = TrustPctsList(
    average.appendedAll(rhs.average),
    standardDeviation.appendedAll(rhs.standardDeviation),
    min.appendedAll(rhs.min),
    max.appendedAll(rhs.max),
    pctTrusted.appendedAll(rhs.pctTrusted),
    pctUntrusted.appendedAll(rhs.pctUntrusted),
    assessedPct.appendedAll(rhs.assessedPct)
  )
}
object TrustPctsList {
  def from(tp: TrustPcts): TrustPctsList = TrustPctsList(
    List(tp.average),
    List(tp.standardDeviation),
    List(tp.min),
    List(tp.max),
    List(tp.pctTrusted),
    List(tp.pctUntrusted),
    List(tp.assessedPct)
  )
}

extension (tup: (TrustPctsList, TrustPctsList))
  @targetName("append")
  def +(rhs: (TrustPctsList, TrustPctsList)): (TrustPctsList, TrustPctsList) = (tup(0) + rhs(0), tup(1) + rhs(1))

case class AggTrustPcts(average: AverageStats,
                        standardDeviation: AverageStats,
                        min: AverageStats,
                        max: AverageStats,
                        pctTrusted: AverageStats,
                        pctUntrusted: AverageStats,
                        assessedPct: AverageStats) {
  def csv: String = s"${average.csv},${standardDeviation.csv},${min.csv},${max.csv},${pctTrusted.csv},${pctUntrusted.csv},${assessedPct.csv}"
  def altCsv: String =
    s"${min.csv},${average.csv},${max.csv},${standardDeviation.csv},${pctTrusted.csv},${pctUntrusted.csv},${assessedPct.csv}"
}
object AggTrustPcts {
  def from(tpSeq: Seq[TrustPcts]): AggTrustPcts =
    AggTrustPcts.from(tpSeq.filter(_ != null).map(TrustPctsList.from).reduce(_ + _))

  def from(tpl: TrustPctsList): AggTrustPcts = AggTrustPcts(
    AverageStats.fromValues(tpl.average),
    AverageStats.fromValues(tpl.standardDeviation),
    AverageStats.fromValues(tpl.min),
    AverageStats.fromValues(tpl.max),
    AverageStats.fromValues(tpl.pctTrusted),
    AverageStats.fromValues(tpl.pctUntrusted),
    AverageStats.fromValues(tpl.assessedPct)
  )
}

case class AggTrustPctsList(average: List[AverageStats],
                            standardDeviation: List[AverageStats],
                            min: List[AverageStats],
                            max: List[AverageStats],
                            pctTrusted: List[AverageStats],
                            pctUntrusted: List[AverageStats],
                            assessedPct: List[AverageStats]) {
  @targetName("append")
  def +(rhs: AggTrustPctsList): AggTrustPctsList = AggTrustPctsList(
    average.appendedAll(rhs.average),
    standardDeviation.appendedAll(rhs.standardDeviation),
    min.appendedAll(rhs.min),
    max.appendedAll(rhs.max),
    pctTrusted.appendedAll(rhs.pctTrusted),
    pctUntrusted.appendedAll(rhs.pctUntrusted),
    assessedPct.appendedAll(rhs.assessedPct)
  )
}
object AggTrustPctsList {
  def from(agg: AggTrustPcts): AggTrustPctsList = AggTrustPctsList(
    List(agg.average),
    List(agg.standardDeviation),
    List(agg.min),
    List(agg.max),
    List(agg.pctTrusted),
    List(agg.pctUntrusted),
    List(agg.assessedPct)
  )
}

case class AggregateAverages(min: AverageStats, mean: AverageStats, max: AverageStats, standardDeviation: AverageStats) {
  def csv: String = s"${min.csv},${mean.csv},${max.csv},${standardDeviation.csv}"
}
object AggregateAverages {
  def from(aggs: Seq[AverageStats]): AggregateAverages = AggregateAverages(
    AverageStats.fromValues(aggs.map(_.min)),
    AverageStats.fromValues(aggs.map(_.mean)),
    AverageStats.fromValues(aggs.map(_.max)),
    AverageStats.fromValues(aggs.map(_.standardDeviation))
  )
}

case class AggTrustPctsAgg(average: AggregateAverages,
                           standardDeviation: AggregateAverages,
                           min: AggregateAverages,
                           max: AggregateAverages,
                           pctTrusted: AggregateAverages,
                           pctUntrusted: AggregateAverages,
                           assessedPct: AggregateAverages) {
  def csv: String = s"${min.csv},${average.csv},${max.csv},${standardDeviation.csv},${pctTrusted.csv},${pctUntrusted.csv},${assessedPct.csv}"
}
object AggTrustPctsAgg {
  def from(agg: AggTrustPctsList): AggTrustPctsAgg = AggTrustPctsAgg(
    AggregateAverages.from(agg.average),
    AggregateAverages.from(agg.standardDeviation),
    AggregateAverages.from(agg.min),
    AggregateAverages.from(agg.max),
    AggregateAverages.from(agg.pctTrusted),
    AggregateAverages.from(agg.pctUntrusted),
    AggregateAverages.from(agg.assessedPct),
  )
}

extension (as: AverageStats)
  def csv: String = s"${as.min.trim},${as.mean.trim},${as.max.trim},${as.standardDeviation.trim}"

@main def main(directory: String, fileTypeStr: String): Unit =
  val simulations = loadSimulations(directory, FileType.valueOf(fileTypeStr.toUpperCase))
  val perSimulationTrustPcts = simulations
    .map(sim => (sim.simStats, sim.cooperative))
    .map { (stats, nodes) =>
      val numMal = stats.numNodes * stats.malPct
      val numCoop = stats.numNodes - numMal
      val trustPcts = nodes.map { n => (TrustPcts.from(n.cooperativePeers, numCoop), TrustPcts.from(n.maliciousPeers, numMal)) }
      (stats, AggTrustPcts.from(trustPcts.map((coop, _) => coop)), AggTrustPcts.from(trustPcts.map((_, mal) => mal)))
    }
    val perSimCsv = perSimulationTrustPcts
    .map((stats, coop, mal) => s"${stats.csv},${coop.csv},${mal.csv}")
    .prepended(s"${SimStats.csvHeader}," +
      "coop_average_min,coop_average_mean,coop_average_max,coop_average_std," +
      "coop_standardDeviation_min,coop_standardDeviation_mean,coop_standardDeviation_max,coop_standardDeviation_std," +
      "coop_min_min,coop_min_mean,coop_min_max,coop_min_std," +
      "coop_max_min,coop_max_mean,coop_max_max,coop_max_std," +
      "coop_pctTrusted_min,coop_pctTrusted_mean,coop_pctTrusted_max,coop_pctTrusted_std," +
      "coop_pctUntrusted_min,coop_pctUntrusted_mean,coop_pctUntrusted_max,coop_pctUntrusted_std," +
      "coop_assessedPct_min,coop_assessedPct_mean,coop_assessedPct_max,coop_assessedPct_std," +
      "mal_average_min,mal_average_mean,mal_average_max,mal_average_std," +
      "mal_standardDeviation_min,mal_standardDeviation_mean,mal_standardDeviation_max,mal_standardDeviation_std," +
      "mal_min_min,mal_min_mean,mal_min_max,mal_min_std," +
      "mal_max_min,mal_max_mean,mal_max_max,mal_max_std," +
      "mal_pctTrusted_min,mal_pctTrusted_mean,mal_pctTrusted_max,mal_pctTrusted_std," +
      "mal_pctUntrusted_min,mal_pctUntrusted_mean,mal_pctUntrusted_max,mal_pctUntrusted_std," +
      "mal_assessedPct_min,mal_assessedPct_mean,mal_assessedPct_max,mal_assessedPct_std")
  writeFile("data/processed/perSim_TrustPcts.csv", perSimCsv)

  val byNumNodes = perSimulationTrustPcts.groupBy((stats, _, _) => stats.numNodes)
  aggregateByDimension("numNodes", byNumNodes)
  val byNumCycles = perSimulationTrustPcts.groupBy((stats, _, _) => stats.numCycles)
  aggregateByDimension("numCycles", byNumCycles)
  val byMalPct = perSimulationTrustPcts.groupBy((stats, _, _) => stats.malPct)
  aggregateByDimension("malPct", byMalPct)
  val byMalActThresh = perSimulationTrustPcts.groupBy((stats, _, _) => stats.malActThresh)
  aggregateByDimension("malActThresh", byMalActThresh)
  val byMalActPct = perSimulationTrustPcts.groupBy((stats, _, _) => stats.malActPct)
  aggregateByDimension("malActPct", byMalActPct)

def aggregateByDimension[T: Numeric](
                          dimension: String,
                          groupedMetrics: Map[T, List[(SimStats, AggTrustPcts, AggTrustPcts)]]
                        ): Unit =
  val reduced = groupedMetrics
    .transform { (dimensionValue, sims) =>
      val coop = AggTrustPctsAgg.from(sims.map((_, coop, _) => coop).map(AggTrustPctsList.from).reduce(_+_))
      val mal = AggTrustPctsAgg.from(sims.map((_, _, mal) => mal).map(AggTrustPctsList.from).reduce(_+_))
      (coop, mal)
    }
    .map((dimensionValue, stats) => (dimensionValue, stats))
    .toList
    .sortBy((dimensionValue, _) => dimensionValue)

  val fullCsvLines = reduced
    .map((dimensionValue, stats) => s"$dimensionValue,${stats(0).csv},${stats(1).csv}")
    .prepended(s"$dimension," +
      "coop_min_min_min,coop_min_min_mean,coop_min_min_max,coop_min_min_std," +
      "coop_min_mean_min,coop_min_mean_mean,coop_min_mean_max,coop_min_mean_std," +
      "coop_min_max_min,coop_min_max_mean,coop_min_max_max,coop_min_max_std," +
      "coop_min_std_min,coop_min_std_mean,coop_min_std_max,coop_min_std_std," +
      "coop_average_min_min,coop_average_min_mean,coop_average_min_max,coop_average_min_std," +
      "coop_average_mean_min,coop_average_mean_mean,coop_average_mean_max,coop_average_mean_std," +
      "coop_average_max_min,coop_average_max_mean,coop_average_max_max,coop_average_max_std," +
      "coop_average_std_min,coop_average_std_mean,coop_average_std_max,coop_average_std_std," +
      "coop_max_min_min,coop_max_min_mean,coop_max_min_max,coop_max_min_std," +
      "coop_max_mean_min,coop_max_mean_mean,coop_max_mean_max,coop_max_mean_std," +
      "coop_max_max_min,coop_max_max_mean,coop_max_max_max,coop_max_max_std," +
      "coop_max_std_min,coop_max_std_mean,coop_max_std_max,coop_max_std_std," +
      "coop_standardDeviation_min_min,coop_standardDeviation_min_mean,coop_standardDeviation_min_max,coop_standardDeviation_min_std," +
      "coop_standardDeviation_mean_min,coop_standardDeviation_mean_mean,coop_standardDeviation_mean_max,coop_standardDeviation_mean_std," +
      "coop_standardDeviation_max_min,coop_standardDeviation_max_mean,coop_standardDeviation_max_max,coop_standardDeviation_max_std," +
      "coop_standardDeviation_std_min,coop_standardDeviation_std_mean,coop_standardDeviation_std_max,coop_standardDeviation_std_std," +
      "coop_pctTrusted_min_min,coop_pctTrusted_min_mean,coop_pctTrusted_min_max,coop_pctTrusted_min_std," +
      "coop_pctTrusted_mean_min,coop_pctTrusted_mean_mean,coop_pctTrusted_mean_max,coop_pctTrusted_mean_std," +
      "coop_pctTrusted_max_min,coop_pctTrusted_max_mean,coop_pctTrusted_max_max,coop_pctTrusted_max_std," +
      "coop_pctTrusted_std_min,coop_pctTrusted_std_mean,coop_pctTrusted_std_max,coop_pctTrusted_std_std," +
      "coop_pctUntrusted_min_min,coop_pctUntrusted_min_mean,coop_pctUntrusted_min_max,coop_pctUntrusted_min_std," +
      "coop_pctUntrusted_mean_min,coop_pctUntrusted_mean_mean,coop_pctUntrusted_mean_max,coop_pctUntrusted_mean_std," +
      "coop_pctUntrusted_max_min,coop_pctUntrusted_max_mean,coop_pctUntrusted_max_max,coop_pctUntrusted_max_std," +
      "coop_pctUntrusted_std_min,coop_pctUntrusted_std_mean,coop_pctUntrusted_std_max,coop_pctUntrusted_std_std," +
      "coop_assessedPct_min_min,coop_assessedPct_min_mean,coop_assessedPct_min_max,coop_assessedPct_min_std," +
      "coop_assessedPct_mean_min,coop_assessedPct_mean_mean,coop_assessedPct_mean_max,coop_assessedPct_mean_std," +
      "coop_assessedPct_max_min,coop_assessedPct_max_mean,coop_assessedPct_max_max,coop_assessedPct_max_std," +
      "coop_assessedPct_std_min,coop_assessedPct_std_mean,coop_assessedPct_std_max,coop_assessedPct_std_std," +
      "mal_min_min_min,mal_min_min_mean,mal_min_min_max,mal_min_min_std," +
      "mal_min_mean_min,mal_min_mean_mean,mal_min_mean_max,mal_min_mean_std," +
      "mal_min_max_min,mal_min_max_mean,mal_min_max_max,mal_min_max_std," +
      "mal_min_std_min,mal_min_std_mean,mal_min_std_max,mal_min_std_std," +
      "mal_average_min_min,mal_average_min_mean,mal_average_min_max,mal_average_min_std," +
      "mal_average_mean_min,mal_average_mean_mean,mal_average_mean_max,mal_average_mean_std," +
      "mal_average_max_min,mal_average_max_mean,mal_average_max_max,mal_average_max_std," +
      "mal_average_std_min,mal_average_std_mean,mal_average_std_max,mal_average_std_std," +
      "mal_max_min_min,mal_max_min_mean,mal_max_min_max,mal_max_min_std," +
      "mal_max_mean_min,mal_max_mean_mean,mal_max_mean_max,mal_max_mean_std," +
      "mal_max_max_min,mal_max_max_mean,mal_max_max_max,mal_max_max_std," +
      "mal_max_std_min,mal_max_std_mean,mal_max_std_max,mal_max_std_std," +
      "mal_standardDeviation_min_min,mal_standardDeviation_min_mean,mal_standardDeviation_min_max,mal_standardDeviation_min_std," +
      "mal_standardDeviation_mean_min,mal_standardDeviation_mean_mean,mal_standardDeviation_mean_max,mal_standardDeviation_mean_std," +
      "mal_standardDeviation_max_min,mal_standardDeviation_max_mean,mal_standardDeviation_max_max,mal_standardDeviation_max_std," +
      "mal_standardDeviation_std_min,mal_standardDeviation_std_mean,mal_standardDeviation_std_max,mal_standardDeviation_std_std," +
      "mal_pctTrusted_min_min,mal_pctTrusted_min_mean,mal_pctTrusted_min_max,mal_pctTrusted_min_std," +
      "mal_pctTrusted_mean_min,mal_pctTrusted_mean_mean,mal_pctTrusted_mean_max,mal_pctTrusted_mean_std," +
      "mal_pctTrusted_max_min,mal_pctTrusted_max_mean,mal_pctTrusted_max_max,mal_pctTrusted_max_std," +
      "mal_pctTrusted_std_min,mal_pctTrusted_std_mean,mal_pctTrusted_std_max,mal_pctTrusted_std_std," +
      "mal_pctUntrusted_min_min,mal_pctUntrusted_min_mean,mal_pctUntrusted_min_max,mal_pctUntrusted_min_std," +
      "mal_pctUntrusted_mean_min,mal_pctUntrusted_mean_mean,mal_pctUntrusted_mean_max,mal_pctUntrusted_mean_std," +
      "mal_pctUntrusted_max_min,mal_pctUntrusted_max_mean,mal_pctUntrusted_max_max,mal_pctUntrusted_max_std," +
      "mal_pctUntrusted_std_min,mal_pctUntrusted_std_mean,mal_pctUntrusted_std_max,mal_pctUntrusted_std_std," +
      "mal_assessedPct_min_min,mal_assessedPct_min_mean,mal_assessedPct_min_max,mal_assessedPct_min_std," +
      "mal_assessedPct_mean_min,mal_assessedPct_mean_mean,mal_assessedPct_mean_max,mal_assessedPct_mean_std," +
      "mal_assessedPct_max_min,mal_assessedPct_max_mean,mal_assessedPct_max_max,mal_assessedPct_max_std," +
      "mal_assessedPct_std_min,mal_assessedPct_std_mean,mal_assessedPct_std_max,mal_assessedPct_std_std"
    )
  val fullFileName = s"${dimension}_full.csv"
  println(s"Writing: $fullFileName")
  writeFile(s"data/processed/$fullFileName", fullCsvLines)

  val liteReduced = reduced
    .map { (dimensionValue, stats) =>
      val coop = stats._1
      val mal = stats._2
      def simplify = (agg: AggTrustPctsAgg) => AggTrustPcts(
        agg.average.mean,
        agg.standardDeviation.mean,
        agg.min.mean,
        agg.max.mean,
        agg.pctTrusted.mean,
        agg.pctUntrusted.mean,
        agg.assessedPct.mean
      )
      (dimensionValue, simplify(coop), simplify(mal))
    }
    val liteCsvRows = liteReduced
    .map((dimensionVal, coop, mal) => s"$dimensionVal,${coop.altCsv},${mal.altCsv}")
    .prepended(s"$dimension," +
      "coop_min_min,coop_min_mean,coop_min_max,coop_min_std," +
      "coop_average_min,coop_average_mean,coop_average_max,coop_average_std," +
      "coop_max_min,coop_max_mean,coop_max_max,coop_max_std," +
      "coop_standardDeviation_min,coop_standardDeviation_mean,coop_standardDeviation_max,coop_standardDeviation_std," +
      "coop_pctTrusted_min,coop_pctTrusted_mean,coop_pctTrusted_max,coop_pctTrusted_std," +
      "coop_pctUntrusted_min,coop_pctUntrusted_mean,coop_pctUntrusted_max,coop_pctUntrusted_std," +
      "coop_assessedPct_min,coop_assessedPct_mean,coop_assessedPct_max,coop_assessedPct_std," +
      "mal_min_min,mal_min_mean,mal_min_max,mal_min_std," +
      "mal_average_min,mal_average_mean,mal_average_max,mal_average_std," +
      "mal_max_min,mal_max_mean,mal_max_max,mal_max_std," +
      "mal_standardDeviation_min,mal_standardDeviation_mean,mal_standardDeviation_max,mal_standardDeviation_std," +
      "mal_pctTrusted_min,mal_pctTrusted_mean,mal_pctTrusted_max,mal_pctTrusted_std," +
      "mal_pctUntrusted_min,mal_pctUntrusted_mean,mal_pctUntrusted_max,mal_pctUntrusted_std," +
      "mal_assessedPct_min,mal_assessedPct_mean,mal_assessedPct_max,mal_assessedPct_std")
  val liteFileName = s"${dimension}_lite.csv"
  println(s"Writing $liteFileName")
  writeFile(s"data/processed/$liteFileName", liteCsvRows)

  val litestReduced = liteReduced
    .map { (dimensionVal, coop, mal) =>
      def simplify = (agg: AggTrustPcts) => TrustPcts(
        agg.average.mean,
        agg.standardDeviation.mean,
        agg.min.mean,
        agg.max.mean,
        agg.pctTrusted.mean,
        agg.pctUntrusted.mean,
        agg.assessedPct.mean
      )
      (dimensionVal, simplify(coop), simplify(mal))
    }
  val litestCsvRows = litestReduced
    .map((dimensionVal, coop, mal) => s"$dimensionVal,${coop.altCsv},${mal.altCsv}")
    .prepended(s"$dimension," +
      "coop_min,coop_average,coop_max,coop_standardDeviation,coop_pctTrusted,coop_pctUntrusted,coop_assessedPct," +
      "mal_min,mal_average,mal_max,mal_standardDeviation,mal_pctTrusted,mal_pctUntrusted,mal_assessedPct")
  val litestFileName = s"${dimension}_litest.csv"
  println(s"Writing $litestFileName")
  writeFile(s"data/processed/$litestFileName", litestCsvRows)

extension (d: Double)
  def trim: Double = BD(d).setScale(4, FLOOR).doubleValue()
