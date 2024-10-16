case class InfoSubset(average: Double,
                      min: Double,
                      max: Double,
                      standardDeviation: Double,
                      totalPeers: Double,
                      numTrusted: Double,
                      numUntrusted: Double)
object InfoSubset {
  def from(ri: ReputationInformation): InfoSubset =
    InfoSubset(ri.average, ri.min, ri.max, ri.standardDeviation, ri.totalPeers, ri.numTrusted, ri.numUntrusted)
}

case class AggInfoSubset(average: AverageStats,
                         min: AverageStats,
                         max: AverageStats,
                         standardDeviation: AverageStats,
                         totalPeers: AverageStats,
                         numTrusted: AverageStats,
                         numUntrusted: AverageStats) {
  def csv: String = s"${min.csv},${average.csv},${max.csv},${standardDeviation.csv},${totalPeers.csv},${numTrusted.csv},${numUntrusted.csv}"
}
object AggInfoSubset {
  def from(vals: Seq[InfoSubset]): AggInfoSubset =
    val avgs = vals
      .map(is => Map(
        "average" -> List(is.average),
        "min" -> List(is.min),
        "max" -> List(is.max),
        "standardDeviation" -> List(is.standardDeviation),
        "totalPeers" -> List(is.totalPeers),
        "numTrusted" -> List(is.numTrusted),
        "numUntrusted" -> List(is.numUntrusted),
      ))
      .reduce((a, b) => a.transform((k, v) => v.appendedAll(b(k))))
      .transform((_, v) => AverageStats.fromValues(v))
    AggInfoSubset(
      avgs("average"),
      avgs("min"),
      avgs("max"),
      avgs("standardDeviation"),
      avgs("totalPeers"),
      avgs("numTrusted"),
      avgs("numUntrusted")
    )
}

def perTrialAggregates(simulations: List[Simulation]): Unit =
  val perTrialAggregate = simulations
    .map(sim => (sim.simStats, sim.cooperative))
    .map((stats, coop) => (
      stats,
      coop.map(_.allPeers).filter(_ != null).map(InfoSubset.from),
      coop.map(_.cooperativePeers).filter(_ != null).map(InfoSubset.from),
      coop.map(_.maliciousPeers).filter(_ != null).map(InfoSubset.from)
    ))
    .map((stats, all, coop, mal) => (stats, AggInfoSubset.from(all), AggInfoSubset.from(coop), AggInfoSubset.from(mal)))
  val perTrialCsv = perTrialAggregate.map((stats, all, coop, mal) =>
      s"${stats.numNodes},${stats.numCycles},${stats.malPct},${stats.malActThresh},${stats.malActPct},${all.csv},${coop.csv},${mal.csv}")
    .prepended("numNodes,numCycles,malPct,malActThresh,malActPct," +
      "allMin_min,allMin_mean,allMin_max,allMin_std,allMin_median," +
      "allMean_min,allMean_mean,allMean_max,allMean_std,allMean_median," +
      "allMax_min,allMax_mean,allMax_max,allMax_std,allMax_median," +
      "allStandardDeviation_min,allStandardDeviation_mean,allStandardDeviation_max,allStandardDeviation_std,allStandardDeviation_median," +
      "allTotalPeers_min,allTotalPeers_mean,allTotalPeers_max,allTotalPeers_std,allTotalPeers_median," +
      "allTrusted_min,allTrusted_mean,allTrusted_max,allTrusted_std,allTrusted_median," +
      "allUntrusted_min,allUntrusted_mean,allUntrusted_max,allUntrusted_std,allUntrusted_median," +
      "coopMin_min,coopMin_mean,coopMin_max,coopMin_std,coopMin_median," +
      "coopMean_min,coopMean_mean,coopMean_max,coopMean_std,coopMean_median," +
      "coopMax_min,coopMax_mean,coopMax_max,coopMax_std,coopMax_median," +
      "coopStandardDeviation_min,coopStandardDeviation_mean,coopStandardDeviation_max,coopStandardDeviation_std,coopStandardDeviation_median," +
      "coopTotalPeers_min,coopTotalPeers_mean,coopTotalPeers_max,coopTotalPeers_std,coopTotalPeers_median," +
      "coopTrusted_min,coopTrusted_mean,coopTrusted_max,coopTrusted_std,coopTrusted_median," +
      "coopUntrusted_min,coopUntrusted_mean,coopUntrusted_max,coopUntrusted_std,coopUntrusted_median," +
      "malMin_min,malMin_mean,malMin_max,malMin_std,malMin_median," +
      "malMean_min,malMean_mean,malMean_max,malMean_std,malMean_median," +
      "malMax_min,malMax_mean,malMax_max,malMax_std,malMax_median," +
      "malStandardDeviation_min,malStandardDeviation_mean,malStandardDeviation_max,malStandardDeviation_std,malStandardDeviation_median," +
      "malTotalPeers_min,malTotalPeers_mean,malTotalPeers_max,malTotalPeers_std,malTotalPeers_median," +
      "malTrusted_min,malTrusted_mean,malTrusted_max,malTrusted_std,malTrusted_median," +
      "malUntrusted_min,malUntrusted_mean,malUntrusted_max,malUntrusted_std,malUntrusted_median"
    )
  writeFile("data/processed/perTrialAggregates.csv", perTrialCsv)