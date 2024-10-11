val csvLightHeader: String = "numNodes,numCycles,malPct,malActThresh,malActPct," +
  "allAverage,coopAverage,malAverage," +
  "allMin,coopMin,malMin," +
  "allMax,coopMax,malMax," +
  "allStandardDeviation,coopStandardDeviation,malStandardDeviation," +
  "allTotalPeers,coopTotalPeers,malTotalPeers," +
  "allNumTrusted,coopNumTrusted,malNumTrusted," +
  "allNumUntrusted,coopNumUntrusted,malNumUntrusted"

def asLightCsvRow(
                   stats: SimStats,
                   all: ReputationInformation,
                   coop: ReputationInformation,
                   mal: ReputationInformation
                 ): String =
  s"${stats.numNodes},${stats.numCycles},${stats.malPct},${stats.malActThresh},${stats.malActPct}," +
    s"${all.average},${coop.average},${mal.average},"+
    s"${all.min},${coop.min},${mal.min},"+
    s"${all.max},${coop.max},${mal.max},"+
    s"${all.standardDeviation},${coop.standardDeviation},${mal.standardDeviation},"+
    s"${all.totalPeers},${coop.totalPeers},${mal.totalPeers},"+
    s"${all.numTrusted},${coop.numTrusted},${mal.numTrusted},"+
    s"${all.numUntrusted},${coop.numUntrusted},${mal.numUntrusted}"