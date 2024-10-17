@main def main(directory: String, fileTypeStr: String): Unit =
  val simulations = loadSimulations(directory, FileType.valueOf(fileTypeStr.toUpperCase))
  perTrialAggregates(simulations)
  perDimensionAnalysis(simulations)
