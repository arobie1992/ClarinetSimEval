@main def main(directory: String): Unit =
  val outputFiles = getFiles(directory)
  val output = processResults(outputFiles)
  val csvRows = output
    .map(s => (s.simStats, s.cooperative))
    .map((stats, coop) => (
      stats,
      coop.map(_.allPeers).filter(_ != null),
      coop.map(_.cooperativePeers).filter(_ != null),
      coop.map(_.maliciousPeers).filter(_ != null)
    ))
    .map((stats, all, coop, mal) => (
      stats,
      (all.reduce(_+_), all.size),
      (coop.reduce(_+_), coop.size),
      (mal.reduce(_+_), mal.size)
    ))
    .map((stats, all, coop, mal) =>
      val safeCalc = (a: ReputationInformation, b: Int) => if (b == 0) null else a/b
      val (allInfo, allCnt) = all
      val (coopInfo, coopCnt) = coop
      val (malInfo, malCnt) = mal
      (stats, safeCalc(allInfo, allCnt), safeCalc(coopInfo, coopCnt), safeCalc(malInfo, malCnt))
    )
    .map(asLightCsvRow)
  val csvContents = List(csvLightHeader).appendedAll(csvRows)
  writeFile("aggregated.csv", csvContents)
