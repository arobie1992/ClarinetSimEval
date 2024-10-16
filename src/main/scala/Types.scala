import upickle.default.ReadWriter

import scala.annotation.targetName

case class ReputationInformation(
                                  average: Double,
                                  min: Double,
                                  max: Double,
                                  standardDeviation: Double,
                                  totalPeers: Double,
                                  numTrusted: Double,
                                  numUntrusted: Double,
                                  numMessages: Double,
                                  numMessagesWithTrusted: Double,
                                  numMessagesWithUntrusted: Double,
                                  numAssessments: Double,
                                  numAssessmentsOfTrusted: Double,
                                  numAssessmentsOfUntrusted: Double
                                ) derives ReadWriter {
    @targetName("add")
    def +(rhs: ReputationInformation): ReputationInformation =
      ReputationInformation(
        average + rhs.average,
        min + rhs.min,
        max + rhs.max,
        standardDeviation + rhs.standardDeviation,
        totalPeers + rhs.totalPeers,
        numTrusted + rhs.numTrusted,
        numUntrusted + rhs.numUntrusted,
        numMessages + rhs.numMessages,
        numMessagesWithTrusted + rhs.numMessagesWithTrusted,
        numMessagesWithUntrusted + rhs.numMessagesWithUntrusted,
        numAssessments + rhs.numAssessments,
        numAssessmentsOfTrusted + rhs.numAssessmentsOfTrusted,
        numAssessmentsOfUntrusted + rhs.numAssessmentsOfUntrusted
      )

    @targetName("divide")
    def /(rhs: Int): ReputationInformation =
      ReputationInformation(
        average / rhs,
        min / rhs,
        max / rhs,
        standardDeviation / rhs,
        totalPeers / rhs,
        numTrusted / rhs,
        numUntrusted / rhs,
        numMessages / rhs,
        numMessagesWithTrusted / rhs,
        numMessagesWithUntrusted / rhs,
        numAssessments / rhs,
        numAssessmentsOfTrusted / rhs,
        numAssessmentsOfUntrusted / rhs
      )
}

case class ID(nodeId: Long, connectionId: String, seqNo: Int) derives ReadWriter

case class MessageAssessment(id: ID, status: String) derives ReadWriter

case class MessageRecord(
                          connectionId: String,
                          seqNo: Int,
                          data: String,
                          senderSignature: String,
                          witnessSignature: String,
                          sender: Long, witness: Long,
                          receiver: Long
                        ) derives ReadWriter

case class PeerInfo(
                     id: Long,
                     `type`: String,
                     reputation: Double,
                     trusted: Boolean,
                     numMessages: Long,
                     messages: List[MessageRecord],
                     numAssessments: Long,
                     assessments: List[MessageAssessment]
                   ) derives ReadWriter

case class NodeMetrics(
                        id: Long,
                        `type`: String,
                        selfWithPeers: ReputationInformation,
                        allPeers: ReputationInformation,
                        cooperativePeers: ReputationInformation,
                        maliciousPeers: ReputationInformation
                      ) derives ReadWriter


case class Simulation(simStats: SimStats, metrics: List[NodeMetrics]) {
  def cooperative: Seq[NodeMetrics] = metrics.filter(m => m.`type` == "COOPERATIVE")
}

case class SimStats(numNodes: Int, numCycles: Int, malPct: Double, malActThresh: Double, malActPct: Double) {
  def toFileName: String = s"sim-nodes$numNodes-cycles$numCycles-malPct$malPct" +
    s"-malActThresh$malActThresh-malActPct$malActPct"

  def csv: String = s"$numNodes,$numCycles,$malPct,$malActThresh,$malActPct"
}
object SimStats {
  def from(fileName: String): SimStats =
    val parts = fileName.split("-").toList
    val numVals: List[Double|Int] = parts.zipWithIndex
      .filter((_, i) => Array(1, 2, 3, 4, 5).contains(i))
      .map((p, i) => (p.substring(findFirstDigitPos(p)), i)).map((p, i) =>
        i match
          case 5 => p.toDouble
          case _ => p.toInt
      )
    val numNodes = numVals.head.asInstanceOf[Int]
    val numCycles = numVals(1).asInstanceOf[Int]
    val numMal = numVals(2).asInstanceOf[Int]
    val malActThreshCount = numVals(3).asInstanceOf[Int]
    SimStats(
      numNodes,
      numCycles,
      numMal.toDouble/numNodes.toDouble,
      malActThreshCount.toDouble/numCycles.toDouble,
      numVals(4).asInstanceOf[Double]
    )
  def csvHeader = "numNodes,numCycles,malPct,malActThresh,malActPct"
}

class Counter(val radices: List[Int]):
  val indices: Array[Int] = radices.map(_ => 0).toArray
  private var overflow = false

  def incr(): Unit =
    var carry = true
    for (i <- indices.indices.reverse) {
      var newVal = indices(i)
      if (carry) {
        newVal += 1
        carry = false
      }
      if (newVal == radices(i)) {
        newVal = 0
        carry = true
      }
      indices(i) = newVal
    }
    overflow = carry

  def continue(): Boolean = !overflow

  override def toString: String = indices.zipWithIndex.map((v, i) => s"$v/${radices(i)}").reduce(_ + "," + _)


case class SimulationByTypes(stats: SimStats, all: ReputationInformation, coop: ReputationInformation, mal: ReputationInformation)

case class AverageStats(mean: Double, standardDeviation: Double, min: Double, max: Double, median: Double)
object AverageStats {
  def fromValues(values: Iterable[Double]): AverageStats =
    AverageStats(mean(values), standardDeviation(values), values.min, values.max, median(values))
}