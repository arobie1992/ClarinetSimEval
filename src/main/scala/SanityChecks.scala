import scala.collection.mutable

val permutationValues: List[List[String]] = List(
  List("100", "250", "500", "750", "1000"),
  List("100", "1000", "10000"),
  List("10", "20", "30", "50", "90"),
  List("10", "20", "30", "50", "70", "90"),
  List("0.1", "0.2", "0.3", "0.5", "0.7", "0.9")
)

def checkMissing(permutationValues: List[List[String]], files: Seq[os.Path]): Unit =
  val missing = findMissing(permutationValues, files)
  if (missing.isEmpty) {
    println("All there :)")
  } else {
    missing.foreach(println)
  }

def findMissing(permutationValues: List[List[String]], files: Seq[os.Path]): List[List[String]] =
  val perms = permute(permutationValues)
  val existing = files.map(getParams).toSet
  perms.filter(!existing.contains(_))

def permute(vals: List[List[String]]): List[List[String]] =
  val counter = Counter(vals.map(_.length))
  val perms = mutable.ListBuffer[List[String]]()
  while(counter.continue()){
    val perm = counter.indices.zipWithIndex.map((j, i) => vals(i)(j))
    val nodeCount = perm(0).toInt
    val malPct = perm(2).toInt
    val malCount = (nodeCount*malPct)/100
    perm(2) = malCount.toString
    val cycleCount = perm(1).toInt
    val malActPct = perm(3).toInt
    val malActCount = (cycleCount*malActPct)/100
    perm(3) = malActCount.toString
    perms += perm.toList
    counter.incr()
  }
  perms.toList

def getParams(file: os.Path): List[String] =
  val fileName = file.getSegment(file.segmentCount-1)
  val parts = fileName.split("-").toList
  parts.zipWithIndex.filter((_, i) => Array(1,2,3,4,5).contains(i))
    .map((p, _) => p.substring(findFirstDigitPos(p)))
    .map(trimTrailing0s)

def findFirstDigitPos(str: String): Int =
  str.zipWithIndex.filter((c, i) => c.isDigit).map((c, i) => i).head

def trimTrailing0s(str: String): String =
  if(str.contains('.')) {
    str.reverse.dropWhile(_ == '0').reverse
  } else {
    str
  }
