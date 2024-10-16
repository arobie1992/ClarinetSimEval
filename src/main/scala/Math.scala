def mean(values: Iterable[Double]): Double = 
  if (values.isEmpty) return 0
  var mean = 0.0
  for (i <- values) {
    mean += i
  }
  mean / values.size

def standardDeviation(values: Iterable[Double]): Double = 
  if (values.size < 2) return 0.0
  val meanVal = mean(values)
  // The variance
  var variance = 0.0
  for (i <- values) {
    variance += Math.pow(i - meanVal, 2)
  }
  variance /= (values.size - 1)
  // Standard Deviation
  Math.sqrt(variance)

def median(values: Iterable[Double]): Double =
  val s = values.toList.sorted
  if(s.isEmpty) return 0
  if(s.length%2==0) {
    val midB = s.length/2
    val midA = midB-1
    (s(midA)+s(midB))/2
  } else {
    s(s.length/2)
  }
