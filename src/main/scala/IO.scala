import upickle.default.read

import java.io.{File, FileWriter}

def getFiles(directory: String): Seq[os.Path] =
  val dir = directory.charAt(0) match {
    case '/' => os.Path(directory)
    case '~' => os.Path(File(System.getProperty("user.home") + directory.substring(1)))
    // I have a feeling this is going to fail but meh
    case _ => os.pwd / directory
  }
  os.list(dir)

def processResults(outputFiles: Seq[os.Path], limit: Int = -1): List[Simulation] =
  val files = if(limit < 0) {
    outputFiles
  } else {
    outputFiles.take(limit)
  }
  files.zipWithIndex.map { (p, i) =>
    println(s"Parsing ${i+1}/${outputFiles.length}: $p")
    parseResults(p)
  }.toList

def parseResults(file: os.Path): Simulation =
  println("Parsing file: " + file.toString)
  val contents = os.read(file)
  Simulation(SimStats.from(file.getSegment(file.segmentCount-1)), read[List[NodeMetrics]](contents))

def writeFile(fileName: String, contents: List[String]): Unit =
  val writer = FileWriter(fileName, false)
  contents.zipWithIndex.foreach((row, i) =>
    writer.append(row).append('\n').flush()
    println(s"Wrote row ${i+1}/${contents.size}")
  )