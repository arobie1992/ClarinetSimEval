import FileType.{FURY, JSON}
import org.apache.fury.Fury
import org.apache.fury.serializer.scala.ScalaSerializers
import upickle.default.read

import java.io.{File, FileOutputStream, FileInputStream, FileWriter}

enum FileType { case JSON, FURY }

def loadSimulations(directory: String, fileType: FileType): List[Simulation] =
  val files = getFiles(directory)
  fileType match
    case JSON => parseJsonResults(files)
    case FURY => parseFuryResults(files)

def getFiles(directory: String): Seq[os.Path] =
  val dir = directory.charAt(0) match {
    case '/' => os.Path(directory)
    case '~' => os.Path(File(System.getProperty("user.home") + directory.substring(1)))
    case _ => os.pwd / os.RelPath(directory)
  }
  os.list(dir)

def parseJsonResults(outputFiles: Seq[os.Path]): List[Simulation] =
  outputFiles.zipWithIndex.map { (f, i) =>
    println(s"Parsing ${i+1}/${outputFiles.length}: $f")
    val contents = os.read(f)
    Simulation(SimStats.from(f.getSegment(f.segmentCount - 1)), read[List[NodeMetrics]](contents))
  }.toList

def parseFuryResults(outputFiles: Seq[os.Path]): List[Simulation] =
  val fury = Fury.builder()
    .withScalaOptimizationEnabled(true)
    .requireClassRegistration(false)
    .withRefTracking(true)
    .build()
  ScalaSerializers.registerSerializers(fury)
  outputFiles.zipWithIndex.map { (f, i) =>
    println(s"Parsing ${i + 1}/${outputFiles.size}: $f")
    val reader = FileInputStream(f.toString)
    val bytes = try reader.readAllBytes() finally reader.close()
    fury.deserialize(bytes).asInstanceOf[Simulation]
  }.toList

def writeFile(fileName: String, contents: List[String]): Unit =
  val writer = FileWriter(fileName, false)
  contents.zipWithIndex.foreach((row, i) =>
    writer.append(row).append('\n').flush()
    println(s"Wrote row ${i+1}/${contents.size}")
  )

def writeFuryFiles(output: List[Simulation], directory: String): Unit =
  val fury = Fury.builder()
    .withScalaOptimizationEnabled(true)
    .requireClassRegistration(false)
    .withRefTracking(true)
    .build()
  ScalaSerializers.registerSerializers(fury)
  output.zipWithIndex.foreach { (sim, i) => 
    val fileName = sim.simStats.toFileName()
    val contents = fury.serialize(sim)
    val writer = FileOutputStream(s"$directory/$fileName")
    try
      writer.write(contents)
      writer.flush()
      println(s"Wrote ${i + 1}/${output.size}: $fileName")
    finally
      writer.close()
  }