package vexriscv

import java.io.{File, OutputStream}
import java.util.concurrent.{ForkJoinPool, TimeUnit}
import org.apache.commons.io.FileUtils
import org.scalatest.{BeforeAndAfterAll, ParallelTestExecution, Tag, Transformer}
import org.scalatest.funsuite.AnyFunSuite

import spinal.core._
import spinal.lib.DoCmd
import vexriscv.demo._
import vexriscv.ip.{DataCacheConfig, InstructionCacheConfig}
import vexriscv.plugin._

import vexriscv.dse._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.sys.process._
import scala.util.parsing.json._
import scala.util.Random

import scala.io._


class TestDSEGenFromFile extends MultithreadedFunSuite(sys.env.getOrElse("VEXRISCV_REGRESSION_THREAD_COUNT", "0").toInt) {
  val seed = sys.env.getOrElse("VEXRISCV_REGRESSION_SEED", Random.nextLong().toString).toLong
  val linuxRegression = sys.env.getOrElse("VEXRISCV_REGRESSION_LINUX_REGRESSION", "no")
  val coremarkRegression = sys.env.getOrElse("VEXRISCV_REGRESSION_COREMARK", "yes")
  val zephyrCount = sys.env.getOrElse("VEXRISCV_REGRESSION_ZEPHYR_COUNT", "4")
  val stopOnError = sys.env.getOrElse("VEXRISCV_REGRESSION_STOP_ON_ERROR", "yes")
  val lock = new{}


  var clockCounter = 0l
  var startAt = System.currentTimeMillis()
  def doTest(positionsToApply : List[dse.VexRiscvPosition], prefix : String = "", testSeed : Int, universes : mutable.HashSet[dse.VexRiscvUniverse]): Unit ={
    val noMemory = universes.contains(dse.VexRiscvUniverse.NO_MEMORY)
    val noWriteback = universes.contains(dse.VexRiscvUniverse.NO_WRITEBACK)
    val name = (if(noMemory) "noMemoryStage_" else "") + (if(noWriteback) "noWritebackStage_" else "") + positionsToApply.map(d => d.dimension.name + "_" + d.name).mkString("_")
    val workspace = "simWorkspace"
    val project = s"$workspace/$prefix"
    def doCmd(cmd: String): String = {
      val stdOut = new StringBuilder()
      class Logger extends ProcessLogger {
        override def err(s: => String): Unit = {
          if (!s.startsWith("ar: creating ")) println(s)
        }
        override def out(s: => String): Unit = {
          println(s)
          stdOut ++= s
        }
        override def buffer[T](f: => T) = f
      }
      Process(cmd, new File(project)).!(new Logger)
      stdOut.toString()
    }

    testMp(prefix + name) {
      println("START TEST " + prefix + name)

      //Cleanup
      FileUtils.deleteDirectory(new File(project))
      FileUtils.forceMkdir(new File(project))

      //Generate RTL
      FileUtils.deleteQuietly(new File("VexRiscv.v"))
        println(s"Configuration=$name")
        //Generate RTL
        SpinalConfig(targetDirectory = project).generateVerilog({
            val config = VexRiscvConfig(
              withMemoryStage = !noMemory,
              withWriteBackStage = !noWriteback,
              plugins = List(
                new DBusSimplePlugin(
                  catchAddressMisaligned = false,
                  catchAccessFault = false,
                  earlyInjection = false
                ),
                new DecoderSimplePlugin(
                  catchIllegalInstruction = false
                ),
                new IntAluPlugin,
                new YamlPlugin("cpu0.yaml"))
            )
            for (positionToApply <- positionsToApply) positionToApply.applyOn(config)
            new VexRiscv(config)
        })

      //Setup test
      val files = List("main.cpp", "jtag.h", "encoding.h" ,"makefile", "dhrystoneO3.logRef", "dhrystoneO3C.logRef","dhrystoneO3MC.logRef","dhrystoneO3M.logRef")
      files.foreach(f => FileUtils.copyFileToDirectory(new File(s"src/test/cpp/regression/$f"), new File(project)))

      //Test RTL
      val debug = true
      val stdCmd = (s"make run REGRESSION_PATH=../../src/test/cpp/regression VEXRISCV_FILE=VexRiscv.v WITH_USER_IO=no REDO=10 TRACE=${if(debug) "yes" else "no"} TRACE_START=100000000000ll FLOW_INFO=no STOP_ON_ERROR=$stopOnError DHRYSTONE=yes COREMARK=${coremarkRegression} THREAD_COUNT=1 ") + s" SEED=${testSeed} "
      val default = " MMU=no PMP=no " + "DEBUG_PLUGIN=no " + "CSR=no " + "DBUS=SIMPLE "
      val testCmd = stdCmd + (positionsToApply).map(_.testParam).mkString(" ") + default
      println(testCmd)
      val str = doCmd(testCmd)
      assert(str.contains("REGRESSION SUCCESS") && !str.contains("Broken pipe"))
      val pattern = "Had simulate ([0-9]+)".r
      val hit = pattern.findFirstMatchIn(str)

      lock.synchronized(clockCounter += hit.get.group(1).toLong)
    }
  }

  val rand = new Random(seed)

  testMp("Info"){
    println(s"MAIN_SEED=$seed")
  }
  println(s"Seed=$seed")

  val configFile = Source.fromFile("DSEConfig.json")
  val configJSON = JSON.parseFull(configFile.mkString).get.asInstanceOf[Map[String, Any]]

  val spaces = List(
    new ShiftSpace -> new ShiftConfig(
      configJSON("Shift").asInstanceOf[String]
      ),
    new BranchSpace -> new BranchConfig(
      configJSON("Branch").asInstanceOf[Boolean]
      ),
    new RegFileSpace -> new RegFileConfig(
      configJSON("RegFileAsync").asInstanceOf[Boolean]
      ),
    new HazardSpace -> new HazardConfig(
      configJSON("Hazard").asInstanceOf[String]
      ),
    new SrcSpace -> new SrcConfig(
      configJSON("Src").asInstanceOf[Map[String, Boolean]]("SepAddSub"),
      configJSON("Src").asInstanceOf[Map[String, Boolean]]("ExecInsert")
      ),
    new MulDivSpace -> new MulDivConfig(
      configJSON("MulDiv").asInstanceOf[Map[String, Any]]
      ),
    new IBusSpace -> new IBusConfig(
      configJSON("IBus").asInstanceOf[Map[String, Any]]
      )
  )

  var universe = mutable.HashSet[dse.VexRiscvUniverse]()

  val universeJSON = configJSON("Universe").asInstanceOf[Map[String, Boolean]]
  if(universeJSON("NoWriteBack")) {
    universe += dse.VexRiscvUniverse.NO_WRITEBACK
    if(universeJSON("NoMemory")) universe += dse.VexRiscvUniverse.NO_MEMORY
  }
  if(universeJSON("ExecuteRegfile")) universe += dse.VexRiscvUniverse.EXECUTE_RF
  if(universeJSON("HasMulDiv")) universe += dse.VexRiscvUniverse.FORCE_MULDIV

  var positions : List[dse.VexRiscvPosition] = null

  do{
    positions = spaces.map( d => d._1.configure(universe.toList, d._2))
  }while(!positions.forall(_.isCompatibleWith(positions)))

  doTest(positions,"DSE",0 , universe)

  testSingleThread("report"){
    val time = (System.currentTimeMillis() - startAt)*1e-3
    val clockPerSecond = (clockCounter/time*1e-3).toLong
    println(s"Duration=${(time/60).toInt}mn clocks=${(clockCounter*1e-6).toLong}M clockPerSecond=${clockPerSecond}K")
  }
}