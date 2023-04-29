/* Modified from test/scala/vexriscv/TestIndividualFeatures.scala */
package vexriscv.dse

import spinal.core._
import spinal.lib.DoCmd
import vexriscv.demo._
import vexriscv.ip.{DataCacheConfig, InstructionCacheConfig}
import vexriscv.plugin._
import vexriscv.{plugin, VexRiscv, VexRiscvConfig}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.sys.process._
import scala.util.parsing.json._
import scala.util.Random

import scala.io._

abstract class DSEConfig 

trait DSE {

  def getDimension : ConfigDimension[_] 

  val AssertionPostion = new VexRiscvPosition("Error"){
    override def applyOn(config: VexRiscvConfig): Unit = 
      throw new AssertionError(s"Illegal Configuration in ${getDimension.name}")
  }
  def configure(universes: Seq[ConfigUniverse], config : DSEConfig) : VexRiscvPosition = {
    val pos = configureImpl(universes, config)
    pos.dimension = getDimension
    pos
  }
  def configureImpl(universes: Seq[ConfigUniverse], config : DSEConfig) : VexRiscvPosition
}

case class ShiftConfig(shift : String) extends DSEConfig

class ShiftSpace extends ShiftDimension with DSE{

  override def getDimension: ConfigDimension[_] = this

  override def configureImpl(universes: Seq[ConfigUniverse], config : DSEConfig) : VexRiscvPosition = {
    val spconfig = config.asInstanceOf[ShiftConfig]
      spconfig.shift match {
        case "LT"  => new VexRiscvPosition("Light"){
          override def applyOn(config: VexRiscvConfig): Unit = config.plugins += new LightShifterPlugin
        }
        case "FE" => new VexRiscvPosition("FullEarly"){
          override def applyOn(config: VexRiscvConfig): Unit = config.plugins += new FullBarrelShifterPlugin(earlyInjection = true)
        }
        case "FL" => 
          if(!universes.contains(VexRiscvUniverse.NO_MEMORY)){
            new VexRiscvPosition("FullLate"){
              override def applyOn(config: VexRiscvConfig): Unit = config.plugins += new FullBarrelShifterPlugin(earlyInjection = true)
            }
          } else {AssertionPostion}
        case _ => AssertionPostion
      }
    }
}

case class BranchConfig(early : Boolean) extends DSEConfig

class BranchSpace extends BranchDimension with DSE{

  override def getDimension: ConfigDimension[_] = this
  
  override def configureImpl(universes: Seq[ConfigUniverse], config : DSEConfig) : VexRiscvPosition = {
    val spconfig = config.asInstanceOf[BranchConfig]
    val catchAll = universes.contains(VexRiscvUniverse.CATCH_ALL)
    val early = spconfig.early || universes.contains(VexRiscvUniverse.NO_MEMORY)
    new VexRiscvPosition(if(early) "Early" else "Late") {
      override def applyOn(config: VexRiscvConfig): Unit = config.plugins += new BranchPlugin(
        earlyBranch = early,
        catchAddressMisaligned = catchAll
      )
    }
  }
}

case class RegFileConfig(async : Boolean) extends DSEConfig

class RegFileSpace extends RegFileDimension with DSE{
  override def getDimension: ConfigDimension[_] = this

  override def configureImpl(universes: Seq[ConfigUniverse], config : DSEConfig) : VexRiscvPosition = {
    val spconfig = config.asInstanceOf[RegFileConfig]
    val executeRf = universes.contains(VexRiscvUniverse.EXECUTE_RF)
    if(spconfig.async){
      new VexRiscvPosition("Async" + (if(executeRf) "ER" else "DR")) {
        override def applyOn(config: VexRiscvConfig): Unit = config.plugins += new RegFilePlugin(
          regFileReadyKind = plugin.ASYNC,
          zeroBoot = true,
          readInExecute = executeRf
        )
      }
    } else{
      new VexRiscvPosition("Sync" + (if(executeRf) "ER" else "DR")) {
        override def applyOn(config: VexRiscvConfig): Unit = config.plugins += new RegFilePlugin(
          regFileReadyKind = plugin.SYNC,
          zeroBoot = true,
          readInExecute = executeRf
        )
      }
    }
  }
}

case class HazardConfig(hazardType : String) extends DSEConfig

class HazardSpace extends HazardDimension with DSE{
  override def getDimension: ConfigDimension[_] = this

  override def configureImpl(universes: Seq[ConfigUniverse], config : DSEConfig) : VexRiscvPosition = {
    val spconfig = config.asInstanceOf[HazardConfig]
      spconfig.hazardType match {
        case "IL"  => new VexRiscvPosition("Interlock") {
          override def applyOn(config: VexRiscvConfig): Unit = config.plugins += new HazardSimplePlugin(
            bypassExecute = false,
            bypassMemory = false,
            bypassWriteBack = false,
            bypassWriteBackBuffer = false,
            pessimisticUseSrc = false,
            pessimisticWriteRegFile = false,
            pessimisticAddressMatch = false
          )
        }
        case "BA"  => new VexRiscvPosition("BypassAll") {
          override def applyOn(config: VexRiscvConfig): Unit = config.plugins += new HazardSimplePlugin(
            bypassExecute = true,
            bypassMemory = true,
            bypassWriteBack = true,
            bypassWriteBackBuffer = true,
            pessimisticUseSrc = false,
            pessimisticWriteRegFile = false,
            pessimisticAddressMatch = false
          )
        }
        case "BE" => new VexRiscvPosition("BypassExecute") {
          override def applyOn(config: VexRiscvConfig): Unit = config.plugins += new HazardSimplePlugin(
            bypassExecute = true,
            bypassMemory = false,
            bypassWriteBack = false,
            bypassWriteBackBuffer = false,
            pessimisticUseSrc = false,
            pessimisticWriteRegFile = false,
            pessimisticAddressMatch = false
          )
        }
        case "BM" => new VexRiscvPosition("BypassMemory") {
          override def applyOn(config: VexRiscvConfig): Unit = config.plugins += new HazardSimplePlugin(
            bypassExecute = false,
            bypassMemory = true,
            bypassWriteBack = false,
            bypassWriteBackBuffer = false,
            pessimisticUseSrc = false,
            pessimisticWriteRegFile = false,
            pessimisticAddressMatch = false
          )
        }
        case "BW" => new VexRiscvPosition("BypassWriteBack") {
          override def applyOn(config: VexRiscvConfig): Unit = config.plugins += new HazardSimplePlugin(
            bypassExecute = false,
            bypassMemory = false,
            bypassWriteBack = true,
            bypassWriteBackBuffer = false,
            pessimisticUseSrc = false,
            pessimisticWriteRegFile = false,
            pessimisticAddressMatch = false
          )
        }
        case "BWB" => new VexRiscvPosition("BypassWriteBackBuffer") {
          override def applyOn(config: VexRiscvConfig): Unit = config.plugins += new HazardSimplePlugin(
            bypassExecute = false,
            bypassMemory = false,
            bypassWriteBack = false,
            bypassWriteBackBuffer = true,
            pessimisticUseSrc = false,
            pessimisticWriteRegFile = false,
            pessimisticAddressMatch = false
          )
        }
        case _ => AssertionPostion
      }
  }
}

case class SrcConfig(
  separatedAddSub : Boolean,
  executeInsertion : Boolean) extends DSEConfig

class SrcSpace extends SrcDimension with DSE{

  override def getDimension: ConfigDimension[_] = this

  override def configureImpl(universes: Seq[ConfigUniverse], config : DSEConfig) : VexRiscvPosition = {
    val spconfig = config.asInstanceOf[SrcConfig]
    val executeRf = universes.contains(VexRiscvUniverse.EXECUTE_RF)
    val separatedAddSub = spconfig.separatedAddSub
    val executeInsertion = universes.contains(VexRiscvUniverse.EXECUTE_RF) || spconfig.executeInsertion
    new VexRiscvPosition((if (separatedAddSub) "AddSub" else "") + (if (executeInsertion) "Execute" else "")) {
      override def applyOn(config: VexRiscvConfig): Unit = config.plugins += new SrcPlugin(
        separatedAddSub = separatedAddSub,
        executeInsertion = executeInsertion
      )
    }
  }
}

case class MulDivConfig(muldivConfig : Map[String, Any]) extends DSEConfig

class MulDivSpace extends MulDivDimension with DSE{
  override def getDimension: ConfigDimension[_] = this

  override def configureImpl(universes: Seq[ConfigUniverse], config : DSEConfig) : VexRiscvPosition = {

    val spconfig = config.asInstanceOf[MulDivConfig]
    val hasMulDiv = universes.contains(VexRiscvUniverse.FORCE_MULDIV)
    val noMemory = universes.contains(VexRiscvUniverse.NO_MEMORY)
    val noWriteBack = universes.contains(VexRiscvUniverse.NO_WRITEBACK)

    if(hasMulDiv){
      val mulUnroll = spconfig.muldivConfig("MulUnroll").asInstanceOf[Double].toInt
      val divUnroll = spconfig.muldivConfig("DivUnroll").asInstanceOf[Double].toInt
      val buffIn = spconfig.muldivConfig("BuffIn").asInstanceOf[Boolean]
      val buffOut = spconfig.muldivConfig("BuffOut").asInstanceOf[Boolean]

      spconfig.muldivConfig("MulType").asInstanceOf[String] match {
        case "Simple" => new VexRiscvPosition(s"MulSimple_Div$divUnroll") {
          override def testParam = "MUL=yes DIV=yes"
          override def applyOn(config: VexRiscvConfig): Unit = {
            config.plugins += new MulSimplePlugin
            config.plugins += new MulDivIterativePlugin(
              genMul = false,
              genDiv = true,
              mulUnrollFactor = 32,
              divUnrollFactor = divUnroll
            )
          } 
        }
        case "Iterative" => if(!noMemory) {
            new VexRiscvPosition(s"Mul${mulUnroll}_Div${divUnroll}") {
            override def testParam = "MUL=yes DIV=yes"
            override def applyOn(config: VexRiscvConfig): Unit = {
              config.plugins += new MulDivIterativePlugin(
                genMul = true,
                genDiv = true,
                mulUnrollFactor = mulUnroll,
                divUnrollFactor = divUnroll
              )
            }
          }
        } else AssertionPostion
        case "Buffer" => if(!noMemory && !noWriteBack) 
        new VexRiscvPosition(s"MulDivBuf$buffIn$buffOut"){
          override def testParam = "MUL=yes DIV=yes"
          override def applyOn(config: VexRiscvConfig): Unit = {
            config.plugins += new MulPlugin(
              inputBuffer = buffIn,
              outputBuffer = buffOut
            )
            config.plugins += new MulDivIterativePlugin(
              genMul = false,
              genDiv = true,
              mulUnrollFactor = 32,
              divUnrollFactor = divUnroll
            )
          }
        } else AssertionPostion
        case "Mul16" => if(!noMemory && !noWriteBack)
        new VexRiscvPosition("MulDivFpga16BitsDsp") {
            override def testParam = "MUL=yes DIV=yes"
            override def applyOn(config: VexRiscvConfig): Unit = {
              config.plugins += new Mul16Plugin
              config.plugins += new MulDivIterativePlugin(
                genMul = false,
                genDiv = true,
                mulUnrollFactor = 32,
                divUnrollFactor = divUnroll
              )}
        } else AssertionPostion
        case _ => AssertionPostion
      }

    } else{
      new VexRiscvPosition("None") {
        override def applyOn(config: VexRiscvConfig): Unit = {}
        override def testParam = "MUL=no DIV=no"
      }
    }

  
  }

}

object GenDSEVexRiscvFromConfig extends App {

  def doGen(positionsToApply : List[VexRiscvPosition], universes : mutable.HashSet[VexRiscvUniverse]): Unit ={

    val noMemory = universes.contains(VexRiscvUniverse.NO_MEMORY)
    val noWriteback = universes.contains(VexRiscvUniverse.NO_WRITEBACK)
    val name = (if(noMemory) "noMemoryStage_" else "") + (if(noWriteback) "noWritebackStage_" else "") + positionsToApply.map(d => d.dimension.name + "_" + d.name).mkString("_")
    println(s"Configuration=$name")
    //Generate RTL
    SpinalVerilog({
        val config = VexRiscvConfig(
          withMemoryStage = !noMemory,
          withWriteBackStage = !noWriteback,
          plugins = List(
            new IBusSimplePlugin(
              resetVector = 0x80000000l,
              cmdForkOnSecondStage = false,
              cmdForkPersistence = false,
              prediction = NONE,
              catchAccessFault = false,
              compressedGen = false
            ),
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
    }

  
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
      )
  )

  var universe = mutable.HashSet[VexRiscvUniverse]()

  val universeJSON = configJSON("Universe").asInstanceOf[Map[String, Boolean]]
  if(universeJSON("NoWriteBack")) {
    universe += VexRiscvUniverse.NO_WRITEBACK
    if(universeJSON("NoMemory")) universe += VexRiscvUniverse.NO_MEMORY
  }
  if(universeJSON("ExecuteRegfile")) universe += VexRiscvUniverse.EXECUTE_RF
  if(universeJSON("HasMulDiv")) universe += VexRiscvUniverse.FORCE_MULDIV

  var positions : List[VexRiscvPosition] = null

  do{
    positions = spaces.map( d => d._1.configure(universe.toList, d._2))
  }while(!positions.forall(_.isCompatibleWith(positions)))

  doGen(positions, universe)
}