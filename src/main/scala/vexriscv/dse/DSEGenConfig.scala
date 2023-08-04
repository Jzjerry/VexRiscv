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

        override def isCompatibleWith(positions: Seq[ConfigPosition[VexRiscvConfig]]) = executeRf || positions.exists{
          case p : InstructionAnticipatedPosition => p.instructionAnticipatedOk()
          case _ => false
        }
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
      val divUnroll = spconfig.muldivConfig("DivUnroll").asInstanceOf[Double].toInt

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
            val mulUnroll = spconfig.muldivConfig("MulUnroll").asInstanceOf[Double].toInt
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
        case "Buffer" => if(!noMemory && !noWriteBack){
          val buffIn = spconfig.muldivConfig("BuffIn").asInstanceOf[Boolean]
          val buffOut = spconfig.muldivConfig("BuffOut").asInstanceOf[Boolean] 
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

case class IBusConfig(
  ibusConfig : Map[String, Any]
) extends DSEConfig

class IBusSpace extends IBusDimension(rvcRate = 0.5) with DSE{
    
  override def getDimension: ConfigDimension[_] = this

  override def configureImpl(universes: Seq[ConfigUniverse], config : DSEConfig) : VexRiscvPosition = {

    val spconfig = config.asInstanceOf[IBusConfig]
    val ibusConfig = spconfig.ibusConfig
    val compressed = ibusConfig("compressed").asInstanceOf[Boolean]

    val prediction = ibusConfig("prediction") match {
      case "None" => NONE
      case "Static" => STATIC
      case "Dynamic" => DYNAMIC
      case "DynamicTarget" => DYNAMIC_TARGET
      case _ => {
        throw new AssertionError(s"Prediction type ${ibusConfig("prediction")} not supported")
        NONE
      }
    }

    ibusConfig("busType") match {
      case "Simple" => {
        val latency = ibusConfig("latency").asInstanceOf[Double].toInt // 1 - 6
        val injectorStage = ibusConfig("injectorStage").asInstanceOf[Boolean] || latency == 1
        val cmdForkOnSecondStage = ibusConfig("cmdForkOnSecondStage").asInstanceOf[Boolean]
        val cmdForkPersistence = ibusConfig("cmdForkPersistence").asInstanceOf[Boolean]
        new VexRiscvPosition("Simple" + latency + (if(cmdForkOnSecondStage) "S2" else "") + (if(cmdForkPersistence) "P" else "")  + (if(injectorStage) "InjStage" else "") + (if(compressed) "Rvc" else "") + prediction.getClass.getTypeName().replace("$","")) with InstructionAnticipatedPosition{
        override def testParam = "IBUS=SIMPLE" + (if(compressed) " COMPRESSED=yes" else "")
        override def applyOn(config: VexRiscvConfig): Unit = config.plugins += new IBusSimplePlugin(
          resetVector = 0x80000000l,
          cmdForkOnSecondStage = cmdForkOnSecondStage,
          cmdForkPersistence = cmdForkPersistence,
          prediction = prediction,
          catchAccessFault = false, // catchAll default false
          compressedGen = compressed,
          busLatencyMin = latency,
          injectorStage = injectorStage,
          memoryTranslatorPortConfig = null // mmuPort default null
        )
        override def instructionAnticipatedOk() = injectorStage
      }
      }
      case "Cached" => {
        val asyncTagMemory = ibusConfig("asyncTagMemory").asInstanceOf[Boolean]
        val mmuConfig = null

        val catchAll = false
        val tighlyCoupled = ibusConfig("tighlyCoupled").asInstanceOf[Boolean] && !catchAll
        val reducedBankWidth = ibusConfig("reducedBankWidth").asInstanceOf[Boolean]
        val relaxedPcCalculation = ibusConfig("relaxedPcCalculation").asInstanceOf[Boolean]
        val twoCycleCache = ibusConfig("twoCycleCache").asInstanceOf[Boolean]
        val injectorStage = ibusConfig("injectorStage").asInstanceOf[Boolean]
        val twoCycleRam = ibusConfig("twoCycleRam").asInstanceOf[Boolean] && twoCycleCache
        val twoCycleRamInnerMux = ibusConfig("twoCycleRamInnerMux").asInstanceOf[Boolean] && twoCycleRam
        val memDataWidth = ibusConfig("memDataWidth").asInstanceOf[Double].toInt  // 32,64,128
        val bytePerLine = Math.max(memDataWidth/8, ibusConfig("bytePerLine").asInstanceOf[Double].toInt) // 8,16,32,64
        val cacheSize = ibusConfig("cacheSize").asInstanceOf[Double].toInt // 512,1024,2048,4096,8192
        val wayCount = ibusConfig("wayCount").asInstanceOf[Double].toInt  // 1,2,4
        if(cacheSize/wayCount < 512 || (catchAll && cacheSize/  wayCount > 4096)){
          throw new AssertionError(s"Cache size ${cacheSize} / way count ${wayCount} not matched")
        }

        new VexRiscvPosition(s"Cached${memDataWidth}d" + (if  (twoCycleCache) "2cc" else "") + (if(injectorStage) "Injstage"  else "") + (if(twoCycleRam) "2cr" else "")  + "S" + cacheSize +  "W" + wayCount + "BPL" + bytePerLine + (if(relaxedPcCalculation)   "Relax" else "") + (if(compressed) "Rvc" else "") + prediction. getClass.getTypeName().replace("$","")+ (if(tighlyCoupled)"Tc"   else "") + (if(asyncTagMemory) "Atm" else "")) with   InstructionAnticipatedPosition{
          override def testParam = s"IBUS=CACHED  IBUS_DATA_WIDTH=$memDataWidth" + (if(compressed) "   COMPRESSED=yes" else "") + (if(tighlyCoupled)" IBUS_TC=yes"   else "")
          override def applyOn(config: VexRiscvConfig): Unit = {
            val p = new IBusCachedPlugin(
              resetVector = 0x80000000l,
              compressedGen = compressed,
              prediction = prediction,
              relaxedPcCalculation = relaxedPcCalculation,
              injectorStage = injectorStage,
              memoryTranslatorPortConfig = mmuConfig,
              config = InstructionCacheConfig(
                cacheSize = cacheSize,
                bytePerLine = bytePerLine,
                wayCount = wayCount,
                addressWidth = 32,
                cpuDataWidth = 32,
                memDataWidth = memDataWidth,
                catchIllegalAccess = catchAll,
                catchAccessFault = catchAll,
                asyncTagMemory = asyncTagMemory,
                twoCycleRam = twoCycleRam,
                twoCycleCache = twoCycleCache,
                twoCycleRamInnerMux = twoCycleRamInnerMux,
                reducedBankWidth = reducedBankWidth
              )
            )
            if(tighlyCoupled) p.newTightlyCoupledPort (TightlyCoupledPortParameter("iBusTc", a => a(30 downto 28)  === 0x0))
            config.plugins += p
          }
          override def instructionAnticipatedOk() = !twoCycleCache ||   ((!twoCycleRam || wayCount == 1) && !compressed)
        }
      }
      case _ => AssertionPostion
    }
  }
}

case class DBusConfig(
  dbusConfig : Map[String, Any]
) extends DSEConfig

class DBusSpace extends DBusDimension with DSE{

  override def getDimension: ConfigDimension[_] = this

  override def configureImpl(universes: Seq[ConfigUniverse], config: DSEConfig): VexRiscvPosition = {
    val catchAll = false
    val noMemory = universes.contains(VexRiscvUniverse.NO_MEMORY)
    val noWriteBack = universes.contains(VexRiscvUniverse.NO_WRITEBACK)
    val spconfig = config.asInstanceOf[DBusConfig]
    val dbusConfig = spconfig.dbusConfig

    dbusConfig("busType") match {
      case "Simple" => {
        val withLrSc = catchAll
        val earlyInjection = dbusConfig("earlyInjection").asInstanceOf[Boolean] && 
          !universes.contains(VexRiscvUniverse.NO_WRITEBACK)
        new VexRiscvPosition("Simple" + (if(earlyInjection) "Early" else "Late")) {
          override def testParam = "DBUS=SIMPLE " + (if(withLrSc) "LRSC=yes " else "")
          override def applyOn(config: VexRiscvConfig): Unit = config.plugins += new DBusSimplePlugin(
          catchAddressMisaligned = catchAll,
          catchAccessFault = catchAll,
          earlyInjection = earlyInjection,
          memoryTranslatorPortConfig = null,  // default no mmu
          withLrSc = withLrSc
          )
        }
      }
      case "Cached" => {
        if(noMemory) { 
          throw new AssertionError("Cached DBus require memory stage")  
        }
        val mmuConfig = null
        val memDataWidth = dbusConfig("memDataWidth").asInstanceOf[Double].toInt // (32,64,128)
        val cpuDataWidth = dbusConfig("cpuDataWidth").asInstanceOf[Double].toInt // (32,64,128).filter(_ <= memDataWidth)
        if(cpuDataWidth > memDataWidth) throw new AssertionError(s"cpuDataWidth $cpuDataWidth > memDataWidth $memDataWidth")
        val bytePerLine = Math.max(memDataWidth/8, dbusConfig("bytePerLine").asInstanceOf[Double].toInt) // (8,16,32,64)
        val withLrSc = true
        val withSmp = false //withLrSc && r.nextBoolean()
        val withAmo = false // catchAll && r.nextBoolean() || withSmp
        val relaxedMemoryTranslationRegister = dbusConfig("relaxedRegister").asInstanceOf[Boolean]
        val earlyWaysHits = dbusConfig("earlyWaysHits").asInstanceOf[Boolean] && !noWriteBack
        val directTlbHit = false 
        val dBusCmdMasterPipe, dBusCmdSlavePipe, dBusRspSlavePipe = false //As it create test   bench issues
        val asyncTagMemory = dbusConfig("asyncTagMemory").asInstanceOf[Boolean]

        val cacheSize = dbusConfig("cacheSize").asInstanceOf[Double].toInt // (512, 1024, 2048, 4096, 8192)
        val wayCount = dbusConfig("wayCount").asInstanceOf[Double].toInt  // (1, 2, 4, 8)
        if(cacheSize/wayCount < 512 || (catchAll && cacheSize/wayCount >  4096)){
          throw new AssertionError(s"cacheSize $cacheSize / wayCount $wayCount is not supported")
        }

        new VexRiscvPosition(s"Cached${memDataWidth}d${cpuDataWidth}c" + "S"  + cacheSize + "W" + wayCount + "BPL" + bytePerLine + (if (dBusCmdMasterPipe) "Cmp " else "") + (if(dBusCmdSlavePipe) "Csp "   else "") + (if(dBusRspSlavePipe) "Rsp " else "") + (if  (relaxedMemoryTranslationRegister) "Rmtr " else "") + (if (earlyWaysHits) "Ewh " else "") + (if(withAmo) "Amo " else "") + (if (withSmp) "Smp " else "") + (if(directTlbHit) "Dtlb " else "") + (if (false) "Tsmmu " else "") + (if(asyncTagMemory) "Atm" else   "")) {
          override def testParam = s"DBUS=CACHED  DBUS_LOAD_DATA_WIDTH=$memDataWidth   DBUS_STORE_DATA_WIDTH=$cpuDataWidth " + (if(withLrSc) "LRSC=yes "   else "")  + (if(withAmo) "AMO=yes " else "")  + (if(withSmp)  "DBUS_EXCLUSIVE=yes DBUS_INVALIDATE=yes " else "")

          override def applyOn(config: VexRiscvConfig): Unit = {
            config.plugins += new DBusCachedPlugin(
              config = new DataCacheConfig(
                cacheSize = cacheSize,
                bytePerLine = bytePerLine,
                wayCount = wayCount,
                addressWidth = 32,
                cpuDataWidth = cpuDataWidth, //Not tested
                memDataWidth = memDataWidth,
                catchAccessError = catchAll,
                catchIllegal = catchAll,
                catchUnaligned = catchAll,
                withLrSc = withLrSc,
                withAmo = withAmo,
                earlyWaysHits = earlyWaysHits,
                withExclusive = withSmp,
                withInvalidate = withSmp,
                directTlbHit = directTlbHit,
                asyncTagMemory = asyncTagMemory
              ),
              dBusCmdMasterPipe = dBusCmdMasterPipe,
              dBusCmdSlavePipe = dBusCmdSlavePipe,
              dBusRspSlavePipe = dBusRspSlavePipe,
              relaxedMemoryTranslationRegister =  relaxedMemoryTranslationRegister,
              memoryTranslatorPortConfig = mmuConfig
            )
          }
        }
      }
      case _ => AssertionPostion
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
     SpinalConfig()
    //  .addStandardMemBlackboxing(blackboxAllWhatsYouCan)
     .generateVerilog({
        val config = VexRiscvConfig(
          withMemoryStage = !noMemory,
          withWriteBackStage = !noWriteback,
          plugins = List(
            new DecoderSimplePlugin(
              catchIllegalInstruction = false
            ),
            new StaticMemoryTranslatorPlugin(
              ioRange = _ (31 downto 28) === 0xF
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
      ),
    new IBusSpace -> new IBusConfig(
      configJSON("IBus").asInstanceOf[Map[String, Any]]
      ),
    new DBusSpace -> new DBusConfig(
      configJSON("DBus").asInstanceOf[Map[String, Any]]
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

  if(sys.env.getOrElse("DSE_RANDOM", "false").toBoolean){
    val rand = new Random(42)
    do{
      positions = spaces.map(d => d._1.randomPosition(universe.toList, rand))
    }while(!positions.forall(_.isCompatibleWith(positions)))
  }else{
    positions = spaces.map( d => d._1.configure(universe.toList, d._2))
    if(!positions.forall(_.isCompatibleWith(positions)))
      throw new AssertionError("Positions are incompatible!")
    }
  doGen(positions, universe)
}