RTL = VexRiscv.v
SCALA_FILE = src/main/scala/vexriscv/dse/DSEGenVexriscv.scala
SBT_RUN = "runMain vexriscv.dse.DSEGenVexriscv"

LOG_PATH = ../log

${RTL}: ${SCALA_FILE}
	sbt ${SBT_RUN}
	
clean: 
	rm -rf ${RTL}

.PHONY: clean