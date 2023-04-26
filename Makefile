RTL = VexRiscv.v
SCALA_SRC = src/main/scala/vexriscv/dse/DSEGenConfig.scala
# SBT_RUN = "runMain vexriscv.dse.DSEGenVexriscv"
SBT_RUN = "runMain vexriscv.dse.GenRandomVexriscv"

LOG_PATH = ../log

# ${RTL}: ${SCALA_FILE}
# 	sbt ${SBT_RUN}

all: clean ${RTL}

${RTL}: ${SCALA_SRC}
	sbt ${SBT_RUN}

clean: 
	rm -rf ${RTL}
	rm -rf *.bin

.PHONY: clean all