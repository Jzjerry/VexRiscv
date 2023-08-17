RTL = VexRiscv.v
SCALA_SRC = src/main/scala/vexriscv/dse/DSEGenConfig.scala
# SBT_RUN = "runMain vexriscv.dse.DSEGenVexriscv"
SBT_RUN = "runMain vexriscv.dse.GenDSEVexRiscvFromConfig"
SBT_TEST = "testOnly vexriscv.TestDSEGenFromFile"
JSON = DSEConfig.json

LOG_PATH = ../log

# ${RTL}: ${SCALA_FILE}
# 	sbt ${SBT_RUN}

all: test ${RTL}

all-tflite: test-tflite ${RTL}

${RTL}: ${JSON}
	sbt ${SBT_RUN}

test: ${JSON}
	sbt ${SBT_TEST}

test-tflite: export VEXRISCV_USE_TFLITE_BENCHMARKS = yes
test-tflite: export VEXRISCV_TFLITE_MODEL = ${TFLITE_BENCH}
test-tflite: ${JSON}
	sbt ${SBT_TEST}

clean: 
	rm -rf ${RTL}
	rm -rf *.bin

.PHONY: clean all test