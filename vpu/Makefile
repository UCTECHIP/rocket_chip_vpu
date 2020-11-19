PROJECT ?=
PROJECT_CIRCUIT_NAME := $(PROJECT)
PROJECT_CIRCUIT_PATH := src/main/scala/$(PROJECT)
PROJECT_CIRCUIT_FILE := $(PROJECT_CIRCUIT_PATH)/$(PROJECT_CIRCUIT_NAME).scala

PROJECT_TESTBENCH_NAME := $(PROJECT)Tests
PROJECT_TESTBENCH_PATH := src/test/scala/$(PROJECT)
PROJECT_TESTBENCH_FILE := $(PROJECT_TESTBENCH_PATH)/$(PROJECT_TESTBENCH_NAME).scala

VERILOG_LAUNCHER_FILE := src/test/scala/utils/VerilogLauncher.scala
VERILOG_MAPPING := \"$(PROJECT_CIRCUIT_NAME)\" -> { (args: Array[String]) => (new chisel3.stage.ChiselStage).execute(args, Seq(ChiselGeneratorAnnotation(() => new $(PROJECT_CIRCUIT_NAME)(customConfig)))) }
VERILOG_MAPPING_EXIST := $(shell grep \"$(PROJECT_CIRCUIT_NAME)\" $(VERILOG_LAUNCHER_FILE) > /dev/null && echo "Y" || echo "N")

TEST_LAUNCHER_FILE := src/test/scala/utils/TestLauncher.scala
TEST_MAPPING := \"$(PROJECT_CIRCUIT_NAME)\" -> { (manager: TesterOptionsManager) => Driver.execute(() => new $(PROJECT_CIRCUIT_NAME)(customConfig), manager){(c) => new $(PROJECT_TESTBENCH_NAME)(c)} }
TEST_MAPPING_EXIST := $(shell grep \"$(PROJECT_CIRCUIT_NAME)\" $(TEST_LAUNCHER_FILE) > /dev/null && echo "Y" || echo "N")

REPL_LAUNCHER_FILE := src/test/scala/utils/ReplLauncher.scala
REPL_MAPPING := \"$(PROJECT_CIRCUIT_NAME)\" -> { (manager: ReplOptionsManager) => Driver.executeFirrtlRepl(() => new $(PROJECT_CIRCUIT_NAME)(customConfig), manager) }
REPL_MAPPING_EXIST := $(shell grep \"$(PROJECT_CIRCUIT_NAME)\" $(REPL_LAUNCHER_FILE) > /dev/null && echo "Y" || echo "N")
SBT ?= sbt -J-Xmx2G -J-Xss32M

help:
	@echo "to create a new project:          make create PROJECT=..."
	@echo "to see what projects can be test: make test"
	@echo "to test a project:                make test PROJECT=..."
	@echo "to show detail during testing:    make detail PROJECT=..."
	@echo "to generate vcd waveform:         make vcd PROJECT=..."
	@echo "just to generate verilog file:    make verilog PROJECT=..."
	@echo "to test using verilator:          make verilator PROJECT=..."
	@echo "to test using treadle:            make treadle PROJECT=..."
	@echo "to clean generated files:         make clean PROJECT=..."
	@echo "to clean all generated files:     make clean-all"

create: create_circuit create_testbench create_launcher

create_circuit:
ifdef PROJECT
	@test ! -d $(PROJECT_CIRCUIT_PATH) && mkdir $(PROJECT_CIRCUIT_PATH) || echo "Circuit file already exist"
	@if [ ! -f $(PROJECT_CIRCUIT_FILE) ]; \
	then \
		echo "// See LICENSE.UCTECHIP for license details."                                            >> $(PROJECT_CIRCUIT_FILE); \
		echo "// See LICENSE.SZU for license details."                                                 >> $(PROJECT_CIRCUIT_FILE); \
		echo "/************************************************************"                           >> $(PROJECT_CIRCUIT_FILE); \
		echo "*"                                                                                       >> $(PROJECT_CIRCUIT_FILE); \
		echo "*       Filename        :       $(PROJECT_CIRCUIT_NAME).scala"                           >> $(PROJECT_CIRCUIT_FILE); \
		echo "*       Author          :       author"                                                  >> $(PROJECT_CIRCUIT_FILE); \
		echo "*       Revision        :       first version"                                           >> $(PROJECT_CIRCUIT_FILE); \
		echo "*       Description     :       "                                                        >> $(PROJECT_CIRCUIT_FILE); \
		echo "*"                                                                                       >> $(PROJECT_CIRCUIT_FILE); \
		echo "*       io.en           :       input, control, (description)"                           >> $(PROJECT_CIRCUIT_FILE); \
		echo "*       io.din          :       input[width-1:0], data, (description)"                   >> $(PROJECT_CIRCUIT_FILE); \
		echo "*       io.dout         :       output[width-1:0], data, (description)"                  >> $(PROJECT_CIRCUIT_FILE); \
		echo "*"                                                                                       >> $(PROJECT_CIRCUIT_FILE); \
		echo "************************************************************/"                           >> $(PROJECT_CIRCUIT_FILE); \
		echo "package vpu"                                                                             >> $(PROJECT_CIRCUIT_FILE); \
		echo ""                                                                                        >> $(PROJECT_CIRCUIT_FILE); \
		echo "import chisel3._"                                                                        >> $(PROJECT_CIRCUIT_FILE); \
		echo "import chisel3.util._"                                                                   >> $(PROJECT_CIRCUIT_FILE); \
		echo ""                                                                                        >> $(PROJECT_CIRCUIT_FILE); \
		echo "class $(PROJECT_CIRCUIT_NAME) extends Module {"                                          >> $(PROJECT_CIRCUIT_FILE); \
		echo "  val io = IO(new Bundle {"                                                              >> $(PROJECT_CIRCUIT_FILE); \
		echo ""                                                                                        >> $(PROJECT_CIRCUIT_FILE); \
		echo "  })"                                                                                    >> $(PROJECT_CIRCUIT_FILE); \
		echo ""                                                                                        >> $(PROJECT_CIRCUIT_FILE); \
		echo "}"                                                                                       >> $(PROJECT_CIRCUIT_FILE); \
	fi
endif

create_testbench:
ifdef PROJECT
	@test ! -d $(PROJECT_TESTBENCH_PATH) && mkdir $(PROJECT_TESTBENCH_PATH) || echo "Testbench file already exist"
	@if [ ! -f $(PROJECT_TESTBENCH_FILE) ]; \
	then \
		echo "// See LICENSE.UCTECHIP for license details."                                            >> $(PROJECT_TESTBENCH_FILE); \
		echo "// See LICENSE.SZU for license details."                                                 >> $(PROJECT_TESTBENCH_FILE); \
		echo "package vpu"                                                                             >> $(PROJECT_TESTBENCH_FILE); \
		echo ""                                                                                        >> $(PROJECT_TESTBENCH_FILE); \
		echo "import chisel3.iotesters._"                                                              >> $(PROJECT_TESTBENCH_FILE); \
		echo ""                                                                                        >> $(PROJECT_TESTBENCH_FILE); \
		echo "class $(PROJECT_TESTBENCH_NAME)(c: $(PROJECT_CIRCUIT_NAME)) extends PeekPokeTester(c) {" >> $(PROJECT_TESTBENCH_FILE); \
		echo ""                                                                                        >> $(PROJECT_TESTBENCH_FILE); \
		echo "}"                                                                                       >> $(PROJECT_TESTBENCH_FILE); \
	fi
endif

create_launcher:
ifdef PROJECT
ifeq ($(TEST_MAPPING_EXIST), N)
	@sed -i -e "/->/s/}$$/},\nINSERTHERE/" -e "s/INSERTHERE/    $(TEST_MAPPING)/" $(TEST_LAUNCHER_FILE)
endif
ifeq ($(REPL_MAPPING_EXIST), N)
	@sed -i -e "/->/s/}$$/},\nINSERTHERE/" -e "s/INSERTHERE/    $(REPL_MAPPING)/" $(REPL_LAUNCHER_FILE)
endif
ifeq ($(VERILOG_MAPPING_EXIST), N)
	@sed -i -e "/->/s/}$$/},\nINSERTHERE/" -e "s/INSERTHERE/    $(VERILOG_MAPPING)/" $(VERILOG_LAUNCHER_FILE)
endif
endif


test: clean
#user can directly use this command to run multiple projects
#e.g. sbt 'test:runMain vpu.TestLauncher project1 project2 ...'
	$(SBT) 'test:runMain vpu.TestLauncher $(PROJECT)'

detail: clean
	$(SBT) 'test:runMain vpu.TestLauncher $(PROJECT) --is-verbose'

vcd: clean
	$(SBT) 'test:runMain vpu.TestLauncher $(PROJECT) --generate-vcd-output on'

verilog: clean
	$(SBT) 'test:runMain vpu.VerilogLauncher $(PROJECT)'

verilator: clean
	$(SBT) 'test:runMain vpu.TestLauncher $(PROJECT) --backend-name verilator'

treadle: clean
	$(SBT) 'test:runMain vpu.TestLauncher $(PROJECT) --backend-name treadle'

clean:
	@test $(PROJECT) && rm -rf test_run_dir/$(PROJECT) || echo "nothing to clean"

clean-all:
	rm -rf test_run_dir target project/project project/target

.PHONY: help test detail vcd verilog verilator treadle clean clean-all
.PHONY: create create_circuit create_testbench create_launcher
