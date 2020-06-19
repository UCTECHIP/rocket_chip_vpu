PROJECT ?= VPU
PROJECTPACKAGE := $(shell echo $(PROJECT) | tr A-Z a-z)
PROJECTMAIN ?= $(PROJECT)Main
PROJECTVER ?= $(PROJECT)Verilog
PROJECTREPL ?= $(PROJECT)Repl
SBT ?= sbt -J-Xmx8G -J-Xss32M 

help:
	@echo "usage: make main PROJECT=..."
	@echo "   OR: make verilog PROJECT=..."
	@echo "   OR: make repl PROJECT=..."

main:
	$(SBT) 'test:runMain $(PROJECTPACKAGE).$(PROJECTMAIN) --backend-name verilator'

verilog:
	rm -rf generated_verilog/$(PROJECT)
	$(SBT) 'test:runMain $(PROJECTPACKAGE).$(PROJECTVER) -fsm --target-dir generated_verilog/$(PROJECT)'

repl:
	$(SBT) 'test:runMain $(PROJECTPACKAGE).$(PROJECTREPL)'

.PHONY: help main verilog repl
