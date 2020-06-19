# rocket_chip_vpu

the repository contains VPU source code writen in Chisel, corresponding to rocket-chip and patch for rocket-chip.
* VPU implementation is base on 0.8-release [RVV SPEC](https://github.com/riscv/riscv-v-spec/releases/tag/0.8).
* patch for rocket-chip is base on commit [26a18fa6](https://github.com/chipsalliance/rocket-chip/tree/26a18fa60e96daa1bfc0229630d25a60d700c0bf).

## Usage
### checkout commit 26a18fa6 of rocket-chip

```
$ git clone https://github.com/chipsalliance/rocket-chip.git
$ cd rocket-chip
$ git checkout 26a18fa6
$ git submodule update --init --recursive
```

### set up rocket-chip environment

git clone [rocket-tools](https://github.com/chipsalliance/rocket-tools), then do as README.md of rocket-chip describes: 
* Install [rocket-tools "Ubuntu Packages Needed"](https://github.com/chipsalliance/rocket-tools/blob/master/README.md); 
* Install rocket-tools
* Install [chisel3 "dependancies": sbt, verilator(optional)](https://github.com/freechipsproject/chisel3/blob/master/SETUP.md)

### copy and appply the patch to rocket-chip

```
$ cd rocket_chip_vpu
$ cp 0001-integrate-VPU-which-is-corresponding-to-0.8-release-.patch /path/to/clone/rocket-chip
$ cd /path/to/clone/rocket-chip
$ git am 0001-integrate-VPU-which-is-corresponding-to-0.8-release-.patch
```

### make verilog
first copy vpu folder to rocket-chip
```
$ cp vpu /path/to/clone/rocket-chip
```

then make verilog
```
$ cd /patch/to/clone/rocket-chip/vsim
$ make verilog CONFIG=freechips.rocketchip.system.VPUConfig
```

## compile VPU only(optional)

VPU depends on [hardfloat](https://github.com/ucb-bar/berkeley-hardfloat) repo. To compile VPU, the most direct way is to copy all scala files to vpu/src/main/scala/hardfloat
```
$ cd vpu
$ mkdir -p src/scala/main/hardfloat
$ cp /path/to/clone/hardfloat/src/main/scala/* src/scala/main/hardfloat
$ make verilog
```
