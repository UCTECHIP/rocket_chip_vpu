# rocket_chip_vpu

the repository contains VPU source code writen in Chisel, corresponding to rocket-chip and patch for rocket-chip.
* VPU implementation is base on 0.8-release [RVV SPEC][spec].
* patch for rocket-chip is base on tag [v1.2.6][tag].

## Usage
### checkout tag [v1.2.6][tag] of rocket-chip

```
  $ git clone https://github.com/chipsalliance/rocket-chip.git
  $ cd rocket-chip
  $ git checkout v1.2.6
  $ git submodule update --init --recursive
```

### Set up rocket-chip environment (for Ubuntu)
#### Install riscv-gnu-toolchain

checkout riscv-gnu-toolchain rvv-0.8.x branch
```
  $ git clone https://github.com/riscv/riscv-gnu-toolchain.git
  $ cd riscv-gnu-toolchain
  $ git checkout -b rvv-0.8.x
  $ git submodule update --init --recursive
```

install riscv-gnu-toolchain [prerequisites][pre] describes: 
```
  $ sudo apt-get install autoconf automake autotools-dev curl libmpc-dev libmpfr-dev libgmp-dev gawk build-essential bison flex texinfo gperf libtool patchutils bc zlib1g-dev libexpat-dev
```

install riscv-gnu-toolchain
```
  $ export RISCV=/path/to/install/riscv-gnu-toolchain
  $ ./configure --prefix=$RISCV
  $ make
```
#### Set up environment for chisel3
[install sbt for chisel3][env]
```
  $ sudo apt-get install default-jdk
  $ echo "deb https://dl.bintray.com/sbt/debian /" | sudo tee -a /etc/  apt/$ sources.list.d/sbt.list$   
  $ sudo apt-key adv --keys$ erve  r hkp://keyserver.ubuntu.com:80 --rec  v $   642AC823$   
  $ sudo apt-get update
  $ sudo apt-get install sbt
``` 

[install verilator][env], optional, which will be auto installed when running simulation in rocket-chip/emulator
```
  $ sudo apt-get install git make autoconf g++ flex bison
  $ git clone http://git.veripool.org/git/verilator
  $ git pull
  $ git checkout v4.016
  $ unset VERILATOR_ROOT
  $ autoconf
  $ ./configure
  $ make
  $ sudo make install
```

### copy and apply the patch to rocket-chip
```
  $ cd rocket_chip_vpu
  $ cp 0001-feat-integrate-VPU-which-is-based-on-0.8-release-SPE.patch /path/to/clone/rocket-chip
  $ cd /path/to/clone/rocket-chip
  $ git am 0001-feat-integrate-VPU-which-is-based-on-0.8-release-SPE.patch
```

### make verilog
first copy vpu folder to rocket-chip
```
  $ cp -R vpu /path/to/clone/rocket-chip
```

then make verilog
```
  $ cd /patch/to/clone/rocket-chip/vsim
  $ make verilog CONFIG=VPUConfig
```

## compile VPU only (optional)

VPU depends on [hardfloat][hf] repo.  
To compile VPU, there are two ways to supply source files.

* the first way is to copy all scala files to vpu/src/main/scala/hardfloat
```
  $ cd vpu
  $ mkdir -p src/scala/main/hardfloat
  $ cp /path/to/clone/hardfloat/src/main/scala/* src/scala/main/hardfloat
  $ make verilog
```

* the second way is to depend on remote repo, which is writen in build.sbt
```
  $ cd vpu
  $ make verilog
```

[spec]: https://github.com/riscv/riscv-v-spec/releases/tag/0.8
[tag]: https://github.com/chipsalliance/rocket-chip/tree/v1.2.6
[pre]: https://github.com/riscv/riscv-gnu-toolchain/blob/rvv-0.8.x/README.md
[env]: https://github.com/freechipsproject/chisel3/blob/master/SETUP.md
[hf]: https://github.com/ucb-bar/berkeley-hardfloat