package vpu

import chisel3._

object VPUVerilog extends App {
  val SeqmulDiv = Some(Array(
    VMulDivParams(mulUnroll = 2,   divUnroll = 2,   mulEarlyOut = true, divEarlyOut = true),
    VMulDivParams(mulUnroll = 4,   divUnroll = 4,   mulEarlyOut = true, divEarlyOut = true),
    VMulDivParams(mulUnroll = 8,   divUnroll = 8,   mulEarlyOut = true, divEarlyOut = true),
    VMulDivParams(mulUnroll = 16,  divUnroll = 16,  mulEarlyOut = true, divEarlyOut = true),
    VMulDivParams(mulUnroll = 32,  divUnroll = 32,  mulEarlyOut = true, divEarlyOut = true),
    VMulDivParams(mulUnroll = 64,  divUnroll = 64,  mulEarlyOut = true, divEarlyOut = true),
    VMulDivParams(mulUnroll = 128, divUnroll = 128, mulEarlyOut = true, divEarlyOut = true),
    VMulDivParams(mulUnroll = 256, divUnroll = 256, mulEarlyOut = true, divEarlyOut = true)
  ))
  val minConfig = VPUParams(VLEN     = 64, 
                            ELEN     = 64, 
                            XLEN     = 64, 
                            FLEN     = 64, 
                            ZFINX    = false, 
                            FSEW16   = false, 
                            FSEWMAX  = 64, 
                            LMULMAX  = 1, 
                            MERGE    = true, 
                            MULDIV   = true, 
                            MULADD   = true, 
                            QMULADD  = true, 
                            mulDiv   = SeqmulDiv, 
                            RED      = true, 
                            MV       = true, 
                            SATADD   = true, 
                            AVERADD  = true, 
                            SATMUL   = true, 
                            SCALESR  = true, 
                            NCLIP    = true, 
                            SLIDE    = true, 
                            GATHER   = true, 
                            COMPRESS = true, 
                            COPY     = true, 
                            FMA      = true, 
                            FCVT     = true, 
                            FCMP     = true, 
                            FSGNJ    = true, 
                            FCLASS   = true, 
                            FMERGE   = true, 
                            FMV      = true, 
                            FDIVSQRT = true, 
                            FRED     = true, 
                            SEGLS    = true, 
                            AMO      = true, 
                            EDIV     = false, 
                            DOT      = true, 
                            FDOT     = true)
  val tagBits = 8
  val addrBits = 40
//  chisel3.Driver.execute(args, () => new VPU8(minConfig, tagBits, addrBits))
  chisel3.Driver.execute(args, () => new VPU_64V64E1L(minConfig, tagBits, addrBits))
}

object VPUMain extends App {
  val SeqmulDiv = Some(Array(
    VMulDivParams(mulUnroll = 2,   divUnroll = 2,   mulEarlyOut = true, divEarlyOut = true),
    VMulDivParams(mulUnroll = 4,   divUnroll = 4,   mulEarlyOut = true, divEarlyOut = true),
    VMulDivParams(mulUnroll = 8,   divUnroll = 8,   mulEarlyOut = true, divEarlyOut = true),
    VMulDivParams(mulUnroll = 16,  divUnroll = 16,  mulEarlyOut = true, divEarlyOut = true),
    VMulDivParams(mulUnroll = 32,  divUnroll = 32,  mulEarlyOut = true, divEarlyOut = true),
    VMulDivParams(mulUnroll = 64,  divUnroll = 64,  mulEarlyOut = true, divEarlyOut = true),
    VMulDivParams(mulUnroll = 128, divUnroll = 128, mulEarlyOut = true, divEarlyOut = true),
    VMulDivParams(mulUnroll = 256, divUnroll = 256, mulEarlyOut = true, divEarlyOut = true)
  ))
  val CustomConfig = VPUParams(VLEN     = 64, 
                               ELEN     = 64, 
                               XLEN     = 64, 
                               FLEN     = 64, 
                               ZFINX    = false, 
                               FSEW16   = false, 
                               FSEWMAX  = 64, 
                               LMULMAX  = 1, 
                               MERGE    = true, 
                               MULDIV   = true, 
                               MULADD   = true, 
                               QMULADD  = true, 
                               mulDiv   = SeqmulDiv, 
                               RED      = true, 
                               MV       = true, 
                               SATADD   = true, 
                               AVERADD  = true, 
                               SATMUL   = true, 
                               SCALESR  = true, 
                               NCLIP    = true, 
                               SLIDE    = true, 
                               GATHER   = true, 
                               COMPRESS = true, 
                               COPY     = true, 
                               FMA      = true, 
                               FCVT     = true, 
                               FCMP     = true, 
                               FSGNJ    = true, 
                               FCLASS   = true, 
                               FMERGE   = true, 
                               FMV      = true, 
                               FDIVSQRT = true, 
                               FRED     = true, 
                               SEGLS    = true, 
                               AMO      = true, 
                               EDIV     = false, 
                               DOT      = true, 
                               FDOT     = true)
  val tagBits = 8
  val addrBits = 40
  iotesters.Driver.execute(args, () => new VPU_64V64E1L(CustomConfig, tagBits, addrBits)) {
    c => new VPUTest(c)
  }
}
/*
object VPURepl extends App {
  iotesters.Driver.executeFirrtlRepl(args, () => new VPU)
}*/
