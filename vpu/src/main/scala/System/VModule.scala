// See LICENSE.UCTECHIP for license details.
// See LICENSE.SZU for license details.
/************************************************************
*
*       Filename        :       VModule.scala
*       Author          :       liangzh
*       Revision        :       2020/10/28
*       Company         :       UC TECH IP
*       Description     :       vector superclass, which has common nesting classes
*
************************************************************/
package vpu

import chisel3._
import chisel3.util._

abstract class VModule(params: VPUParams) extends Module
  with VConsts
  with AsyncResetTrait {
  val VLEN          = params.VLEN
  val ELEN          = params.ELEN
  val XLEN          = params.XLEN
  val FLEN          = params.FLEN
  val ZFINX         = params.ZFINX
  val FSEW16        = params.FSEW16
  val FSEWMAX       = params.FSEWMAX
  val RMUL          = params.RMUL
  val MERGE         = params.MERGE
  val MULDIV        = params.MULDIV
  val vmulDivParams = params.mulDiv.getOrElse(Array.fill(8)(VMulDivParams()))
  val MULADD        = params.MULADD
  val SATADD        = params.SATADD
  val AVERADD       = params.AVERADD
  val SATMUL        = params.SATMUL
  val SCALESR       = params.SCALESR
  val NCLIP         = params.NCLIP
  val FMA           = params.FMA
  val FCVT          = params.FCVT
  val FCMP          = params.FCMP
  val FMINMAX       = params.FMINMAX
  val FSGNJ         = params.FSGNJ
  val FCLASS        = params.FCLASS
  val FMERGE        = params.FMERGE
  val FDIVSQRT      = params.FDIVSQRT
  val FRSQRT        = params.FRSQRT
  val FRECE         = params.FRECE
  val RED           = params.RED
  val FRED          = params.FRED
  val POPC          = params.POPC
  val FIRST         = params.FIRST
  val MINDEX        = params.MINDEX
  val IOTA          = params.IOTA
  val INDEX         = params.INDEX
  val MBITWISE      = params.MBITWISE
  val MV            = params.MV
  val FMV           = params.FMV
  val SLIDE         = params.SLIDE
  val GATHER        = params.GATHER
  val COMPRESS      = params.COMPRESS
  val COPY          = params.COPY

  require(isPow2(VLEN) && VLEN >= 32)
  require(isPow2(ELEN) && ELEN >= 32 && ELEN <= 1024)
  require(isPow2(XLEN) && XLEN >= 32 && XLEN <= 64)
  require(isPow2(FLEN) && FLEN >= 32 && FLEN <= 64)
  require(isPow2(FSEWMAX) && FSEWMAX >= 32 && FSEWMAX <= 128)
  require(ELEN <= 8*VLEN)
  require(FSEWMAX <= ELEN)
  require((ELEN <= VLEN) && (RMUL*ELEN/VLEN <= 8) || (ELEN > VLEN) && (RMUL <= 8))

  val RLEN = if(ELEN <= VLEN) ELEN else VLEN

  val e8Depth    =                                  RMUL * RLEN / 8
  val e16Depth   =                                  RMUL * RLEN / 16
  val e32Depth   =                                  RMUL * RLEN / 32
  val e64Depth   = if(ELEN >= 64   && VLEN >= 64)   RMUL * RLEN / 64   else if(ELEN >= 64   && VLEN < 64)   1 else 0
  val e128Depth  = if(ELEN >= 128  && VLEN >= 128)  RMUL * RLEN / 128  else if(ELEN >= 128  && VLEN < 128)  1 else 0
  val e256Depth  = if(ELEN >= 256  && VLEN >= 256)  RMUL * RLEN / 256  else if(ELEN >= 256  && VLEN < 256)  1 else 0
  val e512Depth  = if(ELEN >= 512  && VLEN >= 512)  RMUL * RLEN / 512  else if(ELEN >= 512  && VLEN < 512)  1 else 0
  val e1024Depth = if(ELEN == 1024 && VLEN >= 1024) RMUL * RLEN / 1024 else if(ELEN >= 1024 && VLEN < 1024) 1 else 0

  val E8Depth    =                  VLEN
  val E16Depth   =                  VLEN / 2
  val E32Depth   =                  VLEN / 4
  val E64Depth   = if(ELEN >= 64)   VLEN / 8   else 0
  val E128Depth  = if(ELEN >= 128)  VLEN / 16  else 0
  val E256Depth  = if(ELEN >= 256)  VLEN / 32  else 0
  val E512Depth  = if(ELEN >= 512)  VLEN / 64  else 0
  val E1024Depth = if(ELEN == 1024) VLEN / 128 else 0

//for FSEW
  val f16Depth   = if(FSEW16)                        RMUL * RLEN / 16  else 0
  val f32Depth   =                                   RMUL * RLEN / 32
  val f64Depth   = if(FSEWMAX >= 64  && VLEN >= 64)  RMUL * RLEN / 64  else if(FSEWMAX >= 64  && VLEN < 64)  1 else 0
  val f128Depth  = if(FSEWMAX == 128 && VLEN >= 128) RMUL * RLEN / 128 else if(FSEWMAX >= 128 && VLEN < 128) 1 else 0

  val F16Depth   = if(FSEW16)         VLEN / 2  else 0
  val F32Depth   =                    VLEN / 4
  val F64Depth   = if(FSEWMAX >= 64)  VLEN / 8  else 0
  val F128Depth  = if(FSEWMAX == 128) VLEN / 16 else 0

//eXDepth related Vector Bundle
  class SEWVec extends Bundle {
    val e8      = Vec(e8Depth,    UInt(8.W))
    val e16     = Vec(e16Depth,   UInt(16.W))
    val e32     = Vec(e32Depth,   UInt(32.W))
    val e64     = Vec(e64Depth,   UInt(64.W))
    val e128    = Vec(e128Depth,  UInt(128.W))
    val e256    = Vec(e256Depth,  UInt(256.W))
    val e512    = Vec(e512Depth,  UInt(512.W))
    val e1024   = Vec(e1024Depth, UInt(1024.W))
  }
  class FullSEWVec extends Bundle {
    val e8      = Vec(E8Depth,    UInt(8.W))
    val e16     = Vec(E16Depth,   UInt(16.W))
    val e32     = Vec(E32Depth,   UInt(32.W))
    val e64     = Vec(E64Depth,   UInt(64.W))
    val e128    = Vec(E128Depth,  UInt(128.W))
    val e256    = Vec(E256Depth,  UInt(256.W))
    val e512    = Vec(E512Depth,  UInt(512.W))
    val e1024   = Vec(E1024Depth, UInt(1024.W))
  }
  //----------------------------------------------------------------//
  class SEWp1Vec extends Bundle {
    val e9      = Vec(e8Depth,    UInt(9.W))
    val e17     = Vec(e16Depth,   UInt(17.W))
    val e33     = Vec(e32Depth,   UInt(33.W))
    val e65     = Vec(e64Depth,   UInt(65.W))
    val e129    = Vec(e128Depth,  UInt(129.W))
    val e257    = Vec(e256Depth,  UInt(257.W))
    val e513    = Vec(e512Depth,  UInt(513.W))
    val e1025   = Vec(e1024Depth, UInt(1025.W))
  }
  //----------------------------------------------------------------//
  class SEWDoubVec extends Bundle {
    val e16     = Vec(e8Depth,    UInt(16.W))
    val e32     = Vec(e16Depth,   UInt(32.W))
    val e64     = Vec(e32Depth,   UInt(64.W))
    val e128    = Vec(e64Depth,   UInt(128.W))
    val e256    = Vec(e128Depth,  UInt(256.W))
    val e512    = Vec(e256Depth,  UInt(512.W))
    val e1024   = Vec(e512Depth,  UInt(1024.W))
    val e2048   = Vec(e1024Depth, UInt(2048.W))
  }
  //----------------------------------------------------------------//
  class SEW1wVec extends Bundle {
    val e8      = Vec(e8Depth,    UInt(1.W))
    val e16     = Vec(e16Depth,   UInt(1.W))
    val e32     = Vec(e32Depth,   UInt(1.W))
    val e64     = Vec(e64Depth,   UInt(1.W))
    val e128    = Vec(e128Depth,  UInt(1.W))
    val e256    = Vec(e256Depth,  UInt(1.W))
    val e512    = Vec(e512Depth,  UInt(1.W))
    val e1024   = Vec(e1024Depth, UInt(1.W))
  }
  class FullSEW1wVec extends Bundle {
    val e8      = Vec(E8Depth,    UInt(1.W))
    val e16     = Vec(E16Depth,   UInt(1.W))
    val e32     = Vec(E32Depth,   UInt(1.W))
    val e64     = Vec(E64Depth,   UInt(1.W))
    val e128    = Vec(E128Depth,  UInt(1.W))
    val e256    = Vec(E256Depth,  UInt(1.W))
    val e512    = Vec(E512Depth,  UInt(1.W))
    val e1024   = Vec(E1024Depth, UInt(1.W))
  }
  //----------------------------------------------------------------//
  class SEWXLENVec extends Bundle {
    val e8      = Vec(E8Depth,    UInt(XLEN.W))
    val e16     = Vec(E16Depth,   UInt(XLEN.W))
    val e32     = Vec(E32Depth,   UInt(XLEN.W))
    val e64     = Vec(E64Depth,   UInt(XLEN.W))
    val e128    = Vec(E128Depth,  UInt(XLEN.W))
    val e256    = Vec(E256Depth,  UInt(XLEN.W))
    val e512    = Vec(E512Depth,  UInt(XLEN.W))
    val e1024   = Vec(E1024Depth, UInt(XLEN.W))
  }
  //----------------------------------------------------------------//
  class Log2SEWVec extends Bundle {
    val to3     = Vec(e8Depth,    UInt(3.W))
    val to4     = Vec(e16Depth,   UInt(4.W))
    val to5     = Vec(e32Depth,   UInt(5.W))
    val to6     = Vec(e64Depth,   UInt(6.W))
    val to7     = Vec(e128Depth,  UInt(7.W))
    val to8     = Vec(e256Depth,  UInt(8.W))
    val to9     = Vec(e512Depth,  UInt(9.W))
    val to10    = Vec(e1024Depth, UInt(10.W))
  }
  class FullLog2SEWVec extends Bundle {
    val to3     = Vec(E8Depth,    UInt(3.W))
    val to4     = Vec(E16Depth,   UInt(4.W))
    val to5     = Vec(E32Depth,   UInt(5.W))
    val to6     = Vec(E64Depth,   UInt(6.W))
    val to7     = Vec(E128Depth,  UInt(7.W))
    val to8     = Vec(E256Depth,  UInt(8.W))
    val to9     = Vec(E512Depth,  UInt(9.W))
    val to10    = Vec(E1024Depth, UInt(10.W))
  }
//eXDepth related Vector Bundle

//fXDepth related Vector Bundle
  class FSEWVec extends Bundle {
    val f16     = Vec(f16Depth,   UInt(16.W))
    val f32     = Vec(f32Depth,   UInt(32.W))
    val f64     = Vec(f64Depth,   UInt(64.W))
    val f128    = Vec(f128Depth,  UInt(128.W))
  }
  class FullFSEWVec extends Bundle {
    val f16     = Vec(F16Depth,   UInt(16.W))
    val f32     = Vec(F32Depth,   UInt(32.W))
    val f64     = Vec(F64Depth,   UInt(64.W))
    val f128    = Vec(F128Depth,  UInt(128.W))
  }
  //----------------------------------------------------------------//
  class FSEW1wVec extends Bundle {
    val f16     = Vec(f16Depth,   UInt(1.W))
    val f32     = Vec(f32Depth,   UInt(1.W))
    val f64     = Vec(f64Depth,   UInt(1.W))
    val f128    = Vec(f128Depth,  UInt(1.W))
  }
  class FullFSEW1wVec extends Bundle {
    val f16     = Vec(F16Depth,   UInt(1.W))
    val f32     = Vec(F32Depth,   UInt(1.W))
    val f64     = Vec(F64Depth,   UInt(1.W))
    val f128    = Vec(F128Depth,  UInt(1.W))
  }
  //----------------------------------------------------------------//
  class FToXSEWVec extends Bundle {
    val e8      = Vec(e8Depth,    UInt(8.W))
    val e16     = Vec(e16Depth,   UInt(16.W))
    val e32     = Vec(e32Depth,   UInt(32.W))
    val e64     = Vec(e64Depth,   UInt(64.W))
    val e128    = Vec(e128Depth,  UInt(128.W))
    val e256    = Vec(e256Depth,  UInt(256.W))
  }
  //----------------------------------------------------------------//
  class FUCBVec extends Bundle {
    val f16     = Vec(f16Depth,   UInt(17.W))
    val f32     = Vec(f32Depth,   UInt(33.W))
    val f64     = Vec(f64Depth,   UInt(65.W))
    val f128    = Vec(f128Depth,  UInt(129.W))
  }
  //----------------------------------------------------------------//
  class FFLAGSVec extends Bundle {
    val exc16   = Vec(f16Depth,   UInt(VPUIOConstants.FFLAGS_SZ.W))
    val exc32   = Vec(f32Depth,   UInt(VPUIOConstants.FFLAGS_SZ.W))
    val exc64   = Vec(f64Depth,   UInt(VPUIOConstants.FFLAGS_SZ.W))
    val exc128  = Vec(f128Depth,  UInt(VPUIOConstants.FFLAGS_SZ.W))
  }
  class FullFFLAGSVec extends Bundle {
    val exc16   = Vec(F16Depth,   UInt(VPUIOConstants.FFLAGS_SZ.W))
    val exc32   = Vec(F32Depth,   UInt(VPUIOConstants.FFLAGS_SZ.W))
    val exc64   = Vec(F64Depth,   UInt(VPUIOConstants.FFLAGS_SZ.W))
    val exc128  = Vec(F128Depth,  UInt(VPUIOConstants.FFLAGS_SZ.W))
  }
//fXDepth related Vector Bundle

}
