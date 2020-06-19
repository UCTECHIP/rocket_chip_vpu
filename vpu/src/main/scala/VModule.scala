// See LICENSE.UCTECHIP for license details.
// See LICENSE.SZU for license details.
/************************************************************
*
*       Filename        :       VModule.scala
*       Author          :       liangzh
*       Revision        :       2019/04/26
*       Company         :       UC TECH IP
*       Department      :       MAG
*       Description     :       vector superclass, which has common nesting classes
*
************************************************************/
package vpu

import chisel3._
import chisel3.util._

abstract class VModule(params: VPUParams) extends Module with VConsts {
  val VLEN          = params.VLEN
  val ELEN          = params.ELEN
  val SELEN         = params.SELEN
  val XLEN          = params.XLEN
  val FLEN          = params.FLEN
  val ZFINX         = params.ZFINX
  val FSEW16        = params.FSEW16
  val FSEWMAX       = params.FSEWMAX
  val F16           = FSEW16
  val F32           = FSEWMAX >= 32
  val F64           = FSEWMAX >= 64
  val F128          = FSEWMAX == 128
  val LMULMAX       = params.LMULMAX
  val MERGE         = params.MERGE
  val MULDIV        = params.MULDIV
  val MULADD        = params.MULADD
  val QMULADD       = params.QMULADD
  val vmulDivParams = params.mulDiv.getOrElse(Array.fill(8)(VMulDivParams()))
  val RED           = params.RED
  val MV            = params.MV
  val SATADD        = params.SATADD
  val AVERADD       = params.AVERADD
  val SATMUL        = params.SATMUL
  val SCALESR       = params.SCALESR
  val NCLIP         = params.NCLIP
  val SLIDE         = params.SLIDE
  val GATHER        = params.GATHER
  val COMPRESS      = params.COMPRESS
  val COPY          = params.COPY
  val FMA           = params.FMA
  val FCVT          = params.FCVT
  val FCMP          = params.FCMP
  val FSGNJ         = params.FSGNJ
  val FCLASS        = params.FCLASS
  val FMERGE        = params.FMERGE
  val FMV           = params.FMV
  val FDIVSQRT      = params.FDIVSQRT
  val FRED          = params.FRED
  val SEGLS         = params.SEGLS
  val AMO           = params.AMO
  val EDIV          = params.EDIV
  val DOT           = params.DOT
  val FDOT          = params.FDOT

//for SEW
  val e8Depth    =                  ELEN * LMULMAX / 8
  val e16Depth   =                  ELEN * LMULMAX / 16
  val e32Depth   =                  ELEN * LMULMAX / 32
  val e64Depth   = if(ELEN >= 64)   ELEN * LMULMAX / 64   else 0
  val e128Depth  = if(ELEN >= 128)  ELEN * LMULMAX / 128  else 0
  val e256Depth  = if(ELEN >= 256)  ELEN * LMULMAX / 256  else 0
  val e512Depth  = if(ELEN >= 512)  ELEN * LMULMAX / 512  else 0
  val e1024Depth = if(ELEN == 1024) ELEN * LMULMAX / 1024 else 0

  val E8Depth    =                  VLEN 
  val E16Depth   =                  VLEN / 2
  val E32Depth   =                  VLEN / 4
  val E64Depth   = if(ELEN >= 64)   VLEN / 8   else 0
  val E128Depth  = if(ELEN >= 128)  VLEN / 16  else 0
  val E256Depth  = if(ELEN >= 256)  VLEN / 32  else 0
  val E512Depth  = if(ELEN >= 512)  VLEN / 64  else 0
  val E1024Depth = if(ELEN == 1024) VLEN / 128 else 0

//for FSEW
  val f16Depth   = if(FSEW16)         ELEN * LMULMAX / 16  else 0
  val f32Depth   =                    ELEN * LMULMAX / 32
  val f64Depth   = if(FSEWMAX >= 64)  ELEN * LMULMAX / 64  else 0
  val f128Depth  = if(FSEWMAX == 128) ELEN * LMULMAX / 128 else 0

  val F16Depth   = if(FSEW16)         VLEN / 2  else 0
  val F32Depth   =                    VLEN / 4
  val F64Depth   = if(FSEWMAX >= 64)  VLEN / 8  else 0
  val F128Depth  = if(FSEWMAX == 128) VLEN / 16 else 0

//for MLEN
  val m1Depth    =                  VLEN / 1
  val m2Depth    =                  VLEN / 2
  val m4Depth    =                  VLEN / 4
  val m8Depth    =                  VLEN / 8
  val m16Depth   =                  VLEN / 16
  val m32Depth   =                  VLEN / 32
  val m64Depth   = if(ELEN >= 64)   VLEN / 64   else 0
  val m128Depth  = if(ELEN >= 128)  VLEN / 128  else 0
  val m256Depth  = if(ELEN >= 256)  VLEN / 256  else 0
  val m512Depth  = if(ELEN >= 512)  VLEN / 512  else 0
  val m1024Depth = if(ELEN == 1024) VLEN / 1024 else 0

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
  class FullSEWDoubVec extends Bundle {
    val e16     = Vec(E8Depth,    UInt(16.W))
    val e32     = Vec(E16Depth,   UInt(32.W))
    val e64     = Vec(E32Depth,   UInt(64.W))
    val e128    = Vec(E64Depth,   UInt(128.W))
    val e256    = Vec(E128Depth,  UInt(256.W))
    val e512    = Vec(E256Depth,  UInt(512.W))
    val e1024   = Vec(E512Depth,  UInt(1024.W))
    val e2048   = Vec(E1024Depth, UInt(2048.W))
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
  def TakeSEW1wVec(in: FullSEW1wVec, dropQuarter: Int) = {
    val out = Wire(new SEW1wVec)
    out.e8    := in.e8.drop(dropQuarter*e8Depth/4).take(e8Depth)
    out.e16   := in.e16.drop(dropQuarter*e16Depth/4).take(e16Depth)
    out.e32   := in.e32.drop(dropQuarter*e32Depth/4).take(e32Depth)
    out.e64   := in.e64.drop(dropQuarter*e64Depth/4).take(e64Depth)
    out.e128  := in.e128.drop(dropQuarter*e128Depth/4).take(e128Depth)
    out.e256  := in.e256.drop(dropQuarter*e256Depth/4).take(e256Depth)
    out.e512  := in.e512.drop(dropQuarter*e512Depth/4).take(e512Depth)
    out.e1024 := in.e1024.drop(dropQuarter*e1024Depth/4).take(e1024Depth)
    out
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

//mXDepth related Vector Bundle
  class MLENVec extends Bundle {
    val m1      = Vec(m1Depth,    UInt(1.W))
    val m2      = Vec(m2Depth,    UInt(1.W))
    val m4      = Vec(m4Depth,    UInt(1.W))
    val m8      = Vec(m8Depth,    UInt(1.W))
    val m16     = Vec(m16Depth,   UInt(1.W))
    val m32     = Vec(m32Depth,   UInt(1.W))
    val m64     = Vec(m64Depth,   UInt(1.W))
    val m128    = Vec(m128Depth,  UInt(1.W))
    val m256    = Vec(m256Depth,  UInt(1.W))
    val m512    = Vec(m512Depth,  UInt(1.W))
    val m1024   = Vec(m1024Depth, UInt(1.W))
  }

  class MLENWideVec extends Bundle {
    val m1      = Vec(m1Depth,    UInt(1.W))
    val m2      = Vec(m2Depth,    UInt(2.W))
    val m4      = Vec(m4Depth,    UInt(4.W))
    val m8      = Vec(m8Depth,    UInt(8.W))
    val m16     = Vec(m16Depth,   UInt(16.W))
    val m32     = Vec(m32Depth,   UInt(32.W))
    val m64     = Vec(m64Depth,   UInt(64.W))
    val m128    = Vec(m128Depth,  UInt(128.W))
    val m256    = Vec(m256Depth,  UInt(256.W))
    val m512    = Vec(m512Depth,  UInt(512.W))
    val m1024   = Vec(m1024Depth, UInt(1024.W))
  }

  class FMLENVec extends Bundle {
    val m2      = Vec(m2Depth,    UInt(1.W))
    val m4      = Vec(m4Depth,    UInt(1.W))
    val m8      = Vec(m8Depth,    UInt(1.W))
    val m16     = Vec(m16Depth,   UInt(1.W))
    val m32     = Vec(m32Depth,   UInt(1.W))
    val m64     = Vec(m64Depth,   UInt(1.W))
    val m128    = Vec(m128Depth,  UInt(1.W))
  }

  class FMLENWideVec extends Bundle {
    val m2      = Vec(m2Depth,    UInt(2.W))
    val m4      = Vec(m4Depth,    UInt(4.W))
    val m8      = Vec(m8Depth,    UInt(8.W))
    val m16     = Vec(m16Depth,   UInt(16.W))
    val m32     = Vec(m32Depth,   UInt(32.W))
    val m64     = Vec(m64Depth,   UInt(64.W))
    val m128    = Vec(m128Depth,  UInt(128.W))
  }
//mXDepth related Vector Bundle

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
  def TakeFSEW1wVec(in: FullFSEW1wVec, dropDouble: Int) = {
    val out = Wire(new FSEW1wVec)
    out.f16   := in.f16.drop(dropDouble*f16Depth/2).take(f16Depth)
    out.f32   := in.f32.drop(dropDouble*f32Depth/2).take(f32Depth)
    out.f64   := in.f64.drop(dropDouble*f64Depth/2).take(f64Depth)
    out.f128  := in.f128.drop(dropDouble*f128Depth/2).take(f128Depth)
    out
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
    val exc16   = Vec(f16Depth,   UInt(VPUConstants.FFLAGS_SZ.W))
    val exc32   = Vec(f32Depth,   UInt(VPUConstants.FFLAGS_SZ.W))
    val exc64   = Vec(f64Depth,   UInt(VPUConstants.FFLAGS_SZ.W))
    val exc128  = Vec(f128Depth,  UInt(VPUConstants.FFLAGS_SZ.W))
  }
  class FullFFLAGSVec extends Bundle {
    val exc16   = Vec(F16Depth,   UInt(VPUConstants.FFLAGS_SZ.W))
    val exc32   = Vec(F32Depth,   UInt(VPUConstants.FFLAGS_SZ.W))
    val exc64   = Vec(F64Depth,   UInt(VPUConstants.FFLAGS_SZ.W))
    val exc128  = Vec(F128Depth,  UInt(VPUConstants.FFLAGS_SZ.W))
  }
//fXDepth related Vector Bundle

//sub-element Vector Bundle
  class EDIVVec(sew:Int) extends Bundle {
    val d2 = Vec(2, UInt((sew/2).W))
    val d4 = Vec(4, UInt((sew/4).W))
    val d8 = Vec(8, UInt((sew/8).W))
    override def cloneType = new EDIVVec(sew).asInstanceOf[this.type]
  }

  class SEWEDIVVec extends Bundle{
    val e8      = Vec(e8Depth,    new EDIVVec(8))   
    val e16     = Vec(e16Depth,   new EDIVVec(16))  
    val e32     = Vec(e32Depth,   new EDIVVec(32))
    val e64     = Vec(e64Depth,   new EDIVVec(64))
    val e128    = Vec(e128Depth,  new EDIVVec(128))
    val e256    = Vec(e256Depth,  new EDIVVec(256))
    val e512    = Vec(e512Depth,  new EDIVVec(512))
    val e1024   = Vec(e1024Depth, new EDIVVec(1024))
  }
}
