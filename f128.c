#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <inttypes.h>

#define I16_HI_BIT 0x8000ull
#define I16_MASK 0xffffull
#define I32_MASK 0xffffffffull
#define I64_HI_BIT 0x8000000000000000ull
#define I64_MAX 0xffffffffffffffffull

#define F128_HI_SIGN_FLAG 0x8000000000000000ull
#define F128_HI_EXP_SHIFT 48
#define F128_EXP_MASK  0x7fffull
#define F128_EXP_BIAS  0x3fffull
#define F128_HI_HIDDEN_BIT   0x1000000000000ull
#define F128_HI_MANTISSA_MASK 0xffffffffffffull

#define F128_NAN_HI  0x7fffffffffffffffull
#define F128_NAN_LOW 0xffffffffffffffffull
#define F128_INF_HI  0x7fff000000000000ull
#define F128_INF_LOW 0

typedef union{
  double f64;
  uint64_t i64;
}f64_bits;


typedef struct{
  uint64_t hi;
  uint64_t low;
}f128;
// [sign:1][exp:15][mantissa:112]

// declarations of functions used before initialization
f128 f128_fromF64(double);
double f128_toF64(f128);

bool f128_isNaN(f128);
int f128_compare(f128,f128);
f128 f128_add(f128,f128);
f128 f128_sub(f128,f128);
f128 f128_mult(f128,f128);
f128 f128_inv(f128);
f128 f128_div(f128,f128);

static int f128_normalizeMantissa(f128*);

#define F64_SIGN_FLAG  0x8000000000000000ull
#define F64_EXP_SHIFT 52
#define F64_EXP_MASK   0x7ff
#define F64_EXP_BIAS   0x3ff
#define F64_HIDDEN_BIT   0x10000000000000ull
#define F64_MANTISSA_MASK 0xfffffffffffffull

f128 f128_fromF64(double x){
  uint64_t bits=(f64_bits){.f64=x}.i64;
  uint64_t sign=bits&F64_SIGN_FLAG;
  uint64_t exp=(bits>>F64_EXP_SHIFT)&F64_EXP_MASK;
  uint64_t mantissa=bits&F64_MANTISSA_MASK;
  uint64_t hi= mantissa >> ( F64_EXP_SHIFT - F128_HI_EXP_SHIFT );
  uint64_t low= mantissa << ( 64 + F128_HI_EXP_SHIFT - F64_EXP_SHIFT );
  f128 r=(f128){.hi=hi,.low=low};
  if(exp==0){// subnormal numbers
    // +1 to compensate exponent offset
    exp=F128_EXP_BIAS-F64_EXP_BIAS-f128_normalizeMantissa(&r)+1;
    r.hi&=F128_HI_MANTISSA_MASK;
  }else{
    exp+=F128_EXP_BIAS-F64_EXP_BIAS;//update bias
    // NaN / Infinity get mapped to NaN / Infinity
  }
  r.hi|=sign|(exp<<F128_HI_EXP_SHIFT);
  return r;
}
double f128_toF64(f128 x){
  uint64_t sign=x.hi&F128_HI_SIGN_FLAG;
  uint64_t exp=(x.hi>>F128_HI_EXP_SHIFT)&F128_EXP_MASK;
  uint64_t mantissaHi=x.hi&F128_HI_MANTISSA_MASK;
  uint64_t mantissaLow=x.low;
  uint64_t mantissa= mantissaHi << ( F64_EXP_SHIFT - F128_HI_EXP_SHIFT );
  mantissa |= mantissaLow >> ( 64 + F128_HI_EXP_SHIFT - F64_EXP_SHIFT );
  // TODO round instead of truncate
  if(exp==F128_EXP_MASK){ // inf/NaN
    exp=F64_EXP_MASK;
  }else if(exp>=F128_EXP_BIAS+F64_EXP_MASK){ // overflow (exp - bias > f64 mask)
    exp=F64_EXP_MASK;
    mantissa=0;
  }else if(exp+F64_EXP_BIAS<=F128_EXP_BIAS){ // underflow ( exp -bias + f64_bias <= 0 )
    uint64_t shift=F128_EXP_BIAS-(exp+F64_EXP_BIAS)+1;// +1 for hidden bit
    if(shift<=F64_EXP_SHIFT){
      mantissa=(mantissa|F64_HIDDEN_BIT)>>shift;
    }else{
      mantissa=0;
    }
    exp=0;
  }else{
    exp+=F64_EXP_BIAS;
    exp-=F128_EXP_BIAS;
  }
  return (f64_bits){.i64=sign|(exp<<F64_EXP_SHIFT)|mantissa}.f64;
}

bool f128_isNaN(f128 x){
  if(((x.hi>>F128_HI_EXP_SHIFT)&F128_EXP_MASK)!=F128_EXP_MASK)
    return false;
  return (x.hi&F128_HI_MANTISSA_MASK)|x.low;
}

int f128_compare(f128 x,f128 y){
  if((x.hi&F128_HI_SIGN_FLAG)!=(y.hi&F128_HI_SIGN_FLAG)){
    // different signs
    return (x.hi&F128_HI_SIGN_FLAG)?-1:1;
  }
  int sign=(x.hi&F128_HI_SIGN_FLAG)?-1:1;
  if(f128_isNaN(x)&&f128_isNaN(y))
    return 0; // treat all NaNs as equal
  uint64_t xHi=x.hi&(~F128_HI_SIGN_FLAG);
  uint64_t yHi=y.hi&(~F128_HI_SIGN_FLAG);
  if(xHi!=yHi){
    return xHi<yHi?-sign:sign;
  }
  return (x.low<y.low)?-sign:(x.low>y.low)?sign:0;
}
// XXX separate functions for operators to satisfy rules for Nan and zero

static int f128_normalizeMantissa(f128* x){
  if(((x->hi>>F128_HI_EXP_SHIFT)&F128_EXP_MASK)!=0)
    return 0;//already normalized
  x->hi&=F128_HI_MANTISSA_MASK;// set sign bit to zero
  int shift=0,baseShift=0;
  // shift until high part non-zero
  if(x->hi==0){
    if(x->low<=(F128_HI_HIDDEN_BIT|F128_HI_MANTISSA_MASK)){ // no high bits set
      x->hi=x->low;
      x->low=0;
      baseShift=64;
    }else{
      x->hi=x->low>>16; // shifting by 48 still less than maximum shift
      x->low<<=48;
      baseShift=48;
    }
  }
  // XXX? get rid of magic numbers
  // shift to 1 **** **** ****
  if((x->hi&0x1fffffffe0000)==0){ // 0 0000 000[000*]
    x->hi<<=32;shift+=32;
  }
  if((x->hi&0x1fffe00000000)==0){ // 0 000[000*]
    x->hi<<=16;shift+=16;
  }
  if((x->hi&0x1fe0000000000)==0){  // 0 0[000*]
    x->hi<<=8;shift+=8;
  }
  if((x->hi&0x1e00000000000)==0){  // 0 [000*]
    x->hi<<=4;shift+=4;
  }
  if((x->hi&0x1800000000000)==0){  // 0b0 0**
    x->hi<<=2;shift+=2;
  }
  if((x->hi&0x1000000000000)==0){  // 0b0 *
    x->hi<<=1;shift+=1;
  }
  if(shift!=0){
    x->hi|=x->low>>(64-shift);
  }
  x->low<<=shift;
  return baseShift+shift;
}

f128 f128_add(f128 x,f128 y){
  if((x.hi&F128_HI_SIGN_FLAG)!=(y.hi&F128_HI_SIGN_FLAG)){
    y.hi^=F128_HI_SIGN_FLAG;
    return f128_sub(x,y);
  }
  uint64_t sign=x.hi&F128_HI_SIGN_FLAG;
  int32_t expX=(x.hi>>F128_HI_EXP_SHIFT)&F128_EXP_MASK;
  int32_t expY=(y.hi>>F128_HI_EXP_SHIFT)&F128_EXP_MASK;
  if(expX==F128_EXP_MASK){
    if(f128_isNaN(x)||f128_isNaN(y)){
      // NaN+y, x+NaN -> NaN
      return (f128){.hi=sign|F128_NAN_HI,.low=F128_NAN_LOW};
    }
    // Infinity+y -> Infinity
    return (f128){.hi=sign|F128_INF_HI,.low=F128_INF_LOW};
  }else if(expY==F128_EXP_MASK){
    if(f128_isNaN(y)){
      // x+NaN -> NaN
      return (f128){.hi=sign|F128_NAN_HI,.low=F128_NAN_LOW};
    }
    // x+Infinity -> Infinity
    return (f128){.hi=sign|F128_INF_HI,.low=F128_INF_LOW};
  }
  // underflow
  if(expX>expY+113) // difference of exponents > 113 -> no change of larger number
    return x;
  if(expY>expX+113)
    return y;
  x.hi&=F128_HI_MANTISSA_MASK;
  y.hi&=F128_HI_MANTISSA_MASK;
  // insert hidden bit
  if(expX==0){
    expX=1;
  }else{
    x.hi |= F128_HI_HIDDEN_BIT;
  }
  if(expY==0){
    expY=1;
  }else{
    y.hi |= F128_HI_HIDDEN_BIT;
  }
  int nz=0,nextBit=0;
  uint64_t exp=expX;
  if(expX>expY){
    int32_t shift=expX-expY;
    if(shift>=64){
      nz=(y.low|(y.hi&((1<<(shift-64-1))-1)))!=0;
      nextBit=y.hi&(1<<(shift-64-1));
      y.low=y.hi>>(shift-64);
      y.hi=0;
    }else{
      nz=(y.low&((1<<(shift-1))-1))!=0;
      nextBit=y.low&(1<<(shift-1));
      y.low=(y.low>>shift)|(y.hi<<(64-shift));
      y.hi=y.hi>>shift;
    }
  }else if(expX!=expY){
    exp=expY;
    int32_t shift=expY-expX;
    if(shift>=64){
      nz=(x.low|(x.hi&((1<<(shift-64-1))-1)))!=0;
      nextBit=x.hi&(1<<(shift-64-1));
      x.low=x.hi>>(shift-64);
      x.hi=0;
    }else{
      nz=(x.low&((1<<(shift-1))-1))!=0;
      nextBit=x.low&(1<<(shift-1));
      x.low=(x.low>>shift)|(x.hi<<(64-shift));
      x.hi=x.hi>>shift;
    }
  }
  uint64_t lowSum=x.low+y.low;
  x.hi+=y.hi+(lowSum<x.low);
  x.low=lowSum;
  // overflow (at most by one bit)
  if(x.hi>(F128_HI_HIDDEN_BIT|F128_HI_MANTISSA_MASK)){
    nz|=nextBit;
    nextBit=x.low&1;
    x.low=(x.low>>1)|(x.hi<<63);
    x.hi>>=1;
    exp++;
  }
  // rounding
  if(nextBit&&(nz||(x.low&1)==1)){
    // round up if next bit 1 and remainder >0.5 or remainder ==0.5 and number odd
    x.low++;
    if(x.low==0){// overflow
      x.hi++;
    }
  }
  // rounding lead to overflow
  if(x.hi>(F128_HI_HIDDEN_BIT|F128_HI_MANTISSA_MASK)){
    // when rounding overflows, then x.low is zero
    x.low=x.hi<<63;
    x.hi>>=1;
    exp++;
  }
  if(exp>=F128_EXP_MASK){// number overflowed
    exp=F128_EXP_MASK;
    x.hi=x.low=0;
  }
  x.hi&=F128_HI_MANTISSA_MASK;
  x.hi|=sign|(exp<<F128_HI_EXP_SHIFT);
  return x;
}
// TODO check if subtraction works correctly for edge cases
f128 f128_sub(f128 x,f128 y){
  if((x.hi&F128_HI_SIGN_FLAG)!=(y.hi&F128_HI_SIGN_FLAG)){
    y.hi^=F128_HI_SIGN_FLAG;
    return f128_add(x,y);
  }
  uint64_t sign=x.hi&F128_HI_SIGN_FLAG;
  int32_t expX=(x.hi>>F128_HI_EXP_SHIFT)&F128_EXP_MASK;
  int32_t expY=(y.hi>>F128_HI_EXP_SHIFT)&F128_EXP_MASK;
  if(expX==F128_EXP_MASK){
    if(f128_isNaN(x)||expY==F128_EXP_MASK){
      // NaN-y, x-NaN, Infinity-Infinity -> NaN
      return (f128){.hi=F128_NAN_HI,.low=F128_NAN_LOW};
    }
    // Infinity-y -> Infinity
    return (f128){.hi=sign|F128_INF_HI,.low=F128_INF_LOW};
  }else if(expY==F128_EXP_MASK){
    if(f128_isNaN(y)){
      // x-NaN -> NaN
      return (f128){.hi=F128_NAN_HI,.low=F128_NAN_LOW};
    }
    // x-Infinity -> -Infinity
    return (f128){
      .hi=(sign^F128_HI_SIGN_FLAG)|F128_INF_HI,
      .low=F128_INF_LOW
    };
  }
  if(expX>expY+113) // difference of exponents > 113 -> no change of larger number
    return x;
  if(expY>expX+113)
    return y;
  x.hi&=F128_HI_MANTISSA_MASK;
  y.hi&=F128_HI_MANTISSA_MASK;
  // insert hidden bit
  if(expX==0){
    expX=1;
  }else{
    x.hi |= F128_HI_HIDDEN_BIT;
  }
  if(expY==0){
    expY=1;
  }else{
    y.hi |= F128_HI_HIDDEN_BIT;
  }
  int nz=0,nextBit=0;
  bool roundDown=false;
  uint64_t exp=expX;
  if(expX>expY){
    int32_t shift=expX-expY;
    if(shift>=64){
      nz=(y.low|(y.hi&((1<<(shift-64-1))-1)))!=0;
      nextBit=y.hi&(1<<(shift-64-1));
      y.low=y.hi>>(shift-64);
      y.hi=0;
    }else{
      nz=(y.low&((1<<(shift-1))-1))!=0;
      nextBit=y.low&(1<<(shift-1));
      y.low=(y.low>>shift)|(y.hi<<(64-shift));
      y.hi=y.hi>>shift;
    }
  }else if(expX!=expY){
    exp=expY;
    roundDown=true;
    int32_t shift=expY-expX;
    if(shift>=64){
      nz=(x.low|(x.hi&((1<<(shift-64-1))-1)))!=0;
      nextBit=x.hi&(1<<(shift-64-1));
      x.low=x.hi>>(shift-64);
      x.hi=0;
    }else{
      nz=(x.low&((1<<(shift-1))-1))!=0;
      nextBit=x.low&(1<<(shift-1));
      x.low=(x.low>>shift)|(x.hi<<(64-shift));
      x.hi=x.hi>>shift;
    }
  }
  if(x.hi<y.hi||(x.hi==y.hi&&x.low<y.low)){
    f128 t=x;
    x=y;
    y=t;
    sign^=F128_HI_SIGN_FLAG;
    roundDown=!roundDown;
  }
  // [x.hi:x.low] >= [y.hi:x.low]
  x.hi-=y.hi+(y.low>x.low);
  x.low-=y.low;
  // rounding
  if(nextBit&&(nz||(x.low&1)==1)){
    // round up/down if next bit 1 and remainder >0.5 or remainder ==0.5 and number odd
    if(roundDown){
      x.low--;
      if(x.low==I64_MAX){// underflow
        x.hi--;
      }
    }else{
      x.low++;
      if(x.low==0){// overflow
        x.hi++;
      }
    }
  }
  if((x.hi|x.low)==0){
    exp=0;
  }else{
    exp-=f128_normalizeMantissa(&x);
  }
  if(exp<=0){ // TODO fix handling of subnormal numbers
    exp=0;
    // undo last shift in normalization
    x.low=(x.low>>1)|(x.hi<<63);
    x.hi>>=1;
  }
  x.hi&=F128_HI_MANTISSA_MASK;
  x.hi|=sign|(exp<<F128_HI_EXP_SHIFT);
  return x;
}


f128 f128_mult(f128 x,f128 y){
  uint64_t sign=(x.hi&F128_HI_SIGN_FLAG)^(y.hi&F128_HI_SIGN_FLAG);
  int32_t expX=(x.hi>>F128_HI_EXP_SHIFT)&F128_EXP_MASK;
  int32_t expY=(y.hi>>F128_HI_EXP_SHIFT)&F128_EXP_MASK;
  if(expX==F128_EXP_MASK){//x infinity and NaN
    if(f128_isNaN(x)||f128_isNaN(y)||(((y.hi&~F128_HI_SIGN_FLAG)|y.low)==0)){
      // NaN*y, x*NaN, Infinity*0 -> NaN
      return (f128){.hi=sign|F128_NAN_HI,.low=F128_NAN_LOW};
    }
    // Infinity*y -> Infinity
    return (f128){.hi=sign|F128_INF_HI,.low=F128_INF_LOW};
  }else if(expY==F128_EXP_MASK){//y infinity or NaN
    if(f128_isNaN(y)||(((x.hi&~F128_HI_SIGN_FLAG)|x.low)==0)){ // x is Finite by first if
      // x*NaN, +*Infinity -> NaN
      return (f128){.hi=sign|F128_NAN_HI,.low=F128_NAN_LOW};
    }
    // x*Infinity -> Infinity
    return (f128){.hi=sign|F128_INF_HI,.low=F128_INF_LOW};
  }
  x.hi&=F128_HI_MANTISSA_MASK;
  y.hi&=F128_HI_MANTISSA_MASK;
  if(expX==0){
    if((x.hi|x.low)==0){// 0*y -> 0
      return (f128){.hi=sign,.low=0};
    }
    expX=1;// exponent 0 uses same power as exponent 1
    expX-=f128_normalizeMantissa(&x);
  }
  if(expY==0){
    if((y.hi|y.low)==0){// x*0 -> 0
      return (f128){.hi=sign,.low=0};
    }
    expY=1;// exponent 0 uses same power as exponent 1
    expY-=f128_normalizeMantissa(&y);
  }
  // multiply 113 bit mantissas
  uint64_t a = (x.hi|F128_HI_HIDDEN_BIT) >> 32;
  uint64_t b = (x.hi) & I32_MASK;
  uint64_t c = (x.low >> 32) & I32_MASK;
  uint64_t d = (x.low) & I32_MASK;
  uint64_t e = (y.hi|F128_HI_HIDDEN_BIT) >> 32;
  uint64_t f = (y.hi) & I32_MASK;;
  uint64_t g = (y.low >> 32) & I32_MASK;
  uint64_t h = (y.low) & I32_MASK;
  uint64_t low=0,mid=0,hi=0,tmp;
  int nz=0, leading;// low bits are not zero
  // have to calculate all digits to get correct rounding
  // offset 0: dh
  tmp=d*h;
  low+=tmp & I32_MASK;
  mid+=tmp>>32;
  // shift result by 32
  nz+=(low & I32_MASK)!=0;
  mid+=low>>32;
  low=mid;mid=0;
  // offset 32: ch dg
  tmp=c*h;
  low+=tmp & I32_MASK;
  mid+=tmp>>32;
  tmp=d*g;
  low+=tmp & I32_MASK;
  mid+=tmp>>32;
  // shift result by 32
  nz+=(low & I32_MASK)!=0;
  mid+=low>>32;
  low=mid;mid=0;
  // offset 64: bh cg df
  tmp=b*h;
  low+=tmp & I32_MASK;
  mid+=tmp>>32;
  tmp=c*g;
  low+=tmp & I32_MASK;
  mid+=tmp>>32;
  tmp=d*f;
  low+=tmp & I32_MASK;
  mid+=tmp>>32;
  // shift result by 32
  nz+=(low & I32_MASK)!=0;
  mid+=low>>32;
  low=mid;mid=0;
  // offset 96: ah bg cf de
  tmp=a*h;
  low+=tmp & I32_MASK;
  mid+=tmp>>32;
  tmp=b*g;
  low+=tmp & I32_MASK;
  mid+=tmp>>32;
  tmp=c*f;
  low+=tmp & I32_MASK;
  mid+=tmp>>32;
  tmp=d*e;
  low+=tmp & I32_MASK;
  mid+=tmp>>32;
  // shift result by 16
  nz+=(low & I16_MASK)!=0;
  leading=(low & I16_HI_BIT)!=0;//next bit after number
  low=(low>>16)+((mid & I16_MASK)<<16);
  mid>>=16;
  // offset 128: ag bf ce
  tmp=a*g;
  low+=(tmp & I16_MASK)<<16;
  mid+=tmp>>16;
  tmp=b*f;
  low+=(tmp & I16_MASK)<<16;
  mid+=tmp>>16;
  tmp=c*e;
  low+=(tmp & I16_MASK)<<16;
  mid+=tmp>>16;
  // offset 160: af be
  tmp=a*f;
  mid+=(tmp & I16_MASK)<<16;
  hi+=tmp>>16;
  tmp=b*e;
  mid+=(tmp & I16_MASK)<<16;
  hi+=tmp>>16;
  // offset 192: ae
  hi+=(a*e)<<16;// a,e<2^17 -> ae < 2^34
  // merge numbers to one 128-bit number in hi:low
  mid+=(low>>32) & I32_MASK;
  low=(low & I32_MASK)|((mid & I32_MASK)<<32);
  hi+=(mid>>32) & I32_MASK;
  // truncate number to 112 bits
  tmp = hi >> 48; // should be between 1 and 4 (inclusive)
  int shift = tmp==1 ? 0 : tmp < 4 ? 1 : 2 ;
  if( tmp >> shift != 1 ){
    fprintf(stderr,"unexpected value for hi bits %"PRIu64"\n",tmp);
  }
  if(shift!=0){
    leading=(low>>(shift-1))&1;
  }
  if(leading&&(nz||((low>>shift)&1)==1)){
    // round up if next bit 1 and remainder >0.5 or remainder ==0.5 and number odd
    low++;
    if(low==0){// overflow
      hi++;
      if(hi>(1ull<<(shift+1))){
        shift++;
      }
    }
  }
  low >>=shift;
  if(shift!=0){
    low |= hi<<(64-shift);
  }
  hi =(hi>>shift) & F128_HI_MANTISSA_MASK;
  // sum contains bias twice -> compensate for extra bias
  int32_t exp=expX+expY+shift-F128_EXP_BIAS;
  if(exp>=(int64_t)F128_EXP_MASK){ // overflow
    exp=F128_EXP_MASK;
    hi=low=0;
  }else if(exp<1){ // underflow
    hi|=F128_HI_HIDDEN_BIT;
    // 1 for hidden bit,+ 1 for every power of two below 0
    int32_t shift=1-exp;
    exp=0;
    // subnormal
    if(shift>112){
      hi=low=0;
    }else if(shift>=64){
      low=hi>>(shift-64);
      hi=0;
    }else{
      low=(low>>shift)|(hi<<(64-shift));
      hi>>=shift;
    }
  }
  // update exponent & sign
  return (f128){.hi=sign|(((uint64_t)exp)<<F128_HI_EXP_SHIFT)|hi,.low=low};
}

#define MIN_INVERTABLE_HI 0x400000000000

/*
Computes the inverse of x, assumes that 0.5<=x<1.
uses Newton-Raphson-Division (https://en.wikipedia.org/wiki/Division_algorithm#Newton%E2%80%93Raphson_division)
*/
static f128 f128_normalizedInv(f128 x){
  // TODO precompute constants in f128 precission
  f128 p=f128_fromF64(48/17.0);
  f128 q=f128_fromF64(32/17.0);
  f128 two=f128_fromF64(2);
  // estimate 1/x as 48/17 - 32/17 * x
  f128 y=f128_sub(p,f128_mult(x,q));
  // iteration step: y2 = y1(2 - x * y1)
  // 5 iteration steps should be enough
  y = f128_mult(y,f128_sub(two,f128_mult(x,y)));
  y = f128_mult(y,f128_sub(two,f128_mult(x,y)));
  y = f128_mult(y,f128_sub(two,f128_mult(x,y)));
  y = f128_mult(y,f128_sub(two,f128_mult(x,y)));
  y = f128_mult(y,f128_sub(two,f128_mult(x,y)));
  return y;
}

f128 f128_inv(f128 x){
  uint64_t sign=x.hi&F128_HI_SIGN_FLAG;
  int32_t expX=(x.hi>>F128_HI_EXP_SHIFT)&F128_EXP_MASK;
  if(expX==F128_EXP_MASK){
    if(f128_isNaN(x)){
      return x;
    }
    // 1/Infinity = 0
    return (f128){.hi=sign,.low=0};
  }else if(expX==0){
    if(x.hi<MIN_INVERTABLE_HI){// 1/0 = Infinity
      // 1/[MIN_INVERTABLE_HI, 0] = +Infinity
      //  and all smaller numbers have finite inverse
      return (f128){.hi=sign|F128_INF_HI,.low=F128_INF_LOW};
    }
    expX=1;// exponent 0 uses same power as exponent 1
    expX-=f128_normalizeMantissa(&x);
  }
  // scale x such that 0.5 <= x < 1
  x.hi=(x.hi&F128_HI_MANTISSA_MASK)|((F128_EXP_BIAS-1)<<F128_HI_EXP_SHIFT);
  f128 y=f128_normalizedInv(x);
  // rescale result
  int32_t expY=(y.hi>>F128_HI_EXP_SHIFT)&F128_EXP_MASK;
  expY+=F128_EXP_BIAS-1-expX;
  y.hi&=(F128_HI_MANTISSA_MASK);
  if(expY<=0){ // number underflow
    int shift=1-expY;
    int next=y.low&1<<(shift-1);
    int tail=y.low&((1<<(shift-1))-1);
    y.hi|=F128_HI_HIDDEN_BIT;
    y.low=(y.low>>shift)|(y.hi<<(64-shift));
    y.hi>>=shift;
    if(next&&(tail>0||y.low&1)){ // round to even
      y.low++;
      if(y.low==0){
        y.hi++;
      }
    }
    expY=0;
  }
  y.hi|=sign|((((uint64_t)expY)&F128_EXP_MASK)<<F128_HI_EXP_SHIFT);
  return y;
}
f128 f128_div(f128 x,f128 y){
  uint64_t sign=(x.hi&F128_HI_SIGN_FLAG)^(y.hi&F128_HI_SIGN_FLAG);
  int32_t expX=(x.hi>>F128_HI_EXP_SHIFT)&F128_EXP_MASK;
  int32_t expY=(y.hi>>F128_HI_EXP_SHIFT)&F128_EXP_MASK;
  if(expX==F128_EXP_MASK){
    if(f128_isNaN(x)||expY==F128_EXP_MASK){
      // NaN/y, NaN/NaN, Infinity/Infinity -> NaN
      return (f128){.hi=sign|F128_NAN_HI,.low=F128_NAN_LOW};
    }
    // Infinity/y, Infinity/0 = Infinity
    return (f128){.hi=sign|F128_INF_HI,.low=F128_INF_LOW};
  }else if(expY==F128_EXP_MASK){
    if(f128_isNaN(y)){
      // x/NaN -> NaN
      return (f128){.hi=sign|F128_NAN_HI,.low=F128_NAN_LOW};
    }
    // x/Infinity, 0/Infinity = 0
    return (f128){.hi=sign,.low=0};
  }else if((expY|(y.hi&F128_HI_MANTISSA_MASK)|y.low)==0){// y==0
    if((expX|(x.hi&F128_HI_MANTISSA_MASK)|x.low)==0){// x==0
      // 0 / 0 -> NaN
      return (f128){.hi=sign|F128_NAN_HI,.low=F128_NAN_LOW};
    }
    // x / 0 -> Infinity
    return (f128){.hi=sign|F128_INF_HI,.low=F128_INF_LOW};
  }
  if(expX==0){
    expX=1;// exponent 0 uses same power as exponent 1
    expX-=f128_normalizeMantissa(&x);
  }
  if(expY==0){
    expY=1;// exponent 0 uses same power as exponent 1
    expY-=f128_normalizeMantissa(&y);
  }
  // scale x such that 0.5 <= x < 1
  int32_t delta=expY-F128_EXP_BIAS+1;
  y.hi=(y.hi&F128_HI_MANTISSA_MASK)|((F128_EXP_BIAS-1)<<F128_HI_EXP_SHIFT);
  y=f128_normalizedInv(y);
  // rescale result
  if(expX>delta){
    expX-=delta;
    delta=0;
  }else{ // subnormal result
    // multiply with normal number then rescale to final size
    delta-=(expX-1);
    expX=1;
  }
  x.hi=(x.hi&F128_HI_MANTISSA_MASK)|(((uint64_t)expX)<<F128_HI_EXP_SHIFT);
  x=f128_mult(x,y);
  if(delta>0){
    expX=(x.hi>>F128_HI_EXP_SHIFT)&F128_EXP_MASK;
    expX-=delta;
    if(expX<=0){
      int shift=1-expX;
      int next=x.low&1<<(shift-1);
      int tail=x.low&((1<<(shift-1))-1);
      x.hi|=F128_HI_HIDDEN_BIT;
      x.low=(x.low>>shift)|(x.hi<<(64-shift));
      x.hi>>=shift;
      if(next&&(tail>0||x.low&1)){ // round to even
        x.low++;
        if(x.low==0){
          x.hi++;
        }
      }
      expX=0;
    }
    x.hi&=F128_HI_MANTISSA_MASK;
    x.hi|=(((uint64_t)expX)&F128_EXP_MASK)<<F128_HI_EXP_SHIFT;
  }
  x.hi|=sign;
  return x;
}

int main(void){
  f128 a={(F128_EXP_BIAS<<F128_HI_EXP_SHIFT)|1,0},
       b={(F128_EXP_BIAS<<F128_HI_EXP_SHIFT)|1,0},c=f128_mult(a,b);
  printf("%016lx %016lx\n",a.hi,a.low);
  printf("%016lx %016lx\n",b.hi,b.low);
  printf("%016lx %016lx\n",c.hi,c.low);
  a=(f128){0xfffffffffffflu,0xfffffffffffffffflu};
  b=(f128){((F128_EXP_BIAS)<<F128_HI_EXP_SHIFT)|0xfffffffffffflu,0xfffffffffffffffflu};
  c=f128_mult(a,b);
  printf("%016lx %016lx\n",a.hi,a.low);
  printf("%016lx %016lx\n",b.hi,b.low);
  printf("%016lx %016lx\n",c.hi,c.low);
  a=f128_fromF64(3.141592653589793238);
  b=f128_fromF64(2.718281828459045235);
  c=f128_mult(a,b);
  printf("%016lx %016lx\n",a.hi,a.low);
  printf("%016lx %016lx\n",b.hi,b.low);
  printf("%016lx %016lx\n",c.hi,c.low);
  printf("%f\n",f128_toF64(c));
  double x=2.5e-162;
  a=f128_fromF64(x);
  printf("%016lx %016lx\n",a.hi,a.low);
  printf("%g %g\n",x,f128_toF64(a));
  b=f128_mult(a,a);
  printf("%016lx %016lx\n",b.hi,b.low);
  printf("%g %g\n",x*x,f128_toF64(b));

  a=f128_fromF64(3.141592653589793238);
  b=f128_fromF64(1.4142135623730950488);
  c=f128_add(a,b);
  printf("%016lx %016lx\n",a.hi,a.low);
  printf("%016lx %016lx\n",b.hi,b.low);
  printf("%016lx %016lx\n",c.hi,c.low);
  printf("%f\n",f128_toF64(c));
  c=f128_sub(a,b);
  printf("%016lx %016lx\n",c.hi,c.low);
  printf("%f\n",f128_toF64(c));
  c=f128_sub(b,a);
  printf("%016lx %016lx\n",c.hi,c.low);
  printf("%f\n",f128_toF64(c));
  c=f128_inv(a);
  printf("%016lx %016lx\n",a.hi,a.low);
  printf("%016lx %016lx\n",c.hi,c.low);
  printf("%.12f\n",f128_toF64(c));
  a=(f128){
    .hi=((F128_EXP_MASK-1)<<F128_HI_EXP_SHIFT)|F128_HI_MANTISSA_MASK,
    .low=-1
  };
  b=f128_inv(a);
  c=f128_mult(a,b);
  printf("%016lx %016lx\n",a.hi,a.low);
  printf("%016lx %016lx\n",b.hi,b.low);
  printf("%016lx %016lx\n",c.hi,c.low);
  a=(f128){
    .hi=1ull<<(F128_HI_EXP_SHIFT-2),
    .low=0
  };
  b=f128_inv(a);
  c=f128_mult(a,b);
  printf("%016lx %016lx\n",a.hi,a.low);
  printf("%016lx %016lx\n",b.hi,b.low);
  printf("%016lx %016lx\n",c.hi,c.low);

  a=f128_fromF64(3.141592653589793238);
  b=f128_fromF64(1.4142135623730950488);
  printf("%016lx %016lx\n",a.hi,a.low);
  printf("%016lx %016lx\n",b.hi,b.low);
  c=f128_div(a,b);
  printf("%016lx %016lx\n",c.hi,c.low);
  printf("%f\n",f128_toF64(c));
  c=f128_div(b,a);
  printf("%016lx %016lx\n",c.hi,c.low);
  printf("%f\n",f128_toF64(c));
}


