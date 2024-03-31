#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <inttypes.h>

#define I64_HI_BIT 0x8000000000000000ull

#define HI_SIGN_FLAG 0x8000000000000000ull
#define HI_EXP_SHIFT 48
#define EXP_MASK  0x7fffull
#define EXP_BIAS  0x3fffull
#define HI_HIDDEN_BIT   0x1000000000000ull
#define HI_MANTISSA_MASK 0xffffffffffffull

#define F128_NAN_HI  0x7fffffffffffffffull
#define F128_NAN_LOW 0xffffffffffffffffull

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
f128 f128_fromF64(double x);
double f128_toF64(f128 x);

bool f128_isNaN(f128 x);
f128 f128_mult(f128 x,f128 y);
f128 f128_add(f128 x,f128 y);
f128 f128_sub(f128 x,f128 y);

int f128_normalizeMantissa(f128* x);

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
  uint64_t hi= mantissa >> ( F64_EXP_SHIFT - HI_EXP_SHIFT );
  uint64_t low= mantissa << ( 64 + HI_EXP_SHIFT - F64_EXP_SHIFT );
  f128 r=(f128){.hi=hi,.low=low};
  if(exp==0){// subnormal numbers
    // +1 to compensate exponent offset
    exp=EXP_BIAS-F64_EXP_BIAS-f128_normalizeMantissa(&r)+1;
    r.hi&=HI_MANTISSA_MASK;
  }else{
    exp+=EXP_BIAS-F64_EXP_BIAS;//update bias
    // NaN / Infinity get mapped to NaN / Infinity
  }
  r.hi|=sign|(exp<<HI_EXP_SHIFT);
  return r;
}
double f128_toF64(f128 x){
  uint64_t sign=x.hi&HI_SIGN_FLAG;
  uint64_t exp=(x.hi>>HI_EXP_SHIFT)&EXP_MASK;
  uint64_t mantissaHi=x.hi&HI_MANTISSA_MASK;
  uint64_t mantissaLow=x.low;
  uint64_t mantissa= mantissaHi << ( F64_EXP_SHIFT - HI_EXP_SHIFT );
  mantissa |= mantissaLow >> ( 64 + HI_EXP_SHIFT - F64_EXP_SHIFT );
  // TODO round instead of truncate
  if(exp==EXP_MASK){ // inf/NaN
    exp=F64_EXP_MASK;
  }else if(exp>=EXP_BIAS+F64_EXP_MASK){ // overflow (exp - bias > f64 mask)
    exp=F64_EXP_MASK;
    mantissa=0;
  }else if(exp+F64_EXP_BIAS<=EXP_BIAS){ // underflow ( exp -bias + f64_bias <= 0 )
    uint64_t shift=EXP_BIAS-(exp+F64_EXP_BIAS)+1;// +1 for hidden bit
    if(shift<=F64_EXP_SHIFT){
      mantissa=(mantissa|F64_HIDDEN_BIT)>>shift;
    }else{
      mantissa=0;
    }
    exp=0;
  }else{
    exp+=F64_EXP_BIAS;
    exp-=EXP_BIAS;
  }
  return (f64_bits){.i64=sign|(exp<<F64_EXP_SHIFT)|mantissa}.f64;
}

bool f128_isNaN(f128 x){
  if(((x.hi>>HI_EXP_SHIFT)&EXP_MASK)!=EXP_MASK)
    return false;
  return (x.hi&HI_MANTISSA_MASK)|x.low;
}

int f128_normalizeMantissa(f128* x){
  if(((x->hi>>HI_EXP_SHIFT)&EXP_MASK)!=0)
    return 0;//already normalized
  x->hi&=HI_MANTISSA_MASK;// set sign bit to zero
  int shift=0,baseShift=0;
  // shift until high part non-zero
  if(x->hi==0){
    if(x->low<=(HI_HIDDEN_BIT|HI_MANTISSA_MASK)){ // no high bits set
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

f128 f128_mult(f128 x,f128 y){
  uint64_t sign=(x.hi&HI_SIGN_FLAG)^(y.hi&HI_SIGN_FLAG);
  int32_t expX=(x.hi>>HI_EXP_SHIFT)&EXP_MASK;
  int32_t expY=(y.hi>>HI_EXP_SHIFT)&EXP_MASK;
  if(expX==EXP_MASK){//x infinity and NaN
    if(f128_isNaN(x)||f128_isNaN(y)||((y.hi|y.low)==0)){
      // NaN*y, x*NaN, Infinity*0 -> NaN
      return (f128){.hi=sign|F128_NAN_HI,.low=F128_NAN_LOW};
    }
    // Infinity*y -> Infinity
    return (f128){.hi=sign|(EXP_MASK<<HI_EXP_SHIFT),.low=0};
  }else if(expY==EXP_MASK){//y infinity or NaN
    if(f128_isNaN(y)||((x.hi|x.low)==0)){ // x is Finite by first if
      // x*NaN, +*Infinity -> NaN
      return (f128){.hi=sign|F128_NAN_HI,.low=F128_NAN_LOW};
    }
    // x*Infinity -> Infinity
    return (f128){.hi=sign|(EXP_MASK<<HI_EXP_SHIFT),.low=0};
  }
  x.hi&=HI_MANTISSA_MASK;
  y.hi&=HI_MANTISSA_MASK;
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
  uint64_t a = ((x.hi >> 32) & 0xffffu) | 0x10000u;
  uint64_t b = (x.hi) & 0xffffffffu;
  uint64_t c = (x.low >> 32) & 0xffffffffu;
  uint64_t d = (x.low) & 0xffffffffu;
  uint64_t e = ((y.hi >> 32) & 0xffffu) | 0x10000u;
  uint64_t f = (y.hi) & 0xffffffffu;;
  uint64_t g = (y.low >> 32) & 0xffffffffu;
  uint64_t h = (y.low) & 0xffffffffu;
  uint64_t low=0,mid=0,hi=0,tmp;
  int nz=0, leading;// low bits are not zero
  // have to calculate all digits to get correct rounding
  // offset 0: dh
  tmp=d*h;
  low+=tmp&0xffffffff;
  mid+=tmp>>32;
  // shift result by 32
  nz+=low!=0;
  low=mid;mid=0;
  // offset 32: ch dg
  tmp=c*h;
  low+=tmp&0xffffffff;
  mid+=tmp>>32;
  tmp=d*g;
  low+=tmp&0xffffffff;
  mid+=tmp>>32;
  // shift result by 32
  nz+=low!=0;
  low=mid;mid=0;
  // offset 64: bh cg df
  tmp=b*h;
  low+=tmp&0xffffffff;
  mid+=tmp>>32;
  tmp=c*g;
  low+=tmp&0xffffffff;
  mid+=tmp>>32;
  tmp=d*f;
  low+=tmp&0xffffffff;
  mid+=tmp>>32;
  // shift result by 32
  nz+=low!=0;
  low=mid;mid=0;
  // offset 96: ah bg cf de
  tmp=a*h;
  low+=tmp&0xffffffff;
  mid+=tmp>>32;
  tmp=b*g;
  low+=tmp&0xffffffff;
  mid+=tmp>>32;
  tmp=c*f;
  low+=tmp&0xffffffff;
  mid+=tmp>>32;
  tmp=d*e;
  low+=tmp&0xffffffff;
  mid+=tmp>>32;
  // shift result by 16
  nz+=(low&0xffff)!=0;
  leading=(low&0x8000)!=0;//next bit after number
  low=(low>>16)|(mid&0xffff)<<16;
  mid>>=16;
  // offset 128: ag bf ce
  tmp=a*g;
  low+=(tmp&0xffff)<<16;
  mid+=tmp>>16;
  tmp=b*f;
  low+=(tmp&0xffff)<<16;
  mid+=tmp>>16;
  tmp=c*e;
  low+=(tmp&0xffff)<<16;
  mid+=tmp>>16;
  // offset 160: af be
  tmp=a*f;
  mid+=(tmp&0xffff)<<16;
  hi+=tmp>>16;
  tmp=b*e;
  mid+=(tmp&0xffff)<<16;
  hi+=tmp>>16;
  // offset 192: ae
  hi+=(a*e)<<16;// a,e<2^17 -> ae < 2^34
  // merge numbers to one 128-bit number in hi:low
  mid+=(low>>32)&0xffffffff;
  low=(low&0xffffffff)|((mid&0xffffffff)<<32);
  hi+=(mid>>32)&0xffffffff;
  // truncate number to 112 bits
  tmp = hi >> 48; // should be between 2 and 4 (inclusive)
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
    }
  }
  low >>=shift;
  if(shift!=0){
    low |= hi<<(64-shift);
  }
  hi =(hi>>shift) & 0xffffffffffff;
  // sum contains bias twice -> compensate for extra bias
  int32_t exp=expX+expY+shift-EXP_BIAS;
  if(exp>=(int64_t)EXP_MASK){ // overflow
    exp=EXP_MASK;
    hi=low=0;
  }else if(exp<1){ // underflow
    hi|=HI_HIDDEN_BIT;
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
  return (f128){.hi=sign|(((uint64_t)exp)<<HI_EXP_SHIFT)|hi,.low=low};
}

f128 f128_add(f128 x,f128 y){
  if((x.hi&HI_SIGN_FLAG)!=(y.hi&HI_SIGN_FLAG)){
    y.hi^=HI_SIGN_FLAG;
    return f128_sub(x,y);
  }
  uint64_t sign=x.hi&HI_SIGN_FLAG;
  int32_t expX=(x.hi>>HI_EXP_SHIFT)&EXP_MASK;
  int32_t expY=(y.hi>>HI_EXP_SHIFT)&EXP_MASK;
  if(expX==EXP_MASK){
    if(f128_isNaN(x)||f128_isNaN(y)){
      // NaN+y, x+NaN -> NaN
      return (f128){.hi=sign|F128_NAN_HI,.low=F128_NAN_LOW};
    }
    // Infinity+y -> Infinity
    return (f128){.hi=sign|(EXP_MASK<<HI_EXP_SHIFT),.low=0};
  }else if(expY==EXP_MASK){
    if(f128_isNaN(y)){
      // x+NaN -> NaN
      return (f128){.hi=sign|F128_NAN_HI,.low=F128_NAN_LOW};
    }
    // x+Infinity -> Infinity
    return (f128){.hi=sign|(EXP_MASK<<HI_EXP_SHIFT),.low=0};
  }
  // underflow
  if(expX>expY+113) // difference of exponents > 113 -> no change of larger number
    return x;
  if(expY>expX+113)
    return y;
  x.hi&=HI_MANTISSA_MASK;
  y.hi&=HI_MANTISSA_MASK;
  // insert hidden bit
  if(expX==0){
    expX=1;
  }else{
    x.hi |= HI_HIDDEN_BIT;
  }
  if(expY==0){
    expY=1;
  }else{
    y.hi |= HI_HIDDEN_BIT;
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
  x.hi+=y.hi+!!((x.low&I64_HI_BIT)&(y.low&I64_HI_BIT));
  x.low+=y.low;
  // overflow (at most by one bit)
  if(x.hi>(HI_HIDDEN_BIT|HI_MANTISSA_MASK)){
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
  if(x.hi>(HI_HIDDEN_BIT|HI_MANTISSA_MASK)){
    // when rounding overflows, then x.low is zero
    x.low=x.hi<<63;
    x.hi>>=1;
    exp++;
  }
  if(exp>=EXP_MASK){// number overflowed
    exp=EXP_MASK;
    x.hi=x.low=0;
  }
  x.hi&=HI_MANTISSA_MASK;
  x.hi|=sign|(exp<<HI_EXP_SHIFT);
  return x;
}
// TODO check if subtraction works correctly for edge cases
f128 f128_sub(f128 x,f128 y){
  if((x.hi&HI_SIGN_FLAG)!=(y.hi&HI_SIGN_FLAG)){
    y.hi^=HI_SIGN_FLAG;
    return f128_add(x,y);
  }
  uint64_t sign=x.hi&HI_SIGN_FLAG;
  int32_t expX=(x.hi>>HI_EXP_SHIFT)&EXP_MASK;
  int32_t expY=(y.hi>>HI_EXP_SHIFT)&EXP_MASK;
  if(expX==EXP_MASK){
    if(f128_isNaN(x)||expY==EXP_MASK){
      // NaN-y, x-NaN, Infinity-Infinity -> NaN
      return (f128){.hi=F128_NAN_HI,.low=F128_NAN_LOW};
    }
    // Infinity-y -> Infinity
    return (f128){.hi=sign|(EXP_MASK<<HI_EXP_SHIFT),.low=0};
  }else if(expY==EXP_MASK){
    if(f128_isNaN(y)){
      // x-NaN -> NaN
      return (f128){.hi=F128_NAN_HI,.low=F128_NAN_LOW};
    }
    // x-Infinity -> -Infinity
    return (f128){.hi=(sign^HI_SIGN_FLAG)|(EXP_MASK<<HI_EXP_SHIFT),.low=0};
  }
  if(expX>expY+113) // difference of exponents > 113 -> no change of larger number
    return x;
  if(expY>expX+113)
    return y;
  x.hi&=HI_MANTISSA_MASK;
  y.hi&=HI_MANTISSA_MASK;
  // insert hidden bit
  if(expX==0){
    expX=1;
  }else{
    x.hi |= HI_HIDDEN_BIT;
  }
  if(expY==0){
    expY=1;
  }else{
    y.hi |= HI_HIDDEN_BIT;
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
    sign^=HI_SIGN_FLAG;
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
      if(x.low==0xffffffffffffffffull){// underflow
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
  if(exp<=0){
    exp=0;
    // undo last shift in normalization
    x.low=(x.low>>1)|(x.hi<<63);
    x.hi>>=1;
  }
  x.hi&=HI_MANTISSA_MASK;
  x.hi|=sign|(exp<<HI_EXP_SHIFT);
  return x;
}

/*
uses Newton-Raphson-Division (https://en.wikipedia.org/wiki/Division_algorithm#Newton%E2%80%93Raphson_division)
*/
f128 f128_inv(f128 x){
  uint64_t sign=x.hi&HI_SIGN_FLAG;
  int32_t expX=(x.hi>>HI_EXP_SHIFT)&EXP_MASK;
  if(expX==EXP_MASK){
    // TODO non-finite numbers
  }else if(expX==0){
    // TODO subnormal numbers
  }
  // scale x such that 0.5 <= x < 1
  x.hi=(x.hi&HI_MANTISSA_MASK)|((EXP_BIAS-1)<<HI_EXP_SHIFT);
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
  // TODO check if exponent can over/underflow
  // rescale result
  int32_t expY=(y.hi>>HI_EXP_SHIFT)&EXP_MASK;
  expY+=EXP_BIAS-1-expX;
  y.hi&=(HI_MANTISSA_MASK);
  y.hi|=sign|((((uint64_t)expY)&EXP_MASK)<<HI_EXP_SHIFT);
  return y;
}

// TODO  div

int main(void){
  f128 a={(EXP_BIAS<<HI_EXP_SHIFT)|1,0},
       b={(EXP_BIAS<<HI_EXP_SHIFT)|1,0},c=f128_mult(a,b);
  printf("%016lx %016lx\n",a.hi,a.low);
  printf("%016lx %016lx\n",b.hi,b.low);
  printf("%016lx %016lx\n",c.hi,c.low);
  a=(f128){0xfffffffffffflu,0xfffffffffffffffflu};
  b=(f128){((EXP_BIAS)<<HI_EXP_SHIFT)|0xfffffffffffflu,0xfffffffffffffffflu};
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
}


