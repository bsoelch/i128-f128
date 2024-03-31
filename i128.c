#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <inttypes.h>

#define I64_HI_BIT 0x8000000000000000ull
#define I32_MASK 0xffffffffull
#define I64_MAX 0xffffffffffffffffull

typedef struct{
  uint64_t hi;
  uint64_t low;
}i128;

i128 i128_not(i128);
i128 i128_and(i128,i128);
i128 i128_or(i128,i128);
i128 i128_xor(i128,i128);
i128 i128_leftShift(i128,unsigned int);
i128 i128_logicalRightShift(i128,unsigned int);
uint64_t i64_arithmeticRightShift(uint64_t,unsigned int);
i128 i128_arithmeticRightShift(i128,unsigned int);
int i64_highestSetBit(uint64_t);
int i128_highestSetBit(i128);
int i128_unsignedCompare(i128,i128);
int i128_compare(i128,i128);
i128 i128_negate(i128);
i128 i128_add(i128,i128);
i128 i128_sub(i128,i128);
i128 i64_bigMult(uint64_t,uint64_t);
i128 i128_mult(i128,i128);
i128 i128_unsignedDivMod(i128,i128,i128*);
i128 i128_divMod(i128,i128,i128*);

i128 i128_not(i128 a){
  return (i128){
    .low=~a.low,
    .hi=~a.hi
  };
}
i128 i128_and(i128 a,i128 b){
  return (i128){
    .low=a.low&b.low,
    .hi=a.hi&b.hi
  };
}
i128 i128_or(i128 a,i128 b){
  return (i128){
    .low=a.low|b.low,
    .hi=a.hi|b.hi
  };
}
i128 i128_xor(i128 a,i128 b){
  return (i128){
    .low=a.low^b.low,
    .hi=a.hi^b.hi
  };
}

i128 i128_leftShift(i128 a,unsigned int k){
  k&=127;
  if(k>=64){
    return (i128){
      .low=0,
      .hi=a.low<<(k-64)
    };
  }else if(k==0){
    return a;
  }
  return (i128){
    .low=a.low<<k,
    .hi=(a.hi<<k)|(a.low >> (64-k))
  };
}
// logical right shift
i128 i128_logicalRightShift(i128 a,unsigned int k){
  k&=127;
  if(k>=64){
    return (i128){
      .low=a.hi>>(k-64),
      .hi=0
    };
  }else if(k==0){
    return a;
  }
  return (i128){
    .low=(a.low>>k) |(a.hi<<(64-k)),
    .hi=a.hi>>k
  };
}
uint64_t i64_arithmeticRightShift(uint64_t a,unsigned int k){
  return (a>>k)|((!!(a&I64_HI_BIT))*(I64_MAX<<(64-k)));
}
// arithmetic right shift
i128 i128_arithmeticRightShift(i128 a,unsigned int k){
  k&=127;
  if(k>=64){
    return (i128){
      .low=i64_arithmeticRightShift(a.hi,k-64),
      .hi=I64_MAX
    };
  }else if(k==0){
    return a;
  }
  return (i128){
    .low=(a.low>>k) |(a.hi<<(64-k)),
    .hi=i64_arithmeticRightShift(a.hi,k)
  };
}

#define POW2_32 1ull<<32
#define POW2_16 1<<16
#define POW2_8  1<<8
#define POW2_4  1<<4
#define POW2_2  1<<2
#define POW2_1  1<<1
int i64_highestSetBit(uint64_t x){
  int res=0;
  if(x>POW2_32){
    res+=32;
    x>>=32;
  }
  if(x>POW2_16){
    res+=16;
    x>>=16;
  }
  if(x>POW2_8){
    res+=8;
    x>>=8;
  }
  if(x>POW2_4){
    res+=4;
    x>>=4;
  }
  if(x>POW2_2){
    res+=2;
    x>>=2;
  }
  if(x>2){
    res+=1;
    x>>=1;
  }
  return res+x;
}
int i128_highestSetBit(i128 x){
  if(x.hi==0)
    return i64_highestSetBit(x.low);
  return i64_highestSetBit(x.hi)+64;
}

int i128_unsignedCompare(i128 a,i128 b){
  if(a.hi<b.hi)
    return -1;
  if(a.hi>b.hi)
    return 1;
  return (a.low<b.low)?-1:(a.hi>b.hi)?1:0;
}
int i128_compare(i128 a,i128 b){
  if((a.hi&I64_HI_BIT)==(b.hi&I64_HI_BIT)){
    // same sign
    return i128_unsignedCompare(a,b);
  }
  return (a.hi&I64_HI_BIT)? -1 : 1;
}

i128 i128_negate(i128 a){
  return (i128){
    .low=-a.low,
    .hi=~a.hi+(a.low==0)
  };
}

i128 i128_add(i128 a,i128 b){
  return (i128){
    .low=a.low+b.low,
    .hi=a.hi+b.hi+!!((a.low&I64_HI_BIT)&(b.low&I64_HI_BIT))
  };
}
i128 i128_sub(i128 a,i128 b){
  return (i128){
    .low=a.low-b.low,
    .hi=a.hi-b.hi+(a.low<b.low)
  };
}

i128 i64_bigMult(uint64_t a,uint64_t b){
  uint64_t aLow=a&I32_MASK;
  uint64_t aHi=a>>32;
  uint64_t bLow=b&I32_MASK;
  uint64_t bHi=b>>32;
  uint64_t x00=aLow*bLow;
  uint64_t x01=aLow*bHi;
  uint64_t x10=aHi*bLow;
  uint64_t x11=aHi*bHi;
  uint64_t carry=((x00>>32)+(x01&I32_MASK)+(x10&I32_MASK))>>32;
  return (i128){
    .low=x00+((x01+x10)<<32),
    .hi=x11+((x01+x10)>>32)+carry
  };
}
i128 i128_mult(i128 a,i128 b){
  // (a+b*2^64) (c+d*2^64) = a*c+2^64*(a*d+b*c)+2^128*(b*d)
  i128 x00=i64_bigMult(a.low,b.low);
  return (i128){
    .low=x00.low,
    .hi=x00.hi+a.low*b.hi+a.hi*b.low
  };
}

i128 i128_unsignedDivMod(i128 a,i128 b,i128* mod){
  if((a.hi<b.hi)||((a.hi==b.hi) && (a.low<=b.low))){ // a <= b
    if(mod!=NULL){
      *mod=a;
    }
    return (i128){
      .hi=0,
      .low=0
    };
  }
  if(a.hi==0){ // both fit in 64 bits
    if(mod!=NULL){
      *mod=(i128){
        .hi=0,
        .low=a.low%b.low
      };
    }
    return (i128){
      .hi=0,
      .low=a.low/b.low
    };
  }
  // TODO handle more special cases ( b.hi=0,b.low=0,b=2^k, ... )
  int shift=i128_highestSetBit(a)-i128_highestSetBit(b);
  i128 bit={.hi=0,.low=0},res={.hi=0,.low=0};
  i128 div=i128_leftShift(b,shift);
  if(shift>=64){
    bit.hi=1ull<<(shift-64);
  }else{
    bit.low=1ull<<shift;
  }
  while(bit.hi|bit.low){
    if(i128_unsignedCompare(a,div)>=0){
      a = i128_sub(a,div);
      res = i128_or(res,bit);
    }
    div=i128_logicalRightShift(div,1);
    bit=i128_logicalRightShift(bit,1);
  }
  if(mod!=NULL){
    *mod=a;
  }
  return res;
}
// TODO check if signs are correct
i128 i128_divMod(i128 a,i128 b,i128* mod){
  bool resSign=false,modSign=false;
  if(a.hi&I64_HI_BIT){
    a=i128_negate(a);
    resSign=!resSign;
    modSign=true;
  }
  if(b.hi&I64_HI_BIT){
    b=i128_negate(b);
    resSign=!resSign;
  }
  i128 res=i128_unsignedDivMod(a,b,mod);
  if(resSign){
    res=i128_negate(res);
  }
  if(modSign&&mod!=NULL){
    *mod=i128_negate(*mod);
  }
  return res;
}

int main(void){
  i128 a={.hi=0x0123456789abcdef,.low=0xfedcba9876543210};
  i128 b={.hi=0,.low=0x1111111111111111};
  i128 m;
  i128 c=i128_divMod(a,b,&m);
  printf("%016lx %016lx\n",a.hi,a.low);
  printf("%016lx %016lx\n",b.hi,b.low);
  printf("%016lx %016lx\n",c.hi,c.low);
  printf("%016lx %016lx\n",m.hi,m.low);
  c=i128_mult(b,c);
  printf("%016lx %016lx\n",c.hi,c.low);
  c=i128_add(c,m);
  printf("%016lx %016lx\n",c.hi,c.low);
  a=(i128){.hi=0xffffffffffffffff,.low=0xffffffffffff0000};
  b=(i128){.hi=0,.low=3};
  c=i128_divMod(a,b,&m);
  printf("%016lx %016lx\n",a.hi,a.low);
  printf("%016lx %016lx\n",b.hi,b.low);
  printf("%016lx %016lx\n",c.hi,c.low);
  printf("%016lx %016lx\n",m.hi,m.low);
  c=i128_mult(b,c);
  printf("%016lx %016lx\n",c.hi,c.low);
  c=i128_add(c,m);
  printf("%016lx %016lx\n",c.hi,c.low);
}

