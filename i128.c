#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <inttypes.h>

#define I64_HI_BIT 0x8000000000000000ull

typedef struct{
  uint64_t hi;
  uint64_t low;
}I128;


I128 I128_NOT(I128 a){
  return (I128){
    .low=~a.low,
    .hi=~a.hi
  };
}
I128 I128_TWOS_COMPLEMENT(I128 a){
  return (I128){
    .low=-a.low,
    .hi=~a.hi+(a.low==0)
  };
}
I128 I128_AND(I128 a,I128 b){
  return (I128){
    .low=a.low&b.low,
    .hi=a.hi&b.hi
  };
}
I128 I128_OR(I128 a,I128 b){
  return (I128){
    .low=a.low|b.low,
    .hi=a.hi|b.hi
  };
}
I128 I128_XOR(I128 a,I128 b){
  return (I128){
    .low=a.low^b.low,
    .hi=a.hi^b.hi
  };
}

I128 I128_LSHIFT(I128 a,unsigned int k){
  k&=127;
  if(k>=64){
    return (I128){
      .low=0,
      .hi=a.low<<(k-64)
    };
  }else if(k==0){
    return a;
  }
  return (I128){
    .low=a.low<<k,
    .hi=(a.hi<<k)|(a.low >> (64-k))
  };
}
// logical right shift
I128 I128_LRSHIFT(I128 a,unsigned int k){
  k&=127;
  if(k>=64){
    return (I128){
      .low=a.hi>>(k-64),
      .hi=0
    };
  }else if(k==0){
    return a;
  }
  return (I128){
    .low=(a.low>>k) |(a.hi<<(64-k)),
    .hi=a.hi>>k
  };
}
uint64_t i64_ashr(uint64_t a,unsigned int k){
  return (a>>k)|((!!(a&I64_HI_BIT))*(0xffffffffffffffffull<<(64-k)));
}
// arithmetic right shift
I128 I128_ARSHIFT(I128 a,unsigned int k){
  k&=127;
  if(k>=64){
    return (I128){
      .low=i64_ashr(a.hi,k-64),
      .hi=0xffffffffffffffffull
    };
  }else if(k==0){
    return a;
  }
  return (I128){
    .low=(a.low>>k) |(a.hi<<(64-k)),
    .hi=i64_ashr(a.hi,k)
  };
}


I128 I128_ADD(I128 a,I128 b){
  return (I128){
    .low=a.low+b.low,
    .hi=a.hi+b.hi+!!((a.low&I64_HI_BIT)&(b.low&I64_HI_BIT))
  };
}
I128 I128_SUBT(I128 a,I128 b){
  return (I128){
    .low=a.low-b.low,
    .hi=a.hi-b.hi+(a.low<b.low)
  };
}

int I128_UCMP(I128 a,I128 b){
  if(a.hi<b.hi)
    return -1;
  if(a.hi>b.hi)
    return 1;
  return (a.low<b.low)?-1:(a.hi>b.hi)?1:0;
}

#define U32_MASK 0xffffffff
I128 I64_BIG_MULT(uint64_t a,uint64_t b){
  uint64_t aLow=a&U32_MASK;
  uint64_t aHi=a>>32;
  uint64_t bLow=b&U32_MASK;
  uint64_t bHi=b>>32;
  uint64_t x00=aLow*bLow;
  uint64_t x01=aLow*bHi;
  uint64_t x10=aHi*bLow;
  uint64_t x11=aHi*bHi;
  uint64_t carry=((x00>>32)+(x01&U32_MASK)+(x10&U32_MASK))>>32;
  return (I128){
    .low=x00+((x01+x10)<<32),
    .hi=x11+((x01+x10)>>32)+carry
  };
}
I128 I128_MULT(I128 a,I128 b){
  // (a+b*2^64) (c+d*2^64) = a*c+2^64*(a*d+b*c)+2^128*(b*d)
  I128 x00=I64_BIG_MULT(a.low,b.low);
  return (I128){
    .low=x00.low,
    .hi=x00.hi+a.low*b.hi+a.hi*b.low
  };
}
#define POW2_32 1ull<<32
#define POW2_16 1<<16
#define POW2_8  1<<8
#define POW2_4  1<<4
#define POW2_2  1<<2
#define POW2_1  1<<1
int I64_BIT_COUNT(uint64_t x){
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
int I128_BIT_COUNT(I128 x){
  if(x.hi==0)
    return I64_BIT_COUNT(x.low);
  return I64_BIT_COUNT(x.hi)+64;
}
//I128 DIV UDIV, MOD,UMOD
I128 I128_UDIVMOD(I128 a,I128 b,I128* mod){
  if((a.hi<b.hi)||((a.hi==b.hi) && (a.low<=b.low))){ // a <= b
    if(mod!=NULL){
      *mod=a;
    }
    return (I128){
      .hi=0,
      .low=0
    };
  }
  if(a.hi==0){ // both fit in 64 bits
    if(mod!=NULL){
      *mod=(I128){
        .hi=0,
        .low=a.low%b.low
      };
    }
    return (I128){
      .hi=0,
      .low=a.low/b.low
    };
  }
  // TODO handle more special cases ( b.hi=0,b.low=0,b=2^k, ... )
  int shift=I128_BIT_COUNT(a)-I128_BIT_COUNT(b);
  I128 bit={.hi=0,.low=0},res={.hi=0,.low=0};
  I128 div=I128_LSHIFT(b,shift);
  if(shift>=64){
    bit.hi=1ull<<(shift-64);
  }else{
    bit.low=1ull<<shift;
  }
  while(bit.hi|bit.low){
    if(I128_UCMP(a,div)>=0){
      a = I128_SUBT(a,div);
      res = I128_OR(res,bit);
    }
    div=I128_LRSHIFT(div,1);
    bit=I128_LRSHIFT(bit,1);
  }
  if(mod!=NULL){
    *mod=a;
  }
  return res;
}
// TODO check if signs are correct
I128 I128_DIVMOD(I128 a,I128 b,I128* mod){
  bool resSign=false,modSign=false;
  if(a.hi&I64_HI_BIT){
    a=I128_TWOS_COMPLEMENT(a);
    resSign=!resSign;
    modSign=true;
  }
  if(b.hi&I64_HI_BIT){
    b=I128_TWOS_COMPLEMENT(b);
    resSign=!resSign;
  }
  I128 res=I128_UDIVMOD(a,b,mod);
  if(resSign){
    res=I128_TWOS_COMPLEMENT(res);
  }
  if(modSign&&mod!=NULL){
    *mod=I128_TWOS_COMPLEMENT(*mod);
  }
  return res;
}

int main(void){
  I128 a={.hi=0x0123456789abcdef,.low=0xfedcba9876543210};
  I128 b={.hi=0,.low=0x1111111111111111};
  I128 m;
  I128 c=I128_DIVMOD(a,b,&m);
  printf("%016lx %016lx\n",a.hi,a.low);
  printf("%016lx %016lx\n",b.hi,b.low);
  printf("%016lx %016lx\n",c.hi,c.low);
  printf("%016lx %016lx\n",m.hi,m.low);
  c=I128_MULT(b,c);
  printf("%016lx %016lx\n",c.hi,c.low);
  c=I128_ADD(c,m);
  printf("%016lx %016lx\n",c.hi,c.low);
  a=(I128){.hi=0xffffffffffffffff,.low=0xffffffffffff0000};
  b=(I128){.hi=0,.low=3};
  c=I128_DIVMOD(a,b,&m);
  printf("%016lx %016lx\n",a.hi,a.low);
  printf("%016lx %016lx\n",b.hi,b.low);
  printf("%016lx %016lx\n",c.hi,c.low);
  printf("%016lx %016lx\n",m.hi,m.low);
  c=I128_MULT(b,c);
  printf("%016lx %016lx\n",c.hi,c.low);
  c=I128_ADD(c,m);
  printf("%016lx %016lx\n",c.hi,c.low);
}

