# 128 bit primitives

An implementation of 128-bit integers & IEEE-floats using portable C

## i128

128-bit integer in twos complement representation

represented as a pair of 64-bit integer

functions:
* `i128_not` bit-wise logical not
* `i128_and` bit-wise logical and
* `i128_or`  bit-wise logical or
* `i128_xor` bit-wise logical xor
* `i128_leftShift` shift bits left by given amount
* `i128_logicalRightShift` shift bits right by given amount, zero extends
* `i64_arithmeticRightShift` portable arithmetic right shift for 64-bit integers
* `i128_arithmeticRightShift` shift bits right by given amount, sign extends
* `i64_highestSetBit` get position of highest 1 bit
* `i128_highestSetBit` get position of highest 1 bit
* `i128_unsignedCompare` negation
* `i128_negate` negation
* `i128_add` addition
* `i128_sub` subtraction
* `i64_bigMult` 128-bit product of two 64-bit integers
* `i128_mult` 128-bit product of two 64-bit integers
* `i128_unsignedDivMod` unsigned division
* `i128_divMod` signed division

## f128

128-bit binary floating point value ( 15 bit exponent, 112 bit mantissa )

represented as a pair of 64-bit integer

functions:
* `f128_fromF64` convert `double` to `f128` ( assumes IEEE 64-bit double )
* `f128_toF64` convert `f128` to `double` ( assumes IEEE 64-bit double )
* `f128_isNaN` checks if number is NaN
* `f128_add` addition
* `f128_sub` subtraction
* `f128_mult` multiplication
* `f128_div` division
* `f128_inv` invert number

