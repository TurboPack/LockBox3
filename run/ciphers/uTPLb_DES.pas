{* ***** BEGIN LICENSE BLOCK *****
Copyright 2010 Sean B. Durkin
This file is part of TurboPower LockBox 3. TurboPower LockBox 3 is free
software being offered under a dual licensing scheme: LGPL3 or MPL1.1.

The contents of this file are subject to the Mozilla Public License (MPL)
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Alternatively, you may redistribute it and/or modify it under the terms of
the GNU Lesser General Public License (LGPL) as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

You should have received a copy of the Lesser GNU General Public License
along with TurboPower LockBox 3.  If not, see <http://www.gnu.org/licenses/>.

TurboPower LockBox is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. In relation to LGPL,
see the GNU Lesser General Public License for more details. In relation to MPL,
see the MPL License for the specific language governing rights and limitations
under the License.

The Initial Developer of the Original Code for TurboPower LockBox version 2
and earlier was TurboPower Software.

 * ***** END LICENSE BLOCK ***** *}

unit uTPLb_DES;

interface

uses
  SysUtils, Classes, uTPLb_BlockCipher, uTPLb_StreamCipher;

type
  TDES = class(TInterfacedObject, IBlockCipher, ICryptoGraphicAlgorithm)
  private
    function  DisplayName: string;
    function  ProgId: string;
    function  Features: TAlgorithmicFeatureSet;
    function  DefinitionURL: string;
    function  WikipediaReference: string;
    function  GenerateKey( Seed: TStream): TSymetricKey;
    function  LoadKeyFromStream( Store: TStream): TSymetricKey;
    function  BlockSize: integer;  // in units of bits. Must be a multiple of 8.
    function  KeySize: integer;
    function  SeedByteSize: integer; // Size that the input of the GenerateKey must be.
    function  MakeBlockCodec( Key: TSymetricKey): IBlockCodec;
    function  SelfTest_Key: TBytes;
    function  SelfTest_Plaintext: TBytes;
    function  SelfTest_Ciphertext: TBytes;

  public
    constructor Create;
  end;


TExpandedKey = array[ 0 .. 15 ] of uint64;

{$IF CompilerVersion < 21}
uint32 = cardinal;
{$IFEND}
// The following routines are exposed only for the sake of D-Unit tests.
procedure IP_Transform( Datum: uint64; var L, R: uint32);
procedure IP_InverseTransform( L, R: uint32; var Datum: uint64);
procedure DES_EncryptBlock(
  Plaintext: uint64; var Ciphertext: uint64; const Key: TExpandedKey);
procedure DES_DecryptBlock(
  Ciphertext: uint64; var Plaintext: uint64; const Key: TExpandedKey);
procedure ExpandKey( Key: uint64; var Ex: TExpandedKey);
function PC_1( K: uint64): uint64;
function PC_2( L, R: uint32): uint64;
function E_Bit_Selection( R: uint32): uint64;
procedure SetParityBitsOnKey( var K: uint64);
function  hasCorrectParity( K: uint64): boolean;

implementation









uses uTPLb_Constants, uTPLb_I18n, uTPLb_StrUtils;

{ Information Resources
  =====================
(1) http://csrc.nist.gov/publications/fips/fips46-3/fips46-3.pdf
(2) http://www.tropsoft.com/strongenc/des.htm
(3) http://orlingrabbe.com/des.htm
(4) http://en.wikipedia.org/wiki/Data_Encryption_Standard
(5) http://www.itl.nist.gov/fipspubs/fip81.htm

}



{ The DES standard describes a transform of 64 bits to 64 bits, with
the bits numbered from 1 to 64 (see figure 1 of FIBS PUB 46-3).
Somewhat annoyingly it does not define how these 64 bits are encoded
in an 8-bit byte stream, in relation to endien-ness. There are at least
4 different endien encodings possible. This ambiguity in the standard is not
cryptographically significant, but it does impact on implementation.

In LockBox 3, the DES algorithm will use the following endien encoding:
 * Bit 1 (designated b1) is encoded in the most significant bit (MSB) of byte address 0.
 * Bit 2 (designated b2) is encoded in the 2nd MSB of byte address 0.
 * b8 is encoded in the LSB of byte address 0.
 * b9 is encoded in the MSB of byte address 1.
 * b64 is encoded in the LSB of byte address 7.
}

type
TDESKey = class( TSymetricKey)
  private
    FNativeKey  : uint64;
    FExpandedKey: TExpandedKey;

  public
    constructor Create( NativeKey1: uint64);
    constructor CreateFromStream( Store: TStream);

    procedure   SaveToStream( Stream: TStream);  override;
    procedure   Burn;                            override;
  end;


TDESCodec = class( TInterfacedObject, IBlockCodec)
  private
    FLockBoxKey: TDESKey;

    constructor Create( LockBoxKey1: TDESKey);
    procedure Encrypt_Block( Plaintext{in}, Ciphertext{out}: TMemoryStream);
    procedure Decrypt_Block( Plaintext{out}, Ciphertext{in}: TMemoryStream);
    procedure Reset;
    procedure Burn;
  end;



function PC_1( K: uint64): uint64;
var
  L, R: uint32;
  t8: byte;
  t32: uint32;
  t16: word;
begin
// input K = 57 58 59 60 61 62 63 64 | 49 50 51 52 53 54 55 56 | 41 42 43 44 45 46 47 48 | 33 34 35 36 37 38 39 40 | 25 26 27 28 29 30 31 32 | 17 18 19 20 21 22 23 24 | 9 10 11 12 13 14 15 16  1  2  3  4  5  6  7  8

//PC-1 output =
//57   49    41   33    25    17    9
// 1   58    50   42    34    26   18
//10    2    59   51    43    35   27
//19   11     3   60    52    44   36
//63   55    47   39    31    23   15
// 7   62    54   46    38    30   22
//14    6    61   53    45    37   29
//21   13     5   28    20    12    4

// We desire result, such that:
// result.Bytes[0] = 57   49   41   33    25   17    9   1
// result.Bytes[1] = 58   50   42   34    26   18   10   2
// result.Bytes[2] = 59   51   43   35    27   19   11   3
// result.Bytes[3] = 60   52   44   36    63   55   47  39
// result.Bytes[4] = 31   23   15    7    62   54   46  38
// result.Bytes[5] = 30   22   14    6    61   53   45  37
// result.Bytes[6] = 29   21   13    5    28   20   12   4
// result.Bytes[7] =  0    0    0    0     0    0    0   0

// In other words we wish to go from LR to L'R' where:
//L  = 25 26 27 28 29 30 31 32 17 18 19 20 21 22 23 24  9 10 11 12 13 14 15 16  1  2  3  4  5  6  7  8
//R  = 57 58 59 60 61 62 63 64 49 50 51 52 53 54 55 56 41 42 43 44 45 46 47 48 33 34 35 36 37 38 39 40
//
//L' = 60 52 44 36 63 55 47 39 59 51 43 35 27 19 11  3 58 50 42 34 26 18 10  2 57 49 41 33 25 17  9  1
//R' =  0 0   0  0  0  0  0  0 29 21 13  5 28 20 12  4 30 22 14  6 61 53 45 37 31 23 15  7 62 54 46 38

// Sample data:
//K = (big-endien) 00010011 00110100 01010111 01111001 10011011 10111100 11011111 11110001
//K = (little-endien) 11110001 11011111 10111100  10011011 01111001 01010111 00110100 00010011
//L  = 25 26 27 28 29 30 31 32 17 18 19 20 21 22 23 24  9 10 11 12 13 14 15 16  1  2  3  4  5  6  7  8
//      0  1  1  1  1  0  0  1  0  1  0  1  0  1  1  1  0  0  1  1  0  1  0  0  0  0  0  1  0  0  1  1
//R  = 57 58 59 60 61 62 63 64 49 50 51 52 53 54 55 56 41 42 43 44 45 46 47 48 33 34 35 36 37 38 39 40
//      1  1  1  1  0  0  0  1  1  1  0  1  1  1  1  1  1  0  1  1  1  1  0  0  1  0  0  1  1  0  1  1

// Origin. Start with:
L := Int64Rec( K).Lo;
R := Int64Rec( K).Hi;
//L  = 25 26 27 28 29 30 31 32 17 18 19 20 21 22 23 24  9 10 11 12 13 14 15 16  1  2  3  4  5  6  7  8
//R  = 57 58 59 60 61 62 63 64 49 50 51 52 53 54 55 56 41 42 43 44 45 46 47 48 33 34 35 36 37 38 39 40
//      0  1  1  1  1  0  0  1  0  1  0  1  0  1  1  1  0  0  1  1  0  1  0  0  0  0  0  1  0  0  1  1
//      1  1  1  1  0  0  0  1  1  1  0  1  1  1  1  1  1  0  1  1  1  1  0  0  1  0  0  1  1  0  1  1


// Step 1. Center byte swap.
t8 := LongRec( L).Bytes[2];
LongRec( L).Bytes[2] := LongRec( L).Bytes[1];
LongRec( L).Bytes[1] := t8;
t8 := LongRec( R).Bytes[2];
LongRec( R).Bytes[2] := LongRec( R).Bytes[1];
LongRec( R).Bytes[1] := t8;
//L = 25 26 27 28 29 30 31 32 |  9 10 11 12 13 14 15 16 | 17 18 19 20 21 22 23 24 |  1  2  3  4  5  6  7  8
//R = 57 58 59 60 61 62 63 64 | 41 42 43 44 45 46 47 48 | 49 50 51 52 53 54 55 56 | 33 34 35 36 37 38 39 40
//     0  1  1  1  1  0  0  1 | 0  0  1  1  0  1  0  0  |  0  1  0  1  0  1  1  1 |    0  0  0  1  0  0  1  1
//     1  1  1  1  0  0  0  1 | 1  0  1  1  1  1  0  0  |  1  1  0  1  1  1  1  1 |    1  0  0  1  1  0  1  1

// Step 2. Swap nibbles in each L byte.
L := ((L shl 4) and $F0F0F0F0) or ((L shr 4) and $0F0F0F0F);
//L = 29 30 31 32 25 26 27 28 | 13 14 15 16  9 10 11 12 | 21 22 23 24 17 18 19 20 |  5  6  7  8  1  2  3  4
//R = 57 58 59 60 61 62 63 64 | 41 42 43 44 45 46 47 48 | 49 50 51 52 53 54 55 56 | 33 34 35 36 37 38 39 40

// Step 3. Swap high nibbles between L & R
t32 := (L and $0F0F0F0F) or (R and $F0F0F0F0);
R   := (L and $F0F0F0F0) or (R and $0F0F0F0F);
L   := t32;
//L = 57 58 59 60 25 26 27 28 | 41 42 43 44  9 10 11 12 | 49 50 51 52 17 18 19 20 | 33 34 35 36  1  2  3  4
//R = 29 30 31 32 61 62 63 64 | 13 14 15 16 45 46 47 48 | 21 22 23 24 53 54 55 56 |  5  6  7  8 37 38 39 40

// Step 4. Swap bytes
t8 := LongRec( L).Bytes[1];
LongRec( L).Bytes[1] := LongRec( R).Bytes[0];
LongRec( R).Bytes[0] := t8;
t8 := LongRec( L).Bytes[3];
LongRec( L).Bytes[3] := LongRec( R).Bytes[2];
LongRec( R).Bytes[2] := t8;
//L = 13 14 15 16 45 46 47 48 | 41 42 43 44  9 10 11 12 |  5  6  7  8 37 38 39 40 | 33 34 35 36  1  2  3  4
//R = 29 30 31 32 61 62 63 64 | 57 58 59 60 25 26 27 28 | 21 22 23 24 53 54 55 56 | 49 50 51 52 17 18 19 20

// Step 5. 2-bit swap
//  L = a-->d  b
//  R = c      d-->a
T32 := ((L shr 2) xor R) and $33333333;
R   := R xor T32;
L   := L xor (T32 shl 2);
//L = 31 32 15 16 63 64 47 48 | 59 60 43 44 27 28 11 12 | 23 24  7  8 55 56 39 40 | 51 52 35 36 19 20  3  4
//R = 29 30 13 14 61 62 45 46 | 57 58 41 42 25 26  9 10 | 21 22  5  6 53 54 37 38 | 49 50 33 34 17 18  1  2

// Step 5. Word swap
t16 := LongRec( L).Lo;
LongRec( L).Lo := LongRec( R).Hi;
LongRec( R).Hi := t16;
//L = 31 32 15 16 63 64 47 48 | 59 60 43 44 27 28 11 12 | 29 30 13 14 61 62 45 46 | 57 58 41 42 25 26  9 10
//R = 23 24  7  8 55 56 39 40 | 51 52 35 36 19 20  3  4 | 21 22  5  6 53 54 37 38 | 49 50 33 34 17 18  1  2

// Step 6. 1-bit swap
//  L = a     b-->c
//  R = c-->b d
T32 := ((R shr 1) xor L) and $55555555;
L   := L xor T32;
R   := R xor (T32 shl 1);
//L = 31 23 15  7 63 55 47 39 | 59 51 43 35 27 19 11  3 | 29 21 13  5 61 53 45 37 | 57 49 41 33 25 17  9  1
//R = 32 24 16  8 64 56 48 40 | 60 52 44 36 28 20 12  4 | 30 22 14  6 62 54 46 38 | 58 50 42 34 26 18 10  2

// Step 6. byte swap
t8 := LongRec( R).Bytes[0];
LongRec( R).Bytes[0] := LongRec( L).Bytes[1];
LongRec( L).Bytes[1] := t8;
t8 := LongRec( R).Bytes[2];
LongRec( R).Bytes[2] := LongRec( L).Bytes[3];
LongRec( L).Bytes[3] := t8;
//L = 60 52 44 36 28 20 12  4 | 59 51 43 35 27 19 11  3 | 58 50 42 34 26 18 10  2 | 57 49 41 33 25 17  9  1
//R = 32 24 16  8 64 56 48 40 | 31 23 15  7 63 55 47 39 | 30 22 14  6 62 54 46 38 | 29 21 13  5 61 53 45 37

// Step 7. Clear R high byte.
LongRec( R).Bytes[3] := 0;
//L = 60 52 44 36 28 20 12  4 | 59 51 43 35 27 19 11  3 | 58 50 42 34 26 18 10  2 | 57 49 41 33 25 17  9  1
//R =  0  0  0  0  0  0  0  0 | 31 23 15  7 63 55 47 39 | 30 22 14  6 62 54 46 38 | 29 21 13  5 61 53 45 37

// Step 8. Swap nibbles: R[0].LoN <--> R[1].LoN .
t8 := LongRec( R).Bytes[0] and $0F;
LongRec( R).Bytes[0] := (LongRec( R).Bytes[0] and $F0) or (LongRec( R).Bytes[1] and $0F);
LongRec( R).Bytes[1] := (LongRec( R).Bytes[1] and $F0) or t8;
//L = 60 52 44 36 28 20 12  4 | 59 51 43 35 27 19 11  3 | 58 50 42 34 26 18 10  2 | 57 49 41 33 25 17  9  1
//R =  0 0   0  0  0  0  0  0 | 31 23 15  7 63 55 47 39 | 30 22 14  6 61 53 45 37 | 29 21 13  5 62 54 46 38

// Step 9. Swap nibbles: L[3].LoN <--> R[2].LoN . and
//                       R[2].HiN <--> R[0].HiN
t8 := LongRec( R).Bytes[2];
LongRec( R).Bytes[2] := (LongRec( R).Bytes[0] and $F0) or (LongRec( L).Bytes[3] and $0F);
LongRec( L).Bytes[3] := (LongRec( L).Bytes[3] and $F0) or (t8 and $0F);
LongRec( R).Bytes[0] := (t8 and $F0)                   or (LongRec( R).Bytes[0] and $0F);
//L = 60 52 44 36 63 55 47 39 | 59 51 43 35 27 19 11  3 | 58 50 42 34 26 18 10  2 | 57 49 41 33 25 17  9  1
//R =  0 0   0  0  0  0  0  0 | 29 21 13  5 28 20 12  4 | 30 22 14  6 61 53 45 37 | 31 23 15  7 62 54 46 38

// ... which is our desired final result!
//L' = 60 52 44 36 63 55 47 39 59 51 43 35 27 19 11  3 58 50 42 34 26 18 10  2 57 49 41 33 25 17  9  1
//R' =  0 0   0  0  0  0  0  0 29 21 13  5 28 20 12  4 30 22 14  6 61 53 45 37 31 23 15  7 62 54 46 38

// Marshal the return.
Int64Rec( result).Lo := L;
Int64Rec( result).Hi := R
end;






function PC_2( L, R: uint32): uint64;
// input L = 25 26 27 28  0  0  0  0 | 17 18 19 20 21 22 23 24 |  9 10 11 12 13 14 15 16 |  1  2  3  4  5  6  7  8  (Little-endien)
// input R = 53 54 55 56  0  0  0  0 | 45 46 47 48 49 50 51 52 | 37 28 39 40 41 42 43 44 | 29 30 31 32 33 34 35 36

//PC-2 output =
//14    17   11    24     1    5
// 3    28   15     6    21   10
//23    19   12     4    26    8
//16     7   27    20    13    2
//41    52   31    37    47   55
//30    40   51    45    33   48
//44    49   39    56    34   53
//46    42   50    36    29   32

// Unlike PC-1 in which we marshaled the result by packing the bits,
//  for this return, we want to marshal the result into 8 times 6 bits,
//  with the result in the low 6 bits of each byte.
// We desire result, such that:
// result.Bytes[0] =  0    0   14   17   11    24     1    5
// result.Bytes[1] =  0    0    3   28   15     6    21   10
// result.Bytes[2] =  0    0   23   19   12     4    26    8
// result.Bytes[3] =  0    0   16    7   27    20    13    2
// result.Bytes[4] =  0    0   41   52   31    37    47   55
// result.Bytes[5] =  0    0   30   40   51    45    33   48
// result.Bytes[6] =  0    0   44   49   39    56    34   53
// result.Bytes[7] =  0    0   46   42   50    36    29   32

// In other words we wish to go from LR to L'R' where:
// L  = 25 26 27 28  0  0  0  0 | 17 18 19 20 21 22 23 24 |  9 10 11 12 13 14 15 16 |  1  2  3  4  5  6  7  8
// R  = 53 54 55 56  0  0  0  0 | 45 46 47 48 49 50 51 52 | 37 28 39 40 41 42 43 44 | 29 30 31 32 33 34 35 36
//
// L' =  0  0 16  7 27 20 13  2 | 0   0 23 19 12  4 26  8 |  0  0  3 28 15  6 21 10 |  0  0 14 17 11 24  1  5
// R' =  0  0 46 42 50 36 29 32 | 0   0 44 49 39 56 34 53 |  0  0 30 40 51 45 33 48 |  0  0 41 52 31 37 47 55

// Ex. 1: $3201073F2F0B3006 = PC_2( $F05599E1, $E0F1CCAA);
// Ex. 2: $25273C36193B1A1E = PC_2( $F0AB32C3, $D0E39955);

// http://orlingrabbe.com/des.htm test data:
// C1D1 = 1110000 1100110 0101010 1011111 1010101 0110011 0011110 0011110 (big-endien compact 56 bits)
// L = 1110000 1100110 0101010 1011111 (big-endien compact 28 bits; 7 bit groups)
// L = 11100001 10011001 01010101 11110000 (big-endien 32 bits; 8 bit groups)
// L = 1111 0000 0101 0101 1001 1001 1110 0001 (little-endien 32 bits; 4 bit groups)
// L = $F05599E1; (Delphi)
// R = 1010101 0110011 0011110 0011110 (big-endien compact 28 bits; 7 bit groups)
// R = 10101010 11001100 11110001 11100000 (big-endien 32 bits; 8 bit groups)
// R = 1110 0000 | 1111 0001 | 1100 1100 | 1010 1010 (little-endien 32 bits; 4 bit groups)
// R = $E0F1CCAA (Delphi)
// K1 = 000110 110000 001011 101111 111111 000111 000001 110010 (big-endien compact 48 bits; 6 bit rows)
// K1 = 00000110 00110000 00001011 00101111 00111111 00000111 00000001 00110010 (big-endien 64 bits; 8 bit rows)
// K1 = 0011 0010 | 0000 0001 | 0000 0111 | 0011 1111 | 0010 1111 | 00001 011 | 0011 0000 | 0000 0110 (little-endien 64 bits; 4 bit rows)
// K1 = $3201073F2F0B3006; (Delphi)

var
  b0, b1, b2, b3, b4, b5, b6, b7: byte;
begin
// Sledge-hammer approach. This implementation is not as efficient as it
//  could be. The reader is invited to exlore more efficient solutions.
//  Please keep me advised.
b0 := LongRec( L).Bytes[0];  // $E1
b1 := LongRec( L).Bytes[1];  // $99
b2 := LongRec( L).Bytes[2];  // $55
b3 := LongRec( L).Bytes[3];  // $F0
b4 := LongRec( R).Bytes[0];  // $AA
b5 := LongRec( R).Bytes[1];  // $CC
b6 := LongRec( R).Bytes[2];  // $F1
b7 := LongRec( R).Bytes[3];  // $E0
// The following code was programmatically generated, deriving from the
//  published PC-2 table.
Int64Rec( result).Bytes[ 0] := ((b1 and $04) shl 3) or   // DONE
                               ((b2 and $80) shr 3) or
                               ((b1 and $20) shr 2) or
                               ((b2 and $01) shl 2) or
                               ((b0 and $80) shr 6) or
                               ((b0 and $08) shr 3);    // Expect = $06

Int64Rec( result).Bytes[ 1] :=  (b0 and $20)        or   // DONE
                                (b3 and $10)        or
                               ((b1 and $02) shl 2) or
                                (b0 and $04)        or
                               ((b2 and $08) shr 2) or
                               ((b1 and $40) shr 6);     // Expect = $30

Int64Rec( result).Bytes[ 2] := ((b2 and $02) shl 4) or   // DONE
                               ((b2 and $20) shr 1) or
                               ((b1 and $10) shr 1) or
                               ((b0 and $10) shr 2) or
                               ((b3 and $40) shr 5) or
                                (b0 and $01)       ;     // Expect = $0B

Int64Rec( result).Bytes[ 3] := ((b1 and $01) shl 5) or   // DONE
                               ((b0 and $02) shl 3) or
                               ((b3 and $20) shr 2) or
                               ((b2 and $10) shr 2) or
                               ((b1 and $08) shr 2) or
                               ((b0 and $40) shr 6);      // Expect = $2F

// 0  0 41 52 31 37 47 55
Int64Rec( result).Bytes[ 4] := ((b5 and $08) shl 2) or    // DONE
                               ((b6 and $01) shl 4) or    //
                               ((b4 and $20) shr 2) or    //
                               ((b5 and $80) shr 5) or    //
                               ((b6 and $20) shr 4) or    //
                               ((b7 and $20) shr 5);      // Expect = $3F   bit 55; e=1;g=0

// 0  0 30 40 51 45 33 48
Int64Rec( result).Bytes[ 5] := ((b4 and $40) shr 1) or   // DONE
                                (b5 and $10)        or   //
                               ((b6 and $02) shl 2) or   //
                               ((b6 and $80) shr 5) or   //
                               ((b4 and $08) shr 2) or   //
                               ((b6 and $10) shr 4);     // Expect = $07

// 0  0 44 49 39 56 34 53
Int64Rec( result).Bytes[ 6] := ((b5 and $01) shl 5) or    // DONE
                               ((b6 and $08) shl 1) or    //
                               ((b5 and $20) shr 2) or    //
                               ((b7 and $10) shr 2) or    //  e=1;g=0
                               ((b4 and $04) shr 1) or    //
                               ((b7 and $80) shr 7);      // Expect = $01

// 0  0 46 42 50 36 29 32
Int64Rec( result).Bytes[ 7] := ((b6 and $40) shr 1) or    // DONE
                               ((b5 and $04) shl 2) or
                               ((b6 and $04) shl 1) or
                               ((b4 and $01) shl 2) or
                               ((b4 and $80) shr 6) or
                               ((b4 and $10) shr 4);      // Expect = $32
end;



procedure Roll1( var Value: uint32);
// Does a big-endien roll left 1
//  going from C to C' thus:
// C  = 25 26 27 28  0  0  0  0 | 17 18 19 20 21 22 23 24 | 9 10 11 12 13 14 15 16 | 1  2  3  4  5  6  7  8
// C' = 26 27 28  1  0  0  0  0 | 18 19 20 21 22 23 24 25 |10 11 12 13 14 15 16 17 | 2  3  4  5  6  7  8  9
begin
Value := ((Value shl 1) and $EEFEFEFE) or
         ((Value and $80808000) shr 15) or
         ((Value and $00000080) shl 21)
end;



procedure ExpandKey( Key: uint64; var Ex: TExpandedKey);
const
  isTwoRolls: array[0..15] of boolean = (
    False, False, True, True, True , True , True, True,
    False, True , True, True, True , True , True, False);
var
  CD: uint64;
  C, D: uint32;
  i: integer;
begin
CD := PC_1( Key);
C :=   int64Rec( CD).Lo         and $F0FFFFFF ;    // Low 28 big-endien bits.
D := ((int64Rec( CD).Lo shr 20) and $000000F0) or  // Next 28 big-endien bits.
     ((int64Rec( CD).Hi shl 12) and $F0F0F000) or
     ((int64Rec( CD).Hi shr  4) and $0F0F0F0F);

for i := 0 to 15 do
  begin
  // Example test data from http://orlingrabbe.com/des.htm
  // C0 = 1111000011001100101010101111 (big-endien; 28 bits)
  // C0 = 11110000110011001010101011110000 (big-endien; 32 bits = 28 + 4 zeroes at the end)
  // C0 = 1111 0000 | 1100 1100 | 1010 1010 | 1111 0000 (big-endien; 32 bits = 28 + 4 zeroes at the end)
  // C0 = 1111 0000 | 1010 1010 | 1100 1100 | 1111 0000 (litte-endien 32 bits)
  // C0 =    F 0         A A         C C         F 0    (litte-endien hex)
  // C0 = $F0AACCF0;    (Delphi)
  // C1 = C0 [big-endien roll left] 1
  // C1 = 1110000110011001010101011111 (big-endien 28 bits)
  // C1 = 1110 0001 | 1001 1001 | 0101 0101 | 1111 0000 (big-endien 32 bits)
  // C1 = 1111 0000 | 0101 0101 | 1001 1001 | 1110 0001 (little-endien 32 bits)
  // C1 = $F05599E1; (Delphi)
  Roll1( C);
  // For test data case and i=0, check C = $F05599E1
  Roll1( D);
  if isTwoRolls[i] then
    begin
    Roll1( C);
    Roll1( D);
    end;
  Ex[i] := PC_2( C, D)
  // K1 = 000110 110000 001011 101111 111111 000111 000001 110010 (big-endien 48; 6-bit groups)
  // K1 = 110010 000001 000111 111111 101111 001011 110000 000110 (little-endien 48; 6-bit groups)
  // K1 = 00110010 00000001 00000111 00111111 00101111 00001011 00110000 00000110 (little-endien 64; 8-bit groups = 48 bits + 2MSB zeros)
  // K1 = $3201073F2F0B3006; (Delphi)
  // Ex[0] = $3201073F2F0B3006 = PC_2( $F05599E1, $E0F1CCAA);
  // Ex[1] = $25273C36193B1A1E = PC_2( $F0AB32C3, $D0E39955);
  end
end;



procedure IP_Transform( Datum: uint64; var L, R: uint32);
// See IP_InverseTransform for an explaination of how this works.
var
  T32: uint32;
  T16: word;
  T8: byte;
begin
L := Int64Rec( Datum).Lo;
R := Int64Rec( Datum).Hi;
T32 := ((L shr 4) xor R) and $0F0F0F0F;
R := R xor T32;
L := L xor (T32 shl 4);
T16 := LongRec(L).Lo;
LongRec(L).Lo := LongRec(R).Hi;
LongRec(R).Hi := T16;
T32 := ((R shr 2) xor L) and $33333333;
L := L xor T32;
R := R xor (T32 shl 2);
T8 := LongRec(R).Bytes[0];
LongRec(R).Bytes[0] := LongRec(L).Bytes[1];
LongRec(L).Bytes[1] := T8;
T8 := LongRec(R).Bytes[2];
LongRec(R).Bytes[2] := LongRec(L).Bytes[3];
LongRec(L).Bytes[3] := T8;
T32 := ((L shr 1) xor R) and $55555555;
R := R xor T32;
L := L xor (T32 shl 1)
{ http://orlingrabbe.com/des.htm gives an example in a binary
  lowest-address=byte-left, MSB-within-byte-left format:
M = 0000 0001 0010 0011 0100 0101 0110 0111 1000 1001 1010 1011 1100 1101 1110 1111
IP = 1100 1100 0000 0000 1100 1100 1111 1111 1111 0000 1010 1010 1111 0000 1010 1010

In our native format drawing format, this is equivalent to:
M = $EFCDAB8967452301;
L = $FFCC00CC;
R = $AAF0AAF0

}
end;



procedure IP_InverseTransform( L, R: uint32; var Datum: uint64);
var
  T32: uint32;
  T16: word;
  T8: byte;
{
Start with input 64 bits (MLB left)
58 50 42 34 26 18 10  2
60 52 44 36 28 20 12  4
62 54 46 38 30 22 14  6
64 56 48 40 32 24 16  8
57 49 41 33 25 17  9  1
59 51 43 35 27 19 11  3
61 53 45 37 29 21 13  5
63 55 47 39 31 23 15  7

Your may recognise this as the IP table on page 10 of the standard.
The above is drawn 1 byte per row, with the lowest address byte on the top
row and the highest address byte on the bottom row. In a little-endien
architecture, the first row is the Least Significant byte. In a big-endien
architecture, the first row is the Most Significant byte. In either case,
the first row is the lowest address byte.
Within a byte, the bits are drawn from MSB on the left to LSB on the right.
The numbers in the matrix are the DES designation for bits. The standard
designates the 'first' bit to be bit 1 (or B1). I interpret 'first' bit
to mean MSB.

We can split these 64 bits into two 32 two bit native (little-endien) integers.
L is first, followed by R.

So ....
L = 64 56 48 40 32 24 16  8 62 54 46 38 30 22 14  6 60 52 44 36 28 20 12  4 58 50 42 34 26 18 10  2
R = 63 55 47 39 31 23 15  7 61 53 45 37 29 21 13  5 59 51 43 35 27 19 11  3 57 49 41 33 25 17  9  1

The L, R is drawn from MSB on the left to LSB on the right.

NOTE:
The standard and most examples you can find are terribly endien-biggoted.
They implicitly assume a big-endien architecture and are often ambigious
in a little-endien context.

For example, refer to "http://orlingrabbe.com/des.htm", section "How DES Works in Detail".
In this example,
  M = the mathematical, unencoded number $123456789ABCDEF;
but DES does not operate on numbers, it transforms blocks of 8 bytes.
The article really means
  M = the 8 bytes (lowest address byte left-most): $01 $23 $45 $67 $89 $AB $CD $EF
On a little-endien machine, this is the encoded form of the number $EFCDAB8967351201.
  or M: uint64 = $EFCDAB8967351201;
The first byte is $01, and the first bit of the first byte is 0.
In a binary lowest-address=byte-left, MSB-within-byte-left format, this datum is drawn:
  M = 0000 0001 0010 0011 0100 0101 0110 0111 1000 1001 1010 1011 1100 1101 1110 1111
  L = 0000 0001 0010 0011 0100 0101 0110 0111
  R = 1000 1001 1010 1011 1100 1101 1110 1111

However, the same datum, drawn in a 'native' format (highest-address=byte-left, MSB-within-byte-left format) is drawn thus:
  M = 1110 1111 1100 1101 1010 1011 1000 1001 0110 0111 0100 0101 0010 0011 0000 0001
  L = 0110 0111 0100 0101 0010 0011 0000 0001
  R = 1110 1111 1100 1101 1010 1011 1000 1001

The word 'native' means in respect of how, in a little-endien machine,
the drawing of an encoded number harmonises with the 'shl' operator shifting
LEFT, and the shr operator shifting RIGHT.

Reading these endien biggoted source documents can get really confusing
until you acknowledge that there is more than one way to draw the same datum.

Our goal is to reach ...
  1  2  3  4  5  6  7  8
  9 10 11 12 13 14 15 16
 17 18 19 20 21 22 23 24
 25 26 27 28 29 30 31 32
 33 34 35 36 37 38 39 40
 41 42 43 44 45 46 47 48
 49 50 51 52 53 54 55 56
 57 58 59 60 61 62 63 64

or equivalently ....
L = 25 26 27 28 29 30 31 32 17 18 19 20 21 22 23 24  9 10 11 12 13 14 15 16  1  2  3  4  5  6  7  8
R = 57 58 59 60 61 62 63 64 49 50 51 52 53 54 55 56 41 42 43 44 45 46 47 48 33 34 35 36 37 38 39 40
}


begin
// Test case data:
//   L = $34324243
//   R = $95D94C0A  ==>
//   Datum = $05B40A0F5413E885

// convert ciphertext LR to plaintext M
// L = 64 56 48 40 32 24 16  8 62 54 46 38 30 22 14  6 60 52 44 36 28 20 12  4 58 50 42 34 26 18 10  2
// R = 63 55 47 39 31 23 15  7 61 53 45 37 29 21 13  5 59 51 43 35 27 19 11  3 57 49 41 33 25 17  9  1

// Step 1. Do a 1 bit cross-over.
//  L = a-->d  b
//  R = c      d-->a
T32 := ((L shr 1) xor R) and $55555555;
R   := R xor T32;
L   := L xor (T32 shl 1);

// L = 55 56 39 40 23 24  7  8 53 54 37 38 21 22  5  6 51 52 35 36 19 20  3  4 49 50 33 34 17 18  1  2
// R = 63 64 47 48 31 32 15 16 61 62 45 46 29 30 13 14 59 60 43 44 27 28 11 12 57 58 41 42 25 26  9 10

// Step 2. Do a byte cross-over.
//  L = a-->d  b
//  R = c      d-->a
T8                  := LongRec(R).Bytes[0];
LongRec(R).Bytes[0] := LongRec(L).Bytes[1];
LongRec(L).Bytes[1] := T8;
T8                  := LongRec(R).Bytes[2];
LongRec(R).Bytes[2] := LongRec(L).Bytes[3];
LongRec(L).Bytes[3] := T8;

// L = 61 62 45 46 29 30 13 14 | 53 54 37 38 21 22  5  6 | 57 58 41 42 25 26  9 10 | 49 50 33 34 17 18  1  2
// R = 63 64 47 48 31 32 15 16 | 55 56 39 40 23 24  7  8 | 59 60 43 44 27 28 11 12 | 51 52 35 36 19 20  3  4

// Step 3. Do a 2 bit cross-over.
//  L = a      b-->c
//  R = c-->b  d
T32 := ((R shr 2) xor L) and $33333333;
L   := L xor T32;
R   := R xor (T32 shl 2);

// L = 61 62 63 64 29 30 31 32 | 53 54 55 56 21 22 23 24 | 57 58 59 60 25 26 27 28 | 49 50 51 52 17 18 19 20
// R = 45 46 47 48 13 14 15 16 | 37 38 39 40  5  6  7  8 | 41 42 43 44  9 10 11 12 | 33 34 35 36  1  2  3  4

// Step 4.  Do a word cross-over.
//  L = a      b-->c
//  R = c-->b  d            Change here
T16           := LongRec(L).Lo;
LongRec(L).Lo := LongRec(R).Hi;
LongRec(R).Hi := T16;

// L = 61 62 63 64 29 30 31 32 | 53 54 55 56 21 22 23 24 | 45 46 47 48 13 14 15 16 | 37 38 39 40  5  6  7  8
// R = 57 58 59 60 25 26 27 28 | 49 50 51 52 17 18 19 20 | 41 42 43 44  9 10 11 12 | 33 34 35 36  1  2  3  4

// Step 5.  Do a nibble cross-over
//  L = a-->d  b
//  R = c      d-->a
T32 := ((L shr 4) xor R) and $0F0F0F0F;
R   := R xor T32;
L   := L xor (T32 shl 4);

// L = 25 26 27 28 29 30 31 32 | 17 18 19 20 21 22 23 24 |  9 10 11 12 13 14 15 16 |  1  2  3  4  5  6  7  8
// R = 57 58 59 60 61 62 63 64 | 49 50 51 52 53 54 55 56 | 41 42 43 44 45 46 47 48 | 33 34 35 36 37 38 39 40

Int64Rec( Datum).Lo := L;
Int64Rec( Datum).Hi := R
end;



function E_Bit_Selection( R: uint32): uint64;
var
  i, j, k: integer;
begin
// E-bit selection table is ...
//32     1    2     3     4    5
// 4     5    6     7     8    9
// 8     9   10    11    12   13
//12    13   14    15    16   17
//16    17   18    19    20   21
//20    21   22    23    24   25
//24    25   26    27    28   29
//28    29   30    31    32    1

// where R is ...
// R  = 25 26 27 28 29 30 31 32 | 17 18 19 20 21 22 23 24 |  9 10 11 12 13 14 15 16 |  1  2  3  4  5  6  7  8

// Example data from http://orlingrabbe.com/des.htm
// R0 = 1111 0000 1010 1010 1111 0000 1010 1010 (big-endien 32)
// R0 = $AAF0AAF0;
// E(R0) = 011110 100001 010101 010101 011110 100001 010101 010101 (big-endien 48)
// E(R0) = 010101 010101 100001 011110 010101 010101 100001 011110 (little-endien 48)
// E(R0) = 00010101 00010101 00100001 00011110 00010101 00010101 00100001 00011110 (little-endien 64 = 48 + 2MBS zeros per row)
// E(R0) = 00010101 00010101 00100001 00011110 00010101 00010101 00100001 00011110 (little-endien 64 = 48 + 2MBS zeros per row)
// E(R0) = $1515211E1515211E; (Delphi)
j := 1;
for i := 0 to 3 do
  begin
  k := i + 1;
  if k = 4 then
    k := 0;
  Int64Rec( result).Bytes[j] := ((LongRec( R).Bytes[i] shl 1) and $3F) xor (LongRec( R).Bytes[k] shr 7);
  Inc( j);
  if j = 8 then
    j := 0;
  Int64Rec( result).Bytes[j] := ((LongRec( R).Bytes[i] shl 5) and $3F) xor (LongRec( R).Bytes[k] shr 3);
  Inc( j)
  end;
end;



const SBox: array[ {Subkey:}0..7, {B-value:}0..63] of 0.. 15 = (
{Subkey 0} (
14, 0, 4, 15, 13, 7, 1, 4, 2, 14, 15, 2, 11, 13, 8, 1,
3, 10, 10, 6, 6, 12, 12, 11, 5, 9, 9, 5, 0, 3, 7, 8,
4, 15, 1, 12, 14, 8, 8, 2, 13, 4, 6, 9, 2, 1, 11, 7,
15, 5, 12, 11, 9, 3, 7, 14, 3, 10, 10, 0, 5, 6, 0, 13),

{Subkey 1} (
15, 3, 1, 13, 8, 4, 14, 7, 6, 15, 11, 2, 3, 8, 4, 14,
9, 12, 7, 0, 2, 1, 13, 10, 12, 6, 0, 9, 5, 11, 10, 5,
0, 13, 14, 8, 7, 10, 11, 1, 10, 3, 4, 15, 13, 4, 1, 2,
5, 11, 8, 6, 12, 7, 6, 12, 9, 0, 3, 5, 2, 14, 15, 9),

{Subkey 2} (
10, 13, 0, 7, 9, 0, 14, 9, 6, 3, 3, 4, 15, 6, 5, 10,
1, 2, 13, 8, 12, 5, 7, 14, 11, 12, 4, 11, 2, 15, 8, 1,
13, 1, 6, 10, 4, 13, 9, 0, 8, 6, 15, 9, 3, 8, 0, 7,
11, 4, 1, 15, 2, 14, 12, 3, 5, 11, 10, 5, 14, 2, 7, 12),

{Subkey 3} (
7, 13, 13, 8, 14, 11, 3, 5, 0, 6, 6, 15, 9, 0, 10, 3,
1, 4, 2, 7, 8, 2, 5, 12, 11, 1, 12, 10, 4, 14, 15, 9,
10, 3, 6, 15, 9, 0, 0, 6, 12, 10, 11, 1, 7, 13, 13, 8,
15, 9, 1, 4, 3, 5, 14, 11, 5, 12, 2, 7, 8, 2, 4, 14),

{Subkey 4} (
2, 14, 12, 11, 4, 2, 1, 12, 7, 4, 10, 7, 11, 13, 6, 1,
8, 5, 5, 0, 3, 15, 15, 10, 13, 3, 0, 9, 14, 8, 9, 6,
4, 11, 2, 8, 1, 12, 11, 7, 10, 1, 13, 14, 7, 2, 8, 13,
15, 6, 9, 15, 12, 0, 5, 9, 6, 10, 3, 4, 0, 5, 14, 3),

{Subkey 5} (
12, 10, 1, 15, 10, 4, 15, 2, 9, 7, 2, 12, 6, 9, 8, 5,
0, 6, 13, 1, 3, 13, 4, 14, 14, 0, 7, 11, 5, 3, 11, 8,
9, 4, 14, 3, 15, 2, 5, 12, 2, 9, 8, 5, 12, 15, 3, 10,
7, 11, 0, 14, 4, 1, 10, 7, 1, 6, 13, 0, 11, 8, 6, 13),

{Subkey 6} (
4, 13, 11, 0, 2, 11, 14, 7, 15, 4, 0, 9, 8, 1, 13, 10,
3, 14, 12, 3, 9, 5, 7, 12, 5, 2, 10, 15, 6, 8, 1, 6,
1, 6, 4, 11, 11, 13, 13, 8, 12, 1, 3, 4, 7, 10, 14, 7,
10, 9, 15, 5, 6, 0, 8, 15, 0, 14, 5, 2, 9, 3, 2, 12),

{Subkey 7} (
13, 1, 2, 15, 8, 13, 4, 8, 6, 10, 15, 3, 11, 7, 1, 4,
10, 12, 9, 5, 3, 6, 14, 11, 5, 0, 0, 14, 12, 9, 7, 2,
7, 2, 11, 1, 4, 14, 1, 7, 9, 4, 12, 10, 14, 8, 2, 13,
0, 15, 6, 12, 10, 9, 13, 0, 15, 3, 3, 5, 5, 6, 8, 11));


BytePermute_8_6_7_2_5_3_4_1: array[ byte ] of byte = (
  $00, $80, $20, $A0, $40, $C0, $60, $E0, $08, $88, $28, $A8, $48, $C8, $68, $E8,
  $02, $82, $22, $A2, $42, $C2, $62, $E2, $0A, $8A, $2A, $AA, $4A, $CA, $6A, $EA,
  $04, $84, $24, $A4, $44, $C4, $64, $E4, $0C, $8C, $2C, $AC, $4C, $CC, $6C, $EC,
  $06, $86, $26, $A6, $46, $C6, $66, $E6, $0E, $8E, $2E, $AE, $4E, $CE, $6E, $EE,
  $10, $90, $30, $B0, $50, $D0, $70, $F0, $18, $98, $38, $B8, $58, $D8, $78, $F8,
  $12, $92, $32, $B2, $52, $D2, $72, $F2, $1A, $9A, $3A, $BA, $5A, $DA, $7A, $FA,
  $14, $94, $34, $B4, $54, $D4, $74, $F4, $1C, $9C, $3C, $BC, $5C, $DC, $7C, $FC,
  $16, $96, $36, $B6, $56, $D6, $76, $F6, $1E, $9E, $3E, $BE, $5E, $DE, $7E, $FE,
  $01, $81, $21, $A1, $41, $C1, $61, $E1, $09, $89, $29, $A9, $49, $C9, $69, $E9,
  $03, $83, $23, $A3, $43, $C3, $63, $E3, $0B, $8B, $2B, $AB, $4B, $CB, $6B, $EB,
  $05, $85, $25, $A5, $45, $C5, $65, $E5, $0D, $8D, $2D, $AD, $4D, $CD, $6D, $ED,
  $07, $87, $27, $A7, $47, $C7, $67, $E7, $0F, $8F, $2F, $AF, $4F, $CF, $6F, $EF,
  $11, $91, $31, $B1, $51, $D1, $71, $F1, $19, $99, $39, $B9, $59, $D9, $79, $F9,
  $13, $93, $33, $B3, $53, $D3, $73, $F3, $1B, $9B, $3B, $BB, $5B, $DB, $7B, $FB,
  $15, $95, $35, $B5, $55, $D5, $75, $F5, $1D, $9D, $3D, $BD, $5D, $DD, $7D, $FD,
  $17, $97, $37, $B7, $57, $D7, $77, $F7, $1F, $9F, $3F, $BF, $5F, $DF, $7F, $FF);



BytePermute_3_8_4_5_6_2_7_1: array[ byte ] of byte = (
  $00, $40, $02, $42, $08, $48, $0A, $4A, $10, $50, $12, $52, $18, $58, $1A, $5A,
  $20, $60, $22, $62, $28, $68, $2A, $6A, $30, $70, $32, $72, $38, $78, $3A, $7A,
  $80, $C0, $82, $C2, $88, $C8, $8A, $CA, $90, $D0, $92, $D2, $98, $D8, $9A, $DA,
  $A0, $E0, $A2, $E2, $A8, $E8, $AA, $EA, $B0, $F0, $B2, $F2, $B8, $F8, $BA, $FA,
  $04, $44, $06, $46, $0C, $4C, $0E, $4E, $14, $54, $16, $56, $1C, $5C, $1E, $5E,
  $24, $64, $26, $66, $2C, $6C, $2E, $6E, $34, $74, $36, $76, $3C, $7C, $3E, $7E,
  $84, $C4, $86, $C6, $8C, $CC, $8E, $CE, $94, $D4, $96, $D6, $9C, $DC, $9E, $DE,
  $A4, $E4, $A6, $E6, $AC, $EC, $AE, $EE, $B4, $F4, $B6, $F6, $BC, $FC, $BE, $FE,
  $01, $41, $03, $43, $09, $49, $0B, $4B, $11, $51, $13, $53, $19, $59, $1B, $5B,
  $21, $61, $23, $63, $29, $69, $2B, $6B, $31, $71, $33, $73, $39, $79, $3B, $7B,
  $81, $C1, $83, $C3, $89, $C9, $8B, $CB, $91, $D1, $93, $D3, $99, $D9, $9B, $DB,
  $A1, $E1, $A3, $E3, $A9, $E9, $AB, $EB, $B1, $F1, $B3, $F3, $B9, $F9, $BB, $FB,
  $05, $45, $07, $47, $0D, $4D, $0F, $4F, $15, $55, $17, $57, $1D, $5D, $1F, $5F,
  $25, $65, $27, $67, $2D, $6D, $2F, $6F, $35, $75, $37, $77, $3D, $7D, $3F, $7F,
  $85, $C5, $87, $C7, $8D, $CD, $8F, $CF, $95, $D5, $97, $D7, $9D, $DD, $9F, $DF,
  $A5, $E5, $A7, $E7, $AD, $ED, $AF, $EF, $B5, $F5, $B7, $F7, $BD, $FD, $BF, $FF);



BytePermute_1_2_7_4_5_6_3_8: array[ byte ] of byte = (
  $00, $01, $20, $21, $04, $05, $24, $25, $08, $09, $28, $29, $0C, $0D, $2C, $2D,
  $10, $11, $30, $31, $14, $15, $34, $35, $18, $19, $38, $39, $1C, $1D, $3C, $3D,
  $02, $03, $22, $23, $06, $07, $26, $27, $0A, $0B, $2A, $2B, $0E, $0F, $2E, $2F,
  $12, $13, $32, $33, $16, $17, $36, $37, $1A, $1B, $3A, $3B, $1E, $1F, $3E, $3F,
  $40, $41, $60, $61, $44, $45, $64, $65, $48, $49, $68, $69, $4C, $4D, $6C, $6D,
  $50, $51, $70, $71, $54, $55, $74, $75, $58, $59, $78, $79, $5C, $5D, $7C, $7D,
  $42, $43, $62, $63, $46, $47, $66, $67, $4A, $4B, $6A, $6B, $4E, $4F, $6E, $6F,
  $52, $53, $72, $73, $56, $57, $76, $77, $5A, $5B, $7A, $7B, $5E, $5F, $7E, $7F,
  $80, $81, $A0, $A1, $84, $85, $A4, $A5, $88, $89, $A8, $A9, $8C, $8D, $AC, $AD,
  $90, $91, $B0, $B1, $94, $95, $B4, $B5, $98, $99, $B8, $B9, $9C, $9D, $BC, $BD,
  $82, $83, $A2, $A3, $86, $87, $A6, $A7, $8A, $8B, $AA, $AB, $8E, $8F, $AE, $AF,
  $92, $93, $B2, $B3, $96, $97, $B6, $B7, $9A, $9B, $BA, $BB, $9E, $9F, $BE, $BF,
  $C0, $C1, $E0, $E1, $C4, $C5, $E4, $E5, $C8, $C9, $E8, $E9, $CC, $CD, $EC, $ED,
  $D0, $D1, $F0, $F1, $D4, $D5, $F4, $F5, $D8, $D9, $F8, $F9, $DC, $DD, $FC, $FD,
  $C2, $C3, $E2, $E3, $C6, $C7, $E6, $E7, $CA, $CB, $EA, $EB, $CE, $CF, $EE, $EF,
  $D2, $D3, $F2, $F3, $D6, $D7, $F6, $F7, $DA, $DB, $FA, $FB, $DE, $DF, $FE, $FF);



BytePermute_1_7_8_6_2_5_3_4: array[ byte ] of byte = (
  $00, $20, $40, $60, $10, $30, $50, $70, $04, $24, $44, $64, $14, $34, $54, $74,
  $01, $21, $41, $61, $11, $31, $51, $71, $05, $25, $45, $65, $15, $35, $55, $75,
  $02, $22, $42, $62, $12, $32, $52, $72, $06, $26, $46, $66, $16, $36, $56, $76,
  $03, $23, $43, $63, $13, $33, $53, $73, $07, $27, $47, $67, $17, $37, $57, $77,
  $08, $28, $48, $68, $18, $38, $58, $78, $0C, $2C, $4C, $6C, $1C, $3C, $5C, $7C,
  $09, $29, $49, $69, $19, $39, $59, $79, $0D, $2D, $4D, $6D, $1D, $3D, $5D, $7D,
  $0A, $2A, $4A, $6A, $1A, $3A, $5A, $7A, $0E, $2E, $4E, $6E, $1E, $3E, $5E, $7E,
  $0B, $2B, $4B, $6B, $1B, $3B, $5B, $7B, $0F, $2F, $4F, $6F, $1F, $3F, $5F, $7F,
  $80, $A0, $C0, $E0, $90, $B0, $D0, $F0, $84, $A4, $C4, $E4, $94, $B4, $D4, $F4,
  $81, $A1, $C1, $E1, $91, $B1, $D1, $F1, $85, $A5, $C5, $E5, $95, $B5, $D5, $F5,
  $82, $A2, $C2, $E2, $92, $B2, $D2, $F2, $86, $A6, $C6, $E6, $96, $B6, $D6, $F6,
  $83, $A3, $C3, $E3, $93, $B3, $D3, $F3, $87, $A7, $C7, $E7, $97, $B7, $D7, $F7,
  $88, $A8, $C8, $E8, $98, $B8, $D8, $F8, $8C, $AC, $CC, $EC, $9C, $BC, $DC, $FC,
  $89, $A9, $C9, $E9, $99, $B9, $D9, $F9, $8D, $AD, $CD, $ED, $9D, $BD, $DD, $FD,
  $8A, $AA, $CA, $EA, $9A, $BA, $DA, $FA, $8E, $AE, $CE, $EE, $9E, $BE, $DE, $FE,
  $8B, $AB, $CB, $EB, $9B, $BB, $DB, $FB, $8F, $AF, $CF, $EF, $9F, $BF, $DF, $FF);



BytePermute_8_7_5_6_4_3_1_2: array[ byte ] of byte = (
  $00, $80, $40, $C0, $10, $90, $50, $D0, $20, $A0, $60, $E0, $30, $B0, $70, $F0,
  $08, $88, $48, $C8, $18, $98, $58, $D8, $28, $A8, $68, $E8, $38, $B8, $78, $F8,
  $04, $84, $44, $C4, $14, $94, $54, $D4, $24, $A4, $64, $E4, $34, $B4, $74, $F4,
  $0C, $8C, $4C, $CC, $1C, $9C, $5C, $DC, $2C, $AC, $6C, $EC, $3C, $BC, $7C, $FC,
  $01, $81, $41, $C1, $11, $91, $51, $D1, $21, $A1, $61, $E1, $31, $B1, $71, $F1,
  $09, $89, $49, $C9, $19, $99, $59, $D9, $29, $A9, $69, $E9, $39, $B9, $79, $F9,
  $05, $85, $45, $C5, $15, $95, $55, $D5, $25, $A5, $65, $E5, $35, $B5, $75, $F5,
  $0D, $8D, $4D, $CD, $1D, $9D, $5D, $DD, $2D, $AD, $6D, $ED, $3D, $BD, $7D, $FD,
  $02, $82, $42, $C2, $12, $92, $52, $D2, $22, $A2, $62, $E2, $32, $B2, $72, $F2,
  $0A, $8A, $4A, $CA, $1A, $9A, $5A, $DA, $2A, $AA, $6A, $EA, $3A, $BA, $7A, $FA,
  $06, $86, $46, $C6, $16, $96, $56, $D6, $26, $A6, $66, $E6, $36, $B6, $76, $F6,
  $0E, $8E, $4E, $CE, $1E, $9E, $5E, $DE, $2E, $AE, $6E, $EE, $3E, $BE, $7E, $FE,
  $03, $83, $43, $C3, $13, $93, $53, $D3, $23, $A3, $63, $E3, $33, $B3, $73, $F3,
  $0B, $8B, $4B, $CB, $1B, $9B, $5B, $DB, $2B, $AB, $6B, $EB, $3B, $BB, $7B, $FB,
  $07, $87, $47, $C7, $17, $97, $57, $D7, $27, $A7, $67, $E7, $37, $B7, $77, $F7,
  $0F, $8F, $4F, $CF, $1F, $9F, $5F, $DF, $2F, $AF, $6F, $EF, $3F, $BF, $7F, $FF);





function DES_Permute(
  SubKeyRound: integer; R: uint32; const Key: TExpandedKey): uint32;
// NOT WORKING YET !!!
var
  i, j: integer;
  T48, E: uint64;
  b: ^byte;
  b0, b1, b2, b3, t8: byte;
  b1b0, b3b2: word;
  t16: word;
begin
//  SubKeyRound = 0;
//  R= $AAF0AAF0;
//  Key = as per http://orlingrabbe.com/des.htm example.
// result should be = 0010 0011 0100 1010 1010 1001 1011 1011 (big-endien)
E := E_Bit_Selection( R);
T48 := E xor Key[ SubKeyRound];
// R0 = 1111 0000 1010 1010 1111 0000 1010 1010 (big-endien) = $AAF0AAF0;
// E(R0)        = 011110 100001 010101 010101 011110 100001 010101 010101 (big-endien)
//              = $1515211E1515211E;
// K1 = Key[0]  = 000110 110000 001011 101111 111111 000111 000001 110010 (big-endien)
//              = $3201073F2F0B3006;
// f = K1+E(R0) = 011000 010001 011110 111010 100001 100110 010100 100111 (big-endien)
//              = $271426213A1E1118;
b := @T48;
for j := 0 to 7 do
  begin
  // b = @Int64Rec( T48).Bytes[ j];
  b^ := SBox[ j, b^];
  Inc( b, 1)
  end;
// S1(B1)S2(B2)S3(B3)S4(B4)S5(B5)S6(B6)S7(B7)S8(B8)
//  = 0101 1100 1000 0010 1011 0101 1001 0111 (big-endien compact)
//  = $0709050B02080C05;
j := 0;
for i := 0 to 3 do
  begin
  LongRec( result).Bytes[i]  := (Int64Rec( T48).Bytes[j  ] shl 4) xor
                                 Int64Rec( T48).Bytes[j+1];
  Inc( j, 2)
  end;
//  = 0101 1100 1000 0010 1011 0101 1001 0111 (big-endien 32)
//  = $97B5825C;

// In the following fragment, we do the P-32 transform. This is ...
//  P-32 transform
//                         16   7  20  21
//                         29  12  28  17
//                          1  15  23  26
//                          5  18  31  10
//                          2   8  24  14
//                         32  27   3   9
//                         19  13  30   6
//                         22  11   4  25
// which should transform 0101 1100 1000 0010 1011 0101 1001 0111 (big-endien 32)  = $97B5825C
//                     to 0010 0011 0100 1010 1010 1001 1011 1011 (big-endien 32)  = $BBA94A23

// Equivalently in bytes ...
// 16   7  20  21  29  12  28  17
//  1  15  23  26   5  18  31  10
//  2   8  24  14  32  27   3   9
// 19  13  30   6  22  11   4  25

// Start with ..
b0 := LongRec( result).Bytes[0];  //  1  2  3  4  5  6  7  8
b1 := LongRec( result).Bytes[1];  //  9 10 11 12 13 14 15 16
b2 := LongRec( result).Bytes[2];  // 17 18 19 20 21 22 23 24
b3 := LongRec( result).Bytes[3];  // 25 26 27 28 29 30 31 32
// Case Study numbers.
// b0 =  92 (decimal) = $5C = [0 1 0 1 | 1 1 0 0] (binary; MSB..LSB)
// b1 = 130 (decimal) = $82 = [1 0 0 0 | 0 0 1 0]
// b2 = 181 (decimal) = $B5 = [1 0 1 1 | 0 1 0 1]
// b3 = 151 (decimal) = $97 = [1 0 0 1 | 0 1 1 1]

// [25 26 27 28 29 30 31 32] --> [32 30 31 26 29 27 28 25]
b3 := BytePermute_8_6_7_2_5_3_4_1[ b3];
// Case Study: b3 = [1 1 1 0 0 0 1 1] = $E3

// [17 18 19 20 21 22 23 24] --> [19 24 20 21 22 18 23 17]
b2 := BytePermute_3_8_4_5_6_2_7_1[ b2];
// Case Study: b2 = [1 1 1 0 1 0 0 1] = $E9

// b2   = 19  24  20  21  22  18  23  17
// b3   = 32  30  31  26  29  27  28  25
// Swap these bits, between b2 and b3:
// swaps: **              **  **  **

t8 := (b2 xor b3) and $8E;
b2 := b2 xor t8;
b3 := b3 xor t8;

// b2   = 32  24  20  21  29  27  28  17
// b3   = 19  30  31  26  22  18  23  25

// Case Study: b2 = [1  1  1  0  0  0  1  1] = $E3
// Case Study: b3 = [1  1  1  0  1  0  0  1] = $E9

// Swap these bits on b3:
//         vv              xx
// 19  30  31  26  22  18  23  25

// [19 30 31 26 22 18 23 25] --> [19 30 31 26 22 18 23 25]
b3 := BytePermute_1_2_7_4_5_6_3_8[ b3];
// Or equivalently, we could have put:
//  t8 := (b3 xor (b3 shr 4)) and $02;
//  b3 := b3 xor t8 xor (t8 shl 4);

// to get ...
// 19  30  23  26  22  18  31  25

// Case Study: b3 = [1 1 0 0 1 0 1 1] = $CB

// Back to the first two bytes.
// b0 = 1  2  3  4  5  6  7  8
// b1 = 9 10 11 12 13 14 15 16

// [1 2 3 4 5 6 7 8] --> [1 7 8 6 2 5 3 4]
b0 := BytePermute_1_7_8_6_2_5_3_4[ b0];
// Case Study: b0 = [0 0 0 1 1 1 0 1] = $1D

// [9 10 11 12 13 14 15 16] --> [16 15 13 14 12 11 9 10]
b1 := BytePermute_8_7_5_6_4_3_1_2[ b1];
// Case Study: b1 = [0 1 0 0 0 0 1 0] = $42

// b0 =  1   7   8   6   2    5   3   4
// b1 = 16  15  13  14   12  11   9  10

//Now swap these bits ...
// b0 =  1   7   8   6   2    5   3   4
//      **          **
// b1 = 16  15  13  14   12  11   9  10

t8 := (b0 xor b1) and $90;
b0 := b0 xor t8;
b1 := b1 xor t8;

//to get this
// b0 = 16   7   8  14   2    5   3   4
// b1 =  1  15  13   6   12  11   9  10

// Case Study: b0 = [0 0 0 0 1 1 0 1] = $0D
// Case Study: b1 = [0 1 0 1 0 0 1 0] = $52

// Now switch these bits ...
// b0 = 16   7   8  14   2   5   3   4
//                          ^^      ^^
//                       vv     vv
// b1 =  1  15  13   6   12 11   9  10

t8 := (b0 xor (b1 shr 1)) and $05;
b0 := b0 xor t8;
b1 := b1 xor (t8 shl 1);

//to get this ...
// b0 = 16   7   8  14   2  12   3   9
// b1 =  1  15  13   6   5  11   4  10

// Case Study: b0 = [0 0 0 0 1 0 0 1] = $09
// Case Study: b1 = [0 1 0 1 1 0 1 0] = $5A

// Now swap these bits ...
// b1|b0= 1  15  13   6   5  11   4  10  16   7   8  14   2  12   3   9   (little-endien)
//               **  **      **  **              **  **  **      **  **
// b3|b2=19  30  23  26  22  18  31  25  32  24  20  21  29  27  28  17

WordRec( b1b0).Lo := b0;
WordRec( b1b0).Hi := b1;
WordRec( b3b2).Lo := b2;
WordRec( b3b2).Hi := b3;
// Case Study: b1b0 = $5A09
// Case Study: b3b2 = $CBE3
t16 := (b1b0 xor b3b2) and $363B;
b1b0 := b1b0 xor t16;
b3b2 := b3b2 xor t16;
//to get this ...
// b1|b0= 1  15  23  26   5  18  31  10  16   7  20  21  29  12  28  17
// b3|b2=19  30  13   6  22  11   4  25  32  24   8  14   2  27   3   9

// Case Study: b1b0 = [0 1 0 0 | 1 0 1 0 | 0 0 1 0 | 0 0 1 1] = $4A23
// Case Study: b3b2 = [1 1 0 1 | 1 0 1 1 | 1 1 0 0 | 1 0 0 1] = $DBC9

// Now switch these bits ...
//                                       vv              xx
// b3|b2=19  30  13   6  22  11   4  25  32  24   8  14   2  27   3   9
t16  := (b3b2 xor (b3b2 shr 4)) and $0008;
b3b2 := b3b2 xor t16 xor (t16 shl 4);

//to get this ...
// b3|b2=19  30  13   6  22  11   4  25   2  24   8  14  32  27   3   9

// Case Study: b3b2 = [1 1 0 1 | 1 0 1 1 | 1 1 0 0 | 1 0 0 1] = $DBC9

// Now switch these bits ...
//           vv  xx                          vv  xx
// b3|b2=19  30  13   6  22  11   4  25   2  24   8  14  32  27   3   9
t16  := (b3b2 xor (b3b2 shr 1)) and $2020;
b3b2 := b3b2 xor t16 xor (t16 shl 1);

//to get this ...
// b3|b2=19  13  30   6  22  11   4  25   2   8  24  14  32  27   3   9

// Case Study numbers.
//  9 10 11 12 | 13 14 15 16 | 1  2  3  4 | 5  6  7  8 = [1 0 0 0 | 0 0 1 0 | 0 1 0 1 | 1 1 0 0]
// 25 26 27 28 | 29 30 31 32 |17 18 19 20 | 21 22 23 24 = [1 0 0 1 | 0 1 1 1 | 1 0 1 1 | 0 1 0 1]
// Case Study: b3b2 = [1 0 1 1 | 1 0 1 1 | 1 0 1 0 | 1 0 0 1] = $BBA9

// End result rendered as words..
// b1|b0= 1  15  23  26   5  18  31  10  16   7  20  21  29  12  28  17
// b3|b2=19  13  30   6  22  11   4  25   2   8  24  14  32  27   3   9

LongRec( result).Lo := b1b0;
LongRec( result).Hi := b3b2

// End result rendered as bytes..
// b0 = 16   7  20  21  29  12  28  17
// b1 =  1  15  23  26   5  18  31  10
// b2=   2   8  24  14  32  27   3   9
// b3=  19  13  30   6  22  11   4  25

// End result rendered as nibbles..
// 16   7  20  21
// 29  12  28  17
//  1  15  23  26
//  5  18  31  10
//  2   8  24  14
// 32  27   3   9
// 19  13  30   6
// 22  11   4  25

// which is the P-32 transform.
end;



procedure DES_EncryptBlock(
  Plaintext: uint64; var Ciphertext: uint64; const Key: TExpandedKey);
var
  L, R, T32: uint32;
  i: integer;
begin
IP_Transform( Plaintext, L, R);
// L0 = 1100 1100 0000 0000 1100 1100 1111 1111 (big-endien)
// R0 = 1111 0000 1010 1010 1111 0000 1010 1010 (big-endien)
// L = $FFCC00CC
// R = $AAF0AAF0
for i := 0 to 15 do
  begin
  T32 := DES_Permute( i, R, Key) xor L;
  // DES_Permute(i=0) = 0010 0011 0100 1010 1010 1001 1011 1011  (big-endien) = $BBA94A23
  // L(i=0)= $FFCC00CC
  // T32(i=0)= 1110 1111 0100 1010 0110 0101 0100 0100 (big-endien) = $44654AEF
  L   := R;
  R   := T32
  end;
// L16 = 0100 0011 0100 0010 0011 0010 0011 0100 (big-endien) = $34324243
// R16 = 0000 1010 0100 1100 1101 1001 1001 0101 (big-endien) = $95D94C0A
// Now Swap L & R and call the inverse IP transform.
IP_InverseTransform( R, L, Ciphertext)
// IP-1 = 10000101 11101000 00010011 01010100 00001111 00001010 10110100 00000101 (big-endien)
// This is the encrypted form of M = 0123456789ABCDEF (big-endien):
//   namely, C = 85E813540F0AB405 (big-endien)  OR
//   C = $05B40A0F5413E885 (delphi).
end;


procedure DES_DecryptBlock(
  Ciphertext: uint64; var Plaintext: uint64; const Key: TExpandedKey);
var
  L, R, T32: uint32;
  i: integer;
begin
IP_Transform( Ciphertext, R, L);  // L & R swapped.
for i := 15 downto 0 do
  begin
  T32 := R;
  R   := L;
  L   := DES_Permute( i, R, Key) xor T32
  end;
IP_InverseTransform( L, R, Plaintext)
end;


{ TDES }

function TDES.BlockSize: integer;
begin
result := 64
end;


constructor TDES.Create;
begin
end;


function TDES.DefinitionURL: string;
begin
result := 'http://csrc.nist.gov/publications/fips/fips46-3/fips46-3.pdf'
end;


function TDES.DisplayName: string;
begin
result := 'DES'
end;


function TDES.Features: TAlgorithmicFeatureSet;
begin
result := [
  afCryptographicallyWeak,
  afOpenSourceSoftware]
end;


function TDES.GenerateKey( Seed: TStream): TSymetricKey;
var
  SeedKey: uint64;
begin
Seed.ReadBuffer( SeedKey, 8);
result := TDESKey.Create( SeedKey)
end;


function TDES.KeySize: integer;
begin
result := 56
end;


function TDES.LoadKeyFromStream( Store: TStream): TSymetricKey;
begin
result := TDESKey.CreateFromStream( Store)
end;

function TDES.MakeBlockCodec( Key: TSymetricKey): IBlockCodec;
begin
result := TDESCodec.Create( Key as TDESKey)
end;



function TDES.ProgId: string;
begin
result := DES_ProgId
end;

function TDES.SeedByteSize: integer;
begin
result := 8
end;



function TDES.SelfTest_Plaintext: TBytes;
{ This from "FIPS PUB 81 - DES MODES OF OPERATION"
(http://www.itl.nist.gov/fipspubs/fip81.htm)
TABLE B1: AN EXAMPLE OF THE ELECTRONIC CODEBOOK (ECB) MODE

The ECB mode in the encrypt state has been selected.
Cryptographic Key = 0123456789abcdef

The plain text is the ASCII code for "Now is the time for all ." These seven-bit characters are written in hexadecimal notation
(0,b7,b6,...,b1).
  TIME PLAINTEXT       | DES INPUT BLOCK  | DES OUTPUT BLOCK |   CIPHERTEXT
   1. 4e6f772069732074 | 4e6f772069732074 | 3fa40e8a984d4315 | 3fa40e8a984d4815
   2. 68652074696d652o | 68652074696d652o | 6a271787ab8883f9 | 6a271787ab8883f9
   3. 666f7220616c6c20 | 666f7220616c6c20 | 893d51ec4b563b53 | 893d51ec4b563b53

The ECB mode in the decrypt state has been selected.
  TIME PLAINTEXT       | DES INPUT BLOCK  | DES OUTPUT BLOCK |   CIPHERTEXT
   1. 3fa40e8a984d4815 | 3fa40e8a984d4815 | 4e6f772o69732o74 | 4e6f772069732074
   2. 6a271787ab8883f9 | 6a271787ab8883f9 | 68652074696d6520 | 68652074696d6520
   3. 893d51ec4b563b53 | 893d51ec4b563b53 | 666f7220616c6c20 | 666f7220616c6c20}
begin
  result := AnsiBytesOf('4e6f772069732074');
end;


function TDES.SelfTest_Ciphertext: TBytes;
begin
result := AnsiBytesOf('3fa40e8a984d4815');
end;

function TDES.SelfTest_Key: TBytes;
begin
result := AnsiBytesOf('0123456789abcdef');
end;

function TDES.WikipediaReference: string;
begin
result := 'Data_Encryption_Standard'
end;



procedure SetParityBitsOnKey( var K: uint64);
var
  ByteIndex, BitIdx: integer;
  Byte1: byte;
  Parity: byte;
begin
for ByteIndex := 0 to 7 do
  begin
  Byte1 := Int64Rec( K).Bytes[ ByteIndex];
  Parity  := $00;
  for BitIdx := 7 downto 1 do
    Parity  := Parity xor ((Byte1 shr BitIdx) and $01);
  Int64Rec( K).Bytes[ ByteIndex] := (Byte1 and $FE) or Parity
  end
end;


function hasCorrectParity( K: uint64): boolean;
var
  ByteIndex, BitIdx: integer;
  Byte1: byte;
  Parity: byte;
begin
for ByteIndex := 0 to 7 do
  begin
  Byte1 := Int64Rec( K).Bytes[ ByteIndex];
  Parity  := $00;
  for BitIdx := 7 downto 1 do
    Parity  := Parity xor ((Byte1 shr BitIdx) and $01);
  result := (Byte1 and $01) = Parity
  end
end;





{ TDESKey }

constructor TDESKey.Create( NativeKey1: uint64);
begin
FNativeKey := NativeKey1;
SetParityBitsOnKey( FNativeKey);
ExpandKey( FNativeKey, FExpandedKey)
end;


constructor TDESKey.CreateFromStream( Store: TStream);
begin
Store.ReadBuffer( FNativeKey, 8);
if not hasCorrectParity( FNativeKey) then
  raise Exception.Create( ES_InvalidKey);
ExpandKey( FNativeKey, FExpandedKey)
end;



procedure TDESKey.SaveToStream(Stream: TStream);
begin
Stream.Write( FNativeKey, 8)
end;


procedure TDESKey.Burn;
begin
FNativeKey := 0;
FillChar( FExpandedKey, SizeOf( FExpandedKey), 0)
end;


{ TDESCodec }

constructor TDESCodec.Create( LockBoxKey1: TDESKey);
begin
FLockBoxKey := LockBoxKey1
end;

procedure TDESCodec.Encrypt_Block( Plaintext, Ciphertext: TMemoryStream);
var
  PlaintextBlock, CiphertextBlock: uint64;
begin
Move( Plaintext.Memory^, PlaintextBlock, 8);
DES_EncryptBlock( PlaintextBlock, CiphertextBlock, FLockBoxKey.FExpandedKey);
Move( CiphertextBlock, Ciphertext.Memory^, 8)
end;


procedure TDESCodec.Decrypt_Block( Plaintext, Ciphertext: TMemoryStream);
var
  PlaintextBlock, CiphertextBlock: uint64;
begin
Move( Ciphertext.Memory^, CiphertextBlock, 8);
DES_DecryptBlock( CiphertextBlock, PlaintextBlock, FLockBoxKey.FExpandedKey);
Move( PlaintextBlock, Plaintext.Memory^, 8)
end;


procedure TDESCodec.Reset;
begin
end;


procedure TDESCodec.Burn;
begin
end;


end.
