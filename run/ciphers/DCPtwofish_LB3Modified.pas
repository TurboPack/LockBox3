{******************************************************************************}
{* DCPcrypt v2.0 written by David Barton (crypto@cityinthesky.co.uk) **********}
{******************************************************************************}
{* A binary compatible implementation of Twofish ******************************}
{******************************************************************************}
{* Copyright (c) 1999-2002 David Barton                                       *}
{* Permission is hereby granted, free of charge, to any person obtaining a    *}
{* copy of this software and associated documentation files (the "Software"), *}
{* to deal in the Software without restriction, including without limitation  *}
{* the rights to use, copy, modify, merge, publish, distribute, sublicense,   *}
{* and/or sell copies of the Software, and to permit persons to whom the      *}
{* Software is furnished to do so, subject to the following conditions:       *}
{*                                                                            *}
{* The above copyright notice and this permission notice shall be included in *}
{* all copies or substantial portions of the Software.                        *}
{*                                                                            *}
{* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *}
{* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *}
{* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *}
{* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *}
{* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *}
{* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *}
{* DEALINGS IN THE SOFTWARE.                                                  *}
{******************************************************************************}
unit DCPtwofish_LB3Modified;
// The original unit name was DCPtwofish .
//  This unit is not part of the LockBox Project, but is used by it.
//  It is a modified version of Dave Barton's DCPtwofish unit of
//  the DCPCrypt library. It has been cut down to just the primitives.

interface

uses
  Types, Classes, SysUtils{$IFDEF MSWINDOWS}, Windows{$ENDIF};

const
  INPUTWHITEN= 0;
  OUTPUTWHITEN= 4;
  NUMROUNDS= 16;
  ROUNDSUBKEYS= (OUTPUTWHITEN + 4);
  TOTALSUBKEYS= (ROUNDSUBKEYS + NUMROUNDS * 2);
  RS_GF_FDBK= $14d;
  MDS_GF_FDBK= $169;
  SK_STEP= $02020202;
  SK_BUMP= $01010101;
  SK_ROTL= 9;


type

TSubKeys = array[ 0.. TOTALSUBKEYS - 1] of DWord;
TSBox    = array[ 0..3, 0..255] of DWord;
T128     = packed array[ 0..3 ] of DWord;
T256     = packed array[ 0..7 ] of DWord;
T2048    = packed array[ 0..255 ] of byte;
Tp8x8    = packed array[ 0..1 ] of T2048;

procedure DCP_towfish_Precomp;

procedure DCP_twofish_InitKey(
  const Key; Size: longword;
  var SubKeys: TSubKeys; var SBox: TSBox);

procedure DCP_twofish_EncryptECB(
  const SubKeys: TSubKeys; const SBox: TSBox;
  const InData: T128; var OutData: T128);

procedure DCP_twofish_DecryptECB(
  const SubKeys: TSubKeys; const SBox: TSBox;
  const InData: T128; var OutData: T128);


implementation

{$OVERFLOWCHECKS OFF}
{$RANGECHECKS OFF}

var
  MDS: TSBox;

function LFSR1( x: DWord): DWord;
begin
  if (x and 1)<> 0 then
    Result:= (x shr 1) xor (MDS_GF_FDBK div 2)
  else
    Result:= (x shr 1);
end;

function LFSR2(x: DWord): DWord;
begin
  if (x and 2)<> 0 then
    if (x and 1)<> 0 then
      Result:= (x shr 2) xor (MDS_GF_FDBK div 2) xor (MDS_GF_FDBK div 4)
    else
      Result:= (x shr 2) xor (MDS_GF_FDBK div 2)
  else
    if (x and 1)<> 0 then
      Result:= (x shr 2) xor (MDS_GF_FDBK div 4)
    else
      Result:= (x shr 2);
end;
function Mul_X(x: DWord): DWord;
begin
  Result:= x xor LFSR2(x);
end;
function Mul_Y(x: DWord): DWord;
begin
  Result:= x xor LFSR1(x) xor LFSR2(x);
end;

function RS_MDS_Encode(lK0, lK1: DWord): DWord;
var
  lR, nJ, lG2, lG3: DWord;
  bB: byte;
begin
  lR:= lK1;
  for nJ:= 0 to 3 do
  begin
    bB:= lR shr 24;
    if (bB and $80)<> 0 then
      lG2:= ((bB shl 1) xor RS_GF_FDBK) and $FF
    else
      lG2:= (bB shl 1) and $FF;
    if (bB and 1)<> 0 then
      lG3:= ((bB shr 1) and $7f) xor (RS_GF_FDBK shr 1) xor lG2
    else
      lG3:= ((bB shr 1) and $7f) xor lG2;
    lR:= (lR shl 8) xor (lG3 shl 24) xor (lG2 shl 16) xor (lG3 shl 8) xor bB;
  end;
  lR:= lR xor lK0;
  for nJ:= 0 to 3 do
  begin
    bB:= lR shr 24;
    if (bB and $80)<> 0 then
      lG2:= ((bB shl 1) xor RS_GF_FDBK) and $FF
    else
      lG2:= (bB shl 1) and $FF;
    if (bB and 1)<> 0 then
      lG3:= ((bB shr 1) and $7f) xor (RS_GF_FDBK shr 1) xor lG2
    else
      lG3:= ((bB shr 1) and $7f) xor lG2;
    lR:= (lR shl 8) xor (lG3 shl 24) xor (lG2 shl 16) xor (lG3 shl 8) xor bB;
  end;
  Result:= lR;
end;


const
  p8x8: Tp8x8= ((
    $a9, $67, $b3, $e8, $04, $fd, $a3, $76,
    $9a, $92, $80, $78, $e4, $dd, $d1, $38,
    $0d, $c6, $35, $98, $18, $f7, $ec, $6c,
    $43, $75, $37, $26, $fa, $13, $94, $48,
    $f2, $d0, $8b, $30, $84, $54, $df, $23,
    $19, $5b, $3d, $59, $f3, $ae, $a2, $82,
    $63, $01, $83, $2e, $d9, $51, $9b, $7c,
    $a6, $eb, $a5, $be, $16, $0c, $e3, $61,
    $c0, $8c, $3a, $f5, $73, $2c, $25, $0b,
    $bb, $4e, $89, $6b, $53, $6a, $b4, $f1,
    $e1, $e6, $bd, $45, $e2, $f4, $b6, $66,
    $cc, $95, $03, $56, $d4, $1c, $1e, $d7,
    $fb, $c3, $8e, $b5, $e9, $cf, $bf, $ba,
    $ea, $77, $39, $af, $33, $c9, $62, $71,
    $81, $79, $09, $ad, $24, $cd, $f9, $d8,
    $e5, $c5, $b9, $4d, $44, $08, $86, $e7,
    $a1, $1d, $aa, $ed, $06, $70, $b2, $d2,
    $41, $7b, $a0, $11, $31, $c2, $27, $90,
    $20, $f6, $60, $ff, $96, $5c, $b1, $ab,
    $9e, $9c, $52, $1b, $5f, $93, $0a, $ef,
    $91, $85, $49, $ee, $2d, $4f, $8f, $3b,
    $47, $87, $6d, $46, $d6, $3e, $69, $64,
    $2a, $ce, $cb, $2f, $fc, $97, $05, $7a,
    $ac, $7f, $d5, $1a, $4b, $0e, $a7, $5a,
    $28, $14, $3f, $29, $88, $3c, $4c, $02,
    $b8, $da, $b0, $17, $55, $1f, $8a, $7d,
    $57, $c7, $8d, $74, $b7, $c4, $9f, $72,
    $7e, $15, $22, $12, $58, $07, $99, $34,
    $6e, $50, $de, $68, $65, $bc, $db, $f8,
    $c8, $a8, $2b, $40, $dc, $fe, $32, $a4,
    $ca, $10, $21, $f0, $d3, $5d, $0f, $00,
    $6f, $9d, $36, $42, $4a, $5e, $c1, $e0),(
    $75, $f3, $c6, $f4, $db, $7b, $fb, $c8,
    $4a, $d3, $e6, $6b, $45, $7d, $e8, $4b,
    $d6, $32, $d8, $fd, $37, $71, $f1, $e1,
    $30, $0f, $f8, $1b, $87, $fa, $06, $3f,
    $5e, $ba, $ae, $5b, $8a, $00, $bc, $9d,
    $6d, $c1, $b1, $0e, $80, $5d, $d2, $d5,
    $a0, $84, $07, $14, $b5, $90, $2c, $a3,
    $b2, $73, $4c, $54, $92, $74, $36, $51,
    $38, $b0, $bd, $5a, $fc, $60, $62, $96,
    $6c, $42, $f7, $10, $7c, $28, $27, $8c,
    $13, $95, $9c, $c7, $24, $46, $3b, $70,
    $ca, $e3, $85, $cb, $11, $d0, $93, $b8,
    $a6, $83, $20, $ff, $9f, $77, $c3, $cc,
    $03, $6f, $08, $bf, $40, $e7, $2b, $e2,
    $79, $0c, $aa, $82, $41, $3a, $ea, $b9,
    $e4, $9a, $a4, $97, $7e, $da, $7a, $17,
    $66, $94, $a1, $1d, $3d, $f0, $de, $b3,
    $0b, $72, $a7, $1c, $ef, $d1, $53, $3e,
    $8f, $33, $26, $5f, $ec, $76, $2a, $49,
    $81, $88, $ee, $21, $c4, $1a, $eb, $d9,
    $c5, $39, $99, $cd, $ad, $31, $8b, $01,
    $18, $23, $dd, $1f, $4e, $2d, $f9, $48,
    $4f, $f2, $65, $8e, $78, $5c, $58, $19,
    $8d, $e5, $98, $57, $67, $7f, $05, $64,
    $af, $63, $b6, $fe, $f5, $b7, $3c, $a5,
    $ce, $e9, $68, $44, $e0, $4d, $43, $69,
    $29, $2e, $ac, $15, $59, $a8, $0a, $9e,
    $6e, $47, $df, $34, $35, $6a, $cf, $dc,
    $22, $c9, $c0, $9b, $89, $d4, $ed, $ab,
    $12, $a2, $0d, $52, $bb, $02, $2f, $a9,
    $d7, $61, $1e, $b4, $50, $04, $f6, $c2,
    $16, $25, $86, $56, $55, $09, $be, $91));

function f32( x: DWord; const K32: T128; Len: DWord): DWord;
var
  t0, t1, t2, t3: DWord;
begin
  t0:= x and $FF;
  t1:= (x shr 8) and $FF;
  t2:= (x shr 16) and $FF;
  t3:= x shr 24;
  if Len= 256 then
  begin
    t0:= p8x8[1,t0] xor ((K32[3]) and $FF);
    t1:= p8x8[0,t1] xor ((K32[3] shr  8) and $FF);
    t2:= p8x8[0,t2] xor ((K32[3] shr 16) and $FF);
    t3:= p8x8[1,t3] xor ((K32[3] shr 24));
  end;
  if Len>= 192 then
  begin
    t0:= p8x8[1,t0] xor ((K32[2]) and $FF);
    t1:= p8x8[1,t1] xor ((K32[2] shr  8) and $FF);
    t2:= p8x8[0,t2] xor ((K32[2] shr 16) and $FF);
    t3:= p8x8[0,t3] xor ((K32[2] shr 24));
  end;
  Result:= MDS[0,p8x8[0,p8x8[0,t0] xor ((K32[1]) and $FF)] xor ((K32[0]) and $FF)] xor
           MDS[1,p8x8[0,p8x8[1,t1] xor ((K32[1] shr  8) and $FF)] xor ((K32[0] shr  8) and $FF)] xor
           MDS[2,p8x8[1,p8x8[0,t2] xor ((K32[1] shr 16) and $FF)] xor ((K32[0] shr 16) and $FF)] xor
           MDS[3,p8x8[1,p8x8[1,t3] xor ((K32[1] shr 24))] xor ((K32[0] shr 24))];
end;


procedure Xor256( var Dst: T2048; const Src: T2048; v: byte);
var
  i, j: DWord;
  PDst, PSrc: PDWord;
begin
j := v * $01010101;
PDst := @Dst;
PSrc := @Src;
for i := 0 to 63 do
  begin
  PDst^ := PSrc^ xor j;
  Inc( PSrc);
  Inc( PDst)
  end
end;

procedure DCP_twofish_InitKey(
  const Key; Size: longword;
  var SubKeys: TSubKeys; var SBox: TSBox);
const
  subkeyCnt= ROUNDSUBKEYS + 2*NUMROUNDS;
var
  key32: T256;
  k32e, k32o, sboxkeys: T128;
  k64Cnt, i, j, A, B, q: DWord;
  L0, L1: T2048;
begin
  FillChar(Key32,Sizeof(Key32),0);
  Move( Key, Key32, Size div 8);
  if Size<= 128 then           { pad the key to either 128bit, 192bit or 256bit}
    Size:= 128
  else if Size<= 192 then
    Size:= 192
  else
    Size:= 256;
  k64Cnt:= Size div 64;
  j:= k64Cnt-1;
  for i:= 0 to j do
  begin
    k32e[i]:= key32[2*i];
    k32o[i]:= key32[2*i+1];
    sboxKeys[j]:= RS_MDS_Encode(k32e[i],k32o[i]);
    Dec(j);
  end;
  q:= 0;
  for i:= 0 to ((subkeyCnt div 2)-1) do
  begin
    A:= f32(q,k32e,Size);
    B:= f32(q+SK_BUMP,k32o,Size);
    B:= (B shl 8) or (B shr 24);
    SubKeys[2*i]:= A+B;
    B:= A + 2*B;
    SubKeys[2*i+1]:= (B shl SK_ROTL) or (B shr (32 - SK_ROTL));
    Inc(q,SK_STEP);
  end;
  case Size of
    128: begin
           Xor256(L0,p8x8[0],(sboxKeys[1] and $FF));
           A:= (sboxKeys[0] and $FF);
           i:= 0;
           while i< 256 do
           begin
             sBox[0 and 2,2*i+(0 and 1)]:= MDS[0,p8x8[0,L0[i]] xor A];
             sBox[0 and 2,2*i+(0 and 1)+2]:= MDS[0,p8x8[0,L0[i+1]] xor A];
             Inc(i,2);
           end;
           Xor256(L0,p8x8[1],(sboxKeys[1] shr 8) and $FF);
           A:= (sboxKeys[0] shr 8) and $FF;
           i:= 0;
           while i< 256 do
           begin
             sBox[1 and 2,2*i+(1 and 1)]:= MDS[1,p8x8[0,L0[i]] xor A];
             sBox[1 and 2,2*i+(1 and 1)+2]:= MDS[1,p8x8[0,L0[i+1]] xor A];
             Inc(i,2);
           end;
           Xor256(L0,p8x8[0],(sboxKeys[1] shr 16) and $FF);
           A:= (sboxKeys[0] shr 16) and $FF;
           i:= 0;
           while i< 256 do
           begin
             sBox[2 and 2,2*i+(2 and 1)]:= MDS[2,p8x8[1,L0[i]] xor A];
             sBox[2 and 2,2*i+(2 and 1)+2]:= MDS[2,p8x8[1,L0[i+1]] xor A];
             Inc(i,2);
           end;
           Xor256(L0,p8x8[1],(sboxKeys[1] shr 24));
           A:= (sboxKeys[0] shr 24);
           i:= 0;
           while i< 256 do
           begin
             sBox[3 and 2,2*i+(3 and 1)]:= MDS[3,p8x8[1,L0[i]] xor A];
             sBox[3 and 2,2*i+(3 and 1)+2]:= MDS[3,p8x8[1,L0[i+1]] xor A];
             Inc(i,2);
           end;
         end;
    192: begin
           Xor256(L0,p8x8[1],sboxKeys[2] and $FF);
           A:= sboxKeys[0] and $FF;
           B:= sboxKeys[1] and $FF;
           i:= 0;
           while i< 256 do
           begin
             sBox[0 and 2,2*i+(0 and 1)]:= MDS[0,p8x8[0,p8x8[0,L0[i]] xor B] xor A];
             sBox[0 and 2,2*i+(0 and 1)+2]:= MDS[0,p8x8[0,p8x8[0,L0[i+1]] xor B] xor A];
             Inc(i,2);
           end;
           Xor256(L0,p8x8[1],(sboxKeys[2] shr 8) and $FF);
           A:= (sboxKeys[0] shr 8) and $FF;
           B:= (sboxKeys[1] shr 8) and $FF;
           i:= 0;
           while i< 256 do
           begin
             sBox[1 and 2,2*i+(1 and 1)]:= MDS[1,p8x8[0,p8x8[1,L0[i]] xor B] xor A];
             sBox[1 and 2,2*i+(1 and 1)+2]:= MDS[1,p8x8[0,p8x8[1,L0[i+1]] xor B] xor A];
             Inc(i,2);
           end;
           Xor256(L0,p8x8[0],(sboxKeys[2] shr 16) and $FF);
           A:= (sboxKeys[0] shr 16) and $FF;
           B:= (sboxKeys[1] shr 16) and $FF;
           i:= 0;
           while i< 256 do
           begin
             sBox[2 and 2,2*i+(2 and 1)]:= MDS[2,p8x8[1,p8x8[0,L0[i]] xor B] xor A];
             sBox[2 and 2,2*i+(2 and 1)+2]:= MDS[2,p8x8[1,p8x8[0,L0[i+1]] xor B] xor A];
             Inc(i,2);
           end;
           Xor256(L0,p8x8[0],(sboxKeys[2] shr 24));
           A:= (sboxKeys[0] shr 24);
           B:= (sboxKeys[1] shr 24);
           i:= 0;
           while i< 256 do
           begin
             sBox[3 and 2,2*i+(3 and 1)]:= MDS[3,p8x8[1,p8x8[1,L0[i]] xor B] xor A];
             sBox[3 and 2,2*i+(3 and 1)+2]:= MDS[3,p8x8[1,p8x8[1,L0[i+1]] xor B] xor A];
             Inc(i,2);
           end;
         end;
    256: begin
           Xor256(L1,p8x8[1],(sboxKeys[3]) and $FF);
           i:= 0;
           while i< 256 do
           begin
             L0[i  ]:= p8x8[1,L1[i]];
             L0[i+1]:= p8x8[1,L1[i+1]];
             Inc(i,2);
           end;
           Xor256(L0,L0,(sboxKeys[2]) and $FF);
           A:= (sboxKeys[0]) and $FF;
           B:= (sboxKeys[1]) and $FF;
           i:= 0;
           while i< 256 do
           begin
             sBox[0 and 2,2*i+(0 and 1)]:= MDS[0,p8x8[0,p8x8[0,L0[i]] xor B] xor A];
             sBox[0 and 2,2*i+(0 and 1)+2]:= MDS[0,p8x8[0,p8x8[0,L0[i+1]] xor B] xor A];
             Inc(i,2);
           end;
           Xor256(L1,p8x8[0],(sboxKeys[3] shr  8) and $FF);
           i:= 0;
           while i< 256 do
           begin
             L0[i  ]:= p8x8[1,L1[i]];
             L0[i+1]:= p8x8[1,L1[i+1]];
             Inc(i,2);
           end;
           Xor256(L0,L0,(sboxKeys[2] shr  8) and $FF);
           A:= (sboxKeys[0] shr  8) and $FF;
           B:= (sboxKeys[1] shr  8) and $FF;
           i:= 0;
           while i< 256 do
           begin
             sBox[1 and 2,2*i+(1 and 1)]:= MDS[1,p8x8[0,p8x8[1,L0[i]] xor B] xor A];
             sBox[1 and 2,2*i+(1 and 1)+2]:= MDS[1,p8x8[0,p8x8[1,L0[i+1]] xor B] xor A];
             Inc(i,2);
           end;

           Xor256(L1,p8x8[0],(sboxKeys[3] shr 16) and $FF);
           i:= 0;
           while i< 256 do
           begin
             L0[i  ]:= p8x8[0,L1[i]];
             L0[i+1]:= p8x8[0,L1[i+1]];
             Inc(i,2);
           end;
           Xor256(L0,L0,(sboxKeys[2] shr 16) and $FF);
           A:= (sboxKeys[0] shr 16) and $FF;
           B:= (sboxKeys[1] shr 16) and $FF;
           i:= 0;
           while i< 256 do
           begin
             sBox[2 and 2,2*i+(2 and 1)]:= MDS[2,p8x8[1,p8x8[0,L0[i]] xor B] xor A];
             sBox[2 and 2,2*i+(2 and 1)+2]:= MDS[2,p8x8[1,p8x8[0,L0[i+1]] xor B] xor A];
             Inc(i,2);
           end;
           Xor256(L1,p8x8[1],(sboxKeys[3] shr 24));
           i:= 0;
           while i< 256 do
           begin
             L0[i  ]:= p8x8[0,L1[i]];
             L0[i+1]:= p8x8[0,L1[i+1]];
             Inc(i,2);
           end;
           Xor256(L0,L0,(sboxKeys[2] shr 24));
           A:= (sboxKeys[0] shr 24);
           B:= (sboxKeys[1] shr 24);
           i:= 0;
           while i< 256 do
           begin
             sBox[3 and 2,2*i+(3 and 1)]:= MDS[3,p8x8[1,p8x8[1,L0[i]] xor B] xor A];
             sBox[3 and 2,2*i+(3 and 1)+2]:= MDS[3,p8x8[1,p8x8[1,L0[i+1]] xor B] xor A];
             Inc(i,2);
           end;
         end;
  end;
end;




procedure DCP_twofish_EncryptECB(
  const SubKeys: TSubKeys; const SBox: TSBox;
  const InData: T128; var OutData: T128);
var
  i: longword;
  t0, t1: DWord;
  X: T128;
  k: integer;
begin
  for k := 0 to 3 do
    x[k] := InData[k] xor SubKeys[INPUTWHITEN+k];
  i:= 0;
  while i<= NUMROUNDS-2 do
  begin
    t0:= sBox[0,(x[0] shl 1) and $1fe] xor sBox[0,((x[0] shr 7) and $1fe)+1]
      xor sBox[2,(x[0] shr 15) and $1fe] xor sBox[2,((x[0] shr 23) and $1fe)+1];
    t1:= sBox[0,((x[1] shr 23) and $1fe)] xor sBox[0,((x[1] shl 1) and $1fe)+1]
      xor sBox[2,((x[1] shr 7) and $1fe)] xor sBox[2,((x[1] shr 15) and $1fe)+1];
    x[3]:= (x[3] shl 1) or (x[3] shr 31);
    x[2]:= x[2] xor (t0 +   t1 + SubKeys[ROUNDSUBKEYS+2*i]);
    x[3]:= x[3] xor (t0 + 2*t1 + SubKeys[ROUNDSUBKEYS+2*i+1]);
    x[2]:= (x[2] shr 1) or (x[2] shl 31);

    t0:= sBox[0,(x[2] shl 1) and $1fe] xor sBox[0,((x[2] shr 7) and $1fe)+1]
      xor sBox[2,((x[2] shr 15) and $1fe)] xor sBox[2,((x[2] shr 23) and $1fe)+1];
    t1:= sBox[0,((x[3] shr 23) and $1fe)] xor sBox[0,((x[3] shl 1) and $1fe)+1]
      xor sBox[2,((x[3] shr 7) and $1fe)] xor sBox[2,((x[3] shr 15) and $1fe)+1];
    x[1]:= (x[1] shl 1) or (x[1] shr 31);
    x[0]:= x[0] xor (t0 +   t1 + SubKeys[ROUNDSUBKEYS+2*(i+1)]);
    x[1]:= x[1] xor (t0 + 2*t1 + SubKeys[ROUNDSUBKEYS+2*(i+1)+1]);
    x[0]:= (x[0] shr 1) or (x[0] shl 31);
    Inc(i,2);
  end;
  OutData[ 0] := x[2] xor SubKeys[OUTPUTWHITEN];
  OutData[ 1] := x[3] xor SubKeys[OUTPUTWHITEN+1];
  OutData[ 2] := x[0] xor SubKeys[OUTPUTWHITEN+2];
  OutData[ 3] := x[1] xor SubKeys[OUTPUTWHITEN+3];
end;


procedure DCP_twofish_DecryptECB(
  const SubKeys: TSubKeys; const SBox: TSBox;
  const InData: T128; var OutData: T128);
var
  i, k: integer;
  t0, t1: DWord;
  X: T128;
begin
  X[2] := InData[0] xor SubKeys[OUTPUTWHITEN];
  X[3] := InData[1] xor SubKeys[OUTPUTWHITEN+1];
  X[0] := InData[2] xor SubKeys[OUTPUTWHITEN+2];
  X[1] := InData[3] xor SubKeys[OUTPUTWHITEN+3];
  i:= NUMROUNDS-2;
  while i>= 0 do
  begin
    t0:= sBox[0,(x[2] shl 1) and $1fe] xor sBox[0,((x[2] shr 7) and $1fe)+1]
      xor sBox[2,((x[2] shr 15) and $1fe)] xor sBox[2,((x[2] shr 23) and $1fe)+1];
    t1:= sBox[0,((x[3] shr 23) and $1fe)] xor sBox[0,((x[3] shl 1) and $1fe)+1]
      xor sBox[2,((x[3] shr 7) and $1fe)] xor sBox[2,((x[3] shr 15) and $1fe)+1];
    x[0]:= (x[0] shl 1) or (x[0] shr 31);
    x[0]:= x[0] xor (t0 +   t1 + SubKeys[ROUNDSUBKEYS+2*(i+1)]);
    x[1]:= x[1] xor (t0 + 2*t1 + SubKeys[ROUNDSUBKEYS+2*(i+1)+1]);
    x[1]:= (x[1] shr 1) or (x[1] shl 31);

    t0:= sBox[0,(x[0] shl 1) and $1fe] xor sBox[0,((x[0] shr 7) and $1fe)+1]
      xor sBox[2,(x[0] shr 15) and $1fe] xor sBox[2,((x[0] shr 23) and $1fe)+1];
    t1:= sBox[0,((x[1] shr 23) and $1fe)] xor sBox[0,((x[1] shl 1) and $1fe)+1]
      xor sBox[2,((x[1] shr 7) and $1fe)] xor sBox[2,((x[1] shr 15) and $1fe)+1];
    x[2]:= (x[2] shl 1) or (x[2] shr 31);
    x[2]:= x[2] xor (t0 +   t1 + SubKeys[ROUNDSUBKEYS+2*i]);
    x[3]:= x[3] xor (t0 + 2*t1 + SubKeys[ROUNDSUBKEYS+2*i+1]);
    x[3]:= (x[3] shr 1) or (x[3] shl 31);
    Dec(i,2);
  end;
  for k := 0 to 3 do
    OutData[k] := X[k] xor SubKeys[INPUTWHITEN+k]
end;


procedure DCP_towfish_Precomp;
var
  m1, mx, my: array[0..1] of DWord;
  nI: longword;
begin
  for nI:= 0 to 255 do
  begin
    m1[0]:= p8x8[0,nI];
    mx[0]:= Mul_X(m1[0]);
    my[0]:= Mul_Y(m1[0]);
    m1[1]:= p8x8[1,nI];
    mx[1]:= Mul_X(m1[1]);
    my[1]:= Mul_Y(m1[1]);
    mds[0,nI]:= (m1[1] shl 0) or
                (mx[1] shl 8) or
                (my[1] shl 16) or
                (my[1] shl 24);
    mds[1,nI]:= (my[0] shl 0) or
                (my[0] shl 8) or
                (mx[0] shl 16) or
                (m1[0] shl 24);
    mds[2,nI]:= (mx[1] shl 0) or
                (my[1] shl 8) or
                (m1[1] shl 16) or
                (my[1] shl 24);
    mds[3,nI]:= (mx[0] shl 0) or
                (m1[0] shl 8) or
                (my[0] shl 16) or
                (mx[0] shl 24);
  end;
end;



end.

