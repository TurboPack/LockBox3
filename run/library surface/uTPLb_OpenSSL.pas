{* ***** BEGIN LICENSE BLOCK *****
This file is a wrapper around the OpenSSL dynamic link library, using
its published API. As a consequence, there are some identifiers in this
file which are copied from or derived from the OpenSSL source code header files.

Although the use here, of the OpenSSL headers are sufficiently limited,
that copying of this file in relation to the OpenSSL copyright is defended
by the doctrine of fair use, as a measure of respect, please note the following
license conditions for the redistribution of the OpenSSL project products.



========================================================================
========================================================================
==========  BEGINNING OF THE OPENSSL LICENSE DISCLOSURE  ===============
========================================================================
========================================================================

  LICENSE ISSUES
  ==============

  The OpenSSL toolkit stays under a dual license, i.e. both the conditions of
  the OpenSSL License and the original SSLeay license apply to the toolkit.
  See below for the actual license texts. Actually both licenses are BSD-style
  Open Source licenses. In case of any license issues related to OpenSSL
  please contact openssl-core@openssl.org.

  OpenSSL License
  ---------------

/* ====================================================================
 * Copyright (c) 1998-2011 The OpenSSL Project.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * 3. All advertising materials mentioning features or use of this
 *    software must display the following acknowledgment:
 *    "This product includes software developed by the OpenSSL Project
 *    for use in the OpenSSL Toolkit. (http://www.openssl.org/)"
 *
 * 4. The names "OpenSSL Toolkit" and "OpenSSL Project" must not be used to
 *    endorse or promote products derived from this software without
 *    prior written permission. For written permission, please contact
 *    openssl-core@openssl.org.
 *
 * 5. Products derived from this software may not be called "OpenSSL"
 *    nor may "OpenSSL" appear in their names without prior written
 *    permission of the OpenSSL Project.
 *
 * 6. Redistributions of any form whatsoever must retain the following
 *    acknowledgment:
 *    "This product includes software developed by the OpenSSL Project
 *    for use in the OpenSSL Toolkit (http://www.openssl.org/)"
 *
 * THIS SOFTWARE IS PROVIDED BY THE OpenSSL PROJECT ``AS IS'' AND ANY
 * EXPRESSED OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE OpenSSL PROJECT OR
 * ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 * ====================================================================
 *
 * This product includes cryptographic software written by Eric Young
 * (eay@cryptsoft.com).  This product includes software written by Tim
 * Hudson (tjh@cryptsoft.com).
 *
 */

 Original SSLeay License
 -----------------------

/* Copyright (C) 1995-1998 Eric Young (eay@cryptsoft.com)
 * All rights reserved.
 *
 * This package is an SSL implementation written
 * by Eric Young (eay@cryptsoft.com).
 * The implementation was written so as to conform with Netscapes SSL.
 *
 * This library is free for commercial and non-commercial use as long as
 * the following conditions are aheared to.  The following conditions
 * apply to all code found in this distribution, be it the RC4, RSA,
 * lhash, DES, etc., code; not just the SSL code.  The SSL documentation
 * included with this distribution is covered by the same copyright terms
 * except that the holder is Tim Hudson (tjh@cryptsoft.com).
 *
 * Copyright remains Eric Young's, and as such any Copyright notices in
 * the code are not to be removed.
 * If this package is used in a product, Eric Young should be given attribution
 * as the author of the parts of the library used.
 * This can be in the form of a textual message at program startup or
 * in documentation (online or textual) provided with the package.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *    "This product includes cryptographic software written by
 *     Eric Young (eay@cryptsoft.com)"
 *    The word 'cryptographic' can be left out if the rouines from the library
 *    being used are not cryptographic related :-).
 * 4. If you include any Windows specific code (or a derivative thereof) from
 *    the apps directory (application code) you must include an acknowledgement:
 *    "This product includes software written by Tim Hudson (tjh@cryptsoft.com)"
 *
 * THIS SOFTWARE IS PROVIDED BY ERIC YOUNG ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 * The licence and distribution terms for any publically available version or
 * derivative of this code cannot be changed.  i.e. this code cannot simply be
 * copied and put under another distribution licence
 * [including the GNU Public Licence.]
 */
  LICENSE ISSUES
  ==============

  The OpenSSL toolkit stays under a dual license, i.e. both the conditions of
  the OpenSSL License and the original SSLeay license apply to the toolkit.
  See below for the actual license texts. Actually both licenses are BSD-style
  Open Source licenses. In case of any license issues related to OpenSSL
  please contact openssl-core@openssl.org.

  OpenSSL License
  ---------------

/* ====================================================================
 * Copyright (c) 1998-2011 The OpenSSL Project.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * 3. All advertising materials mentioning features or use of this
 *    software must display the following acknowledgment:
 *    "This product includes software developed by the OpenSSL Project
 *    for use in the OpenSSL Toolkit. (http://www.openssl.org/)"
 *
 * 4. The names "OpenSSL Toolkit" and "OpenSSL Project" must not be used to
 *    endorse or promote products derived from this software without
 *    prior written permission. For written permission, please contact
 *    openssl-core@openssl.org.
 *
 * 5. Products derived from this software may not be called "OpenSSL"
 *    nor may "OpenSSL" appear in their names without prior written
 *    permission of the OpenSSL Project.
 *
 * 6. Redistributions of any form whatsoever must retain the following
 *    acknowledgment:
 *    "This product includes software developed by the OpenSSL Project
 *    for use in the OpenSSL Toolkit (http://www.openssl.org/)"
 *
 * THIS SOFTWARE IS PROVIDED BY THE OpenSSL PROJECT ``AS IS'' AND ANY
 * EXPRESSED OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE OpenSSL PROJECT OR
 * ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 * ====================================================================
 *
 * This product includes cryptographic software written by Eric Young
 * (eay@cryptsoft.com).  This product includes software written by Tim
 * Hudson (tjh@cryptsoft.com).
 *
 */

 Original SSLeay License
 -----------------------

/* Copyright (C) 1995-1998 Eric Young (eay@cryptsoft.com)
 * All rights reserved.
 *
 * This package is an SSL implementation written
 * by Eric Young (eay@cryptsoft.com).
 * The implementation was written so as to conform with Netscapes SSL.
 *
 * This library is free for commercial and non-commercial use as long as
 * the following conditions are aheared to.  The following conditions
 * apply to all code found in this distribution, be it the RC4, RSA,
 * lhash, DES, etc., code; not just the SSL code.  The SSL documentation
 * included with this distribution is covered by the same copyright terms
 * except that the holder is Tim Hudson (tjh@cryptsoft.com).
 *
 * Copyright remains Eric Young's, and as such any Copyright notices in
 * the code are not to be removed.
 * If this package is used in a product, Eric Young should be given attribution
 * as the author of the parts of the library used.
 * This can be in the form of a textual message at program startup or
 * in documentation (online or textual) provided with the package.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *    "This product includes cryptographic software written by
 *     Eric Young (eay@cryptsoft.com)"
 *    The word 'cryptographic' can be left out if the rouines from the library
 *    being used are not cryptographic related :-).
 * 4. If you include any Windows specific code (or a derivative thereof) from
 *    the apps directory (application code) you must include an acknowledgement:
 *    "This product includes software written by Tim Hudson (tjh@cryptsoft.com)"
 *
 * THIS SOFTWARE IS PROVIDED BY ERIC YOUNG ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 * The licence and distribution terms for any publically available version or
 * derivative of this code cannot be changed.  i.e. this code cannot simply be
 * copied and put under another distribution licence
 * [including the GNU Public Licence.]
 */

========================================================================
========================================================================
==========  END OF THE OPENSSL LICENSE DISCLOSURE ======================
========================================================================
========================================================================







========================================================================
========================================================================
==========  BEGINNING OF COPYRIGHT NOTICE AND ==========================
==========    STATEMENT OF COPYING PERMISSION FOR THIS FILE  ===========
========================================================================
========================================================================

Copyright 2011 Sean B. Durkin
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


========================================================================
========================================================================
==========  END OF COPYRIGHT NOTICE AND ================================
==========    STATEMENT OF COPYING PERMISSION FOR THIS FILE  ===========
========================================================================
========================================================================
 * ***** END LICENSE BLOCK ***** *}

{$IFDEF VER150}
unit uTPLb_OpenSSL;
{$ENDIF}

{$IF compilerversion > 16}
unit uTPLb_OpenSSL experimental;
{$IFEND}

{$define OpenSSL_Win64_NotYetSupported}
// Undefine the above, when someone has tested the TOpenSSL_Signatory component
//  for 64-bit platform.


interface
uses Classes, uTPLb_BaseNonVisualComponent, uTPLb_Signatory,
     uTPLb_Asymetric, SysUtils, Windows
{$IF compilerversion <= 17}
     , uTPLb_D7Compatibility
{$IFEND}
     ;

const
{$IFDEF WIN32}
  LibEay = 'libeay32.dll';
{$ENDIF}

{$IFDEF WIN64}
  LibEay = 'libeay64.dll';
{$ENDIF}

  Default_AsymetricKeySizeInBits = 2048;

type
TOpenLibError = (
  erNull,
  erSetDLLDirectory,
  erLoadFailed,
  erException,
  erVersionTooLow,
  erSignatureAbsent,
  erPlatformNotSupported
  );


type

TCipherToEncryptPrivateKeyWith = (
  cipher_InTheClear,
  cipher_des_ede3_cbc,
  cipher_des_cbc ,
  cipher_idea_cbc ,
  cipher_seed_cbc ,
  cipher_aes_128_cbc,
  cipher_aes_192_cbc ,
  cipher_aes_256_cbc ,
  cipher_aes_256_ofb ,
  cipher_camellia_128_cbc,
  cipher_camellia_192_cbc,
  cipher_camellia_256_cbc);


const
  EVP_MAX_MD_SIZE			 = 64;
  EVP_MAX_KEY_LENGTH	 = 32;
  EVP_MAX_IV_LENGTH		 = 16;
  EVP_MAX_BLOCK_LENGTH = 32;

type
EVP_CIPHER_CTX = packed record
	cipher: pointer; // to EVP_CIPHER
	engine: pointer; // to ENGINE *engine;	Functional reference if 'cipher' is ENGINE-provided
	encrypt: integer;		// encrypt or decrypt
	buf_len: integer;		// number we have left

	oiv: array[0..EVP_MAX_IV_LENGTH-1] of byte; // original iv
	iv : array[0..EVP_MAX_IV_LENGTH-1] of byte; // working  iv
	buf: array[0..EVP_MAX_BLOCK_LENGTH-1] of byte; // saved partial block
	num: integer;				// used by cfb/ofb mode
	app_data: pointer; // application stuff
	key_len: integer;		// May change for variable length cipher
	flags: longword; // Various flags
	cipher_data: pointer; // per EVP data
	final_used: integer;
	block_mask: integer;
	final1: array[0..EVP_MAX_BLOCK_LENGTH-1] of byte; // possible final block */
  xtra: array[0..100] of byte;
	end;

EVP_CIPHER_varProc = procedure( var a: EVP_CIPHER_CTX) cdecl;
EVP_CIPHER_intFunc = function ( var a: EVP_CIPHER_CTX): integer cdecl;

PBIO = pointer; // For the curious, you can find a more transparent
                //  definition of BIO/PBIO in unit IdSSLOpenSSLHeaders,
                //  or go directly to the OpenSSL header files.

TOpenSSL_Signatory = class;


TBIO = class( TObject)
  protected
    FLib: TOpenSSL_Signatory;
    FBIOobj: pBIO;

    function InitBIO_Object: pBIO; virtual; abstract;

  public
    constructor Create( Lib1: TOpenSSL_Signatory);
    function    ReadAnsiString: ansistring;
      // Actually the character encoding must be determined by the context.
      // It is not necessarily ansi. It could be a raw byte data.
    destructor  Destroy; override;
  end;

TMemoryBIO = class( TBIO)
  protected
    function InitBIO_Object: pBIO; override;

  public
  end;

TReadOnlyBIO = class( TBIO)
  private
    FData: TBytes;

  protected
    function InitBIO_Object: pBIO; override;

  public
    constructor Create( Lib1: TOpenSSL_Signatory; Data: pointer; DataLen: integer);
    destructor Destroy; override;
  end;


PBN_GENCB = ^BN_GENCB;
BN_GENCB = packed record
  ver : longword; // WILL be 2.
  arg : pointer;
  cb_2: function( p1, p2 : longint; p3: PBN_GENCB): longint; cdecl;
  end;

pem_password_cb = function( buf: PAnsistring; size, rwflag: longint;
                            userdata: pointer): longint; cdecl;

TSigHashKind = ( // How was the input message to sign/verify computed from the source document?
  hshSHA1,       // The document was hashed with SHA1 to produce the input message. NID_sha1 = 64
  hshRipemd160,  // The hash algorithm was ripemd160. NID_ripemd160 = 117
  hshMD5);       // The hash was MD5. NID_md5 = 4

BIGNUM = packed record
  d    : longword;
  top  : longint;
  dmax : longint;
  neg  : longint;
  flags: longint;
  end;
PBIGNUM = ^BIGNUM;

CRYPTO_EX_DATA = packed record
  sk   : pointer;
  dummy: longint;
  end;

RSA = packed record
  pad     : longint;
  version : longword;
  engine  : pointer; // PENGINE
  n : PBIGNUM;
  e : PBIGNUM;
  d : PBIGNUM;
  p : PBIGNUM;
  q : PBIGNUM;
  dmp1 : PBIGNUM;
  dmq1 : PBIGNUM;
  iqmp : PBIGNUM;
  ex_data : CRYPTO_EX_DATA;
  references: longint;
  flags : longint;
  _method_mod_n : pointer;
  _method_mod_p : pointer;
  _method_mod_q : pointer;
  bignum_data : PAnsichar;
  blinding : pointer;
  mt_blinding : pointer;
  end;
PRSA  = ^RSA;
PPRSA = ^PRSA;

Pbio_info_cb = procedure( _para1: PBIO; _para2: longint;
  _para3: PAnsichar; _para4: longint; _para5, _para6: longword); cdecl;

BIO_METHOD = packed record
  _type: longint;
  name : PAnsichar;
  bwrite       : function (_para1: PBIO; _para2 : PAnsichar; _para3 : longint): longint; cdecl;
  bread        : function (_para1: PBIO; _para2: PAnsiChar; _para3: longint): longint; cdecl;
  bputs        : function (_para1: PBIO; _para2 : PAnsichar): longint; cdecl;
  bgets        : function (_para1: PBIO; _para2 : PAnsichar; _para3: longint): longint; cdecl;
  ctrl         : function (_para1: PBIO; _para2 : longint; _para3: longword; _para4: pointer): longword; cdecl;
  create       : function (_para1: PBIO): longint; cdecl;
  destroy      : function (_para1: PBIO): longint; cdecl;
  callback_ctrl: function (_para1: PBIO; _para2: longint; _para : pbio_info_cb): longword; cdecl;
  end;
PBIO_METHOD = ^BIO_METHOD;

PEVP_CIPHER = pointer;

TOpenSLL_CipherFunc = function: PEVP_CIPHER cdecl;

TOnOpenSSLProgress = function( Sender: TObject; p1, p2: integer): boolean of object;


TOpenSSL_Base = class
{$IF CompilerVersion > 15}
                      abstract
{$IFEND}
                              ( TTPLb_BaseNonVisualComponent)
  private
    FHandle: HMODULE;
    FLibPath: string;
    FLibName: string;
    FFileName: string;  // LibPath + LibName
    FDLLDir: string;    // dir of needed DLL's.
    FRqrMajorV: integer;
    FRqrMinorV: integer;
    FRqrReleaseV: integer;
    FRqrBuildV: integer;
    FErr: TOpenLibError;
    FWindowsError: DWORD;
    FErrMsg: string;
    FMajorV, FMinorV, FReleaseV, FBuildV: integer;
    FhaveTriedToOpen: boolean;
    FdoOPENSSL_LOAD_CONF: boolean;
    FisSafeToFreeRSA: boolean;
    FPassword: utf8string;
    FCachedSize: integer;

    FOpenSSLproc_BioSMem: function: PBIO_METHOD cdecl;
    FOpenSSLproc_BioNew : function(_type: PBIO_METHOD): PBIO cdecl;
    FOpenSSLproc_BioFree: function( bio: PBIO): longint cdecl;
    FOpenSSLproc_BioRead: function( b: PBIO; data: Pointer; len: longint): longint cdecl;
    //FOpenSSLProc_setup_engine: function( var err: BIO; sEngine: PChar; debug: longint): PEngine; cdecl;
    FOpenSSLProc_RAND_seed: procedure( const buf: pointer; num: integer); cdecl;
    FOpenSSLProc_RAND_status: function: integer; cdecl;
    FOpenSSLProc_RSA_new: function: PRSA;
    // FOpenSSLProc_RSA_new_method: function( Eng: PEngine): PRSA;
    FOpenSSLProc_RSA_size: function( RSA: PRSA): longint;
    FOpenSSLProc_RSA_free: procedure( R: PRSA);
    FOpenSSLProc_BN_set_word: function( n: pBIGNUM; w: longword): longint; cdecl;
    FOpenSSLProc_RSA_generate_key_ex: function( rsa : PRSA; bits: longint; e: PBIGNUM; cb: PBN_GENCB): integer; cdecl;
    FOpenSSLProc_BN_free: procedure( n: PBIGNUM); cdecl;
    FOpenSSLProc_BN_new: function: PBIGNUM;
    FOpenSSLProc_ERR_get_err: function: longword; cdecl;
    FOpenSSLProc_ERR_error_string_n: procedure( ErrorCode: longword; Buf: PAnsiChar; len: longword); cdecl;
    FOpenSSLProc_BN_get_word: function ( a: PBIGNUM): longword; cdecl;
    FOpenSSLProc_RSA_up_ref: function( rsa: PRSA): longint; cdecl;
    FOpenSSLProc_PEM_write_bio_RSAPrivateKey:
      function( bp: PBIO; x: PRSA; const enc: PEVP_CIPHER; kstr: PAnsiChar;
         klen: longint; cb: pem_password_cb; u: pointer): longint; cdecl;
    FOpenSSLProc_PEM_write_bio_RSAPublicKey: function( bp: PBIO; x: PRSA): longint; cdecl;
    FOpenSSLProc_RSA_sign: function( type1: longint; m: pointer; m_len: longword;
      sigret: PAnsichar; var siglen: longword; rsa: PRSA): longint; cdecl;
    FOpenSSLProc_RSA_verify: function (type1: longint; m: pointer; m_len: longword;
       sigbuf: PAnsichar; siglen: longword; rsa: PRSA): longint; cdecl;
    FOpenSSLProc_RSA_print: function( bp: PBIO; x: PRSA; offset: longint): longint; cdecl;
    FOpenSSLProc_PEM_read_bio_RSAPrivateKey:
      function( bp: PBIO; x: PPRSA; cb: pem_password_cb; u: pointer): PRSA; cdecl;
    FOpenSSLProc_BIO_new_mem_buf: function( buf: pointer; len: longint): PBIO; cdecl;
    FOpenSSLProc_PEM_read_bio_RSAPublicKey:
      function( bp: PBIO; x: PPRSA; cb: pem_password_cb; u: pointer): PRSA; cdecl;

    FOpenSSLProc_EVP_des_ede3_cbc    : TOpenSLL_CipherFunc; // des3
    FOpenSSLProc_EVP_des_cbc         : TOpenSLL_CipherFunc;
    FOpenSSLProc_EVP_idea_cbc        : TOpenSLL_CipherFunc; // idea
    FOpenSSLProc_EVP_seed_cbc        : TOpenSLL_CipherFunc; //  Encrypt PEM output with CBC seed
    FOpenSSLProc_EVP_aes_128_cbc     : TOpenSLL_CipherFunc;
    FOpenSSLProc_EVP_aes_192_cbc     : TOpenSLL_CipherFunc;
    FOpenSSLProc_EVP_aes_256_cbc     : TOpenSLL_CipherFunc;
    FOpenSSLProc_EVP_aes_256_ofb     : TOpenSLL_CipherFunc;
    FOpenSSLProc_EVP_camellia_128_cbc: TOpenSLL_CipherFunc;
    FOpenSSLProc_EVP_camellia_192_cbc: TOpenSLL_CipherFunc;
    FOpenSSLProc_EVP_camellia_256_cbc: TOpenSLL_CipherFunc;

    FOpenSSLProc_EVP_CIPHER_CTX_init: EVP_CIPHER_varProc;
    FOpenSSLProc_EVP_CIPHER_CTX_cleanup: EVP_CIPHER_intFunc;
    FOpenSSLProc_EVP_CipherInit_ex: function( var ctx: EVP_CIPHER_CTX;
      const cipher: PEVP_CIPHER; impl: pointer {PENGINE};
		  const key: PByte; const iv: PByte; enc: integer): integer; cdecl;
    FOpenSSLProc_EVP_CIPHER_CTX_set_key_length:
      function (var x: EVP_CIPHER_CTX; keylen: integer): integer; cdecl;
    FOpenSSLProc_EVP_CipherUpdate: function( var ctx: EVP_CIPHER_CTX;
      const outBytes: PByte; var outLen: integer;
      const inBytes: PByte; inLen: integer): integer; cdecl;
    FOpenSSLProc_EVP_CipherFinal_ex: function( var ctx: EVP_CIPHER_CTX;
      const outBytes: PByte; var outLen: integer): integer; cdecl;
    FOpenSSLProc_EVP_CIPHER_CTX_set_padding: function( var ctx: EVP_CIPHER_CTX;
      pad: integer): integer; cdecl;

    function GetEVP( Cipher: TCipherToEncryptPrivateKeyWith): PEVP_CIPHER;

    function TryOpen(
      const LibPath: string;    // Empty or path to lib (no final backslash)
      const LibName: string;    // pathless library filename.
      const DLLDir: string;     // Empty or SetDLLDirectory directory.
      RqrMajorV, RqrMinorV, RqrReleaseV, RqrBuildV: integer;
      const RqrSignatureProc: string; // An exported procedure of this name
        // MUST be in the library or it is not valid.
                                // Minimum requirements.
                                // If zero's then no requirements.
      var Err: TOpenLibError;   // Kind of error.
      var Handle: HMODULE;      // Returned library handle.
      var WindowsError: DWORD;  // windows GetLastError where appropriate.
      var FileName: string;     // Full file name of loaded library
      var ErrMsg: string;       // Error message, where available.
      var MajorV, MinorV, ReleaseV, BuildV: integer // Actual version numbers.

      ): boolean;               // True if and only if loaded ok.

    function  GetIsLoaded: boolean;
    procedure SetIsLoaded( Value: boolean);

    procedure SetLibPath( const Value: string);
    procedure SetLibName( const Value: string);
    procedure SetDLLDir( const Value: string);
    function  GetRequiredVersionStr: string;
    procedure SetRequiredVersionStr( const Value: string);
    procedure OpenLib;
    procedure CloseLib;
    procedure GetProcs;
    procedure ClearProcs;
    procedure SetMemoryManager;
    procedure App_Start;
    procedure App_Shutdown;
    function  isLibraryUniqueInstance: boolean;
    procedure GetAndCallParameterless( const ProcName: string);
    function  OpenSSL_CallBack( p1, p2: integer): integer;
    procedure GetError( var ErrorCode: integer; var ErrorMsg: string);

  protected
    FStoragePassword: utf8string;
    FOnProgress: TOnOpenSSLProgress;
    procedure PreShutdown; virtual;

  public
    constructor Create( AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;

    property  isLoaded: boolean  read GetIsLoaded write SetIsLoaded;

    property Handle: HMODULE read FHandle;
    property FileName: string     read FFileName;
    property Err: TOpenLibError   read FErr;
    property WindowsError: DWORD  read FWindowsError;
    property ErrMsg: string       read FErrMsg;
    property MajorV: integer      read FMajorV;
    property MinorV: integer      read FMinorV;
    property ReleaseV: integer    read FReleaseV;
    property BuildV: integer      read FBuildV;
    property haveTriedToOpen: boolean  read FhaveTriedToOpen;


  published
    property LibPath: string           read FLibPath write SetLibPath;
    property LibName: string           read FLibName write SetLibName; // default LibEay32;
    property SupportDLLDir: string     read FDLLDir  write  SetDLLDir;
      // ^ dir of needed DLL's.
    property RequiredVersion: string   read GetRequiredVersionStr write SetRequiredVersionStr;
     // ^ Must be like '1.2.3.4' or '1.2.3' or '1.2' or '1' or empty.
    property OnProgress: TOnOpenSSLProgress   read FOnProgress write FOnProgress;
  end;


{$IF CompilerVersion >= 23.0}
  {$IFDEF OpenSSL_Win64_NotYetSupported}
  [ComponentPlatformsAttribute( pidWin32)] // Just 32-bit for the moment.
  {$ELSE}
  [ComponentPlatformsAttribute( pidWin32, pidWin64)]
  {$ENDIF}
{$IFEND}
TOpenSSL_Signatory = class( TOpenSSL_Base)
  private
    FRSA: PRSA;
    FStorageCipher: TCipherToEncryptPrivateKeyWith;
    FAsymetricKeySizeInBits: cardinal;
    FKeyStorageParts: TKeyStoragePartSet;

    function  PEM_Password_CallBack( buf: PAnsistring; size, rwflag: longint): longint;
    procedure CloseRSA;
    procedure GenRSA( KeySizeInBits: integer; var ErrorCode: integer; var ErrorMsg: string);


    procedure LoadRSA_PrivateKey(
      var ErrorMessage: string;
      Cipher: TCipherToEncryptPrivateKeyWith;
      const sPrivateKey: utf8string;
      const Password: utf8string);

    procedure LoadRSA_PublicKey(
      var ErrorMessage: string;
      const sPublicKey: utf8string);

    function  RSA_Size: integer;

    procedure RSA_Sign(
      PreHash: TSigHashKind;
      Message1: pointer; MessageLen: cardinal;
      var Signature: utf8string; var ErrorMessage: string);

    function RSA_Verify(
      PreHash: TSigHashKind;
      Message1: pointer; MessageLen: cardinal;
      const Signature: utf8string): boolean; // True for verified good.

    function  PrivateKey_AsPEM( Cipher: TCipherToEncryptPrivateKeyWith; const Password: utf8string): utf8string;

    function  GetPrivateKey_AsPEM: utf8string;
    procedure SetPrivateKey_AsPEM( const Value: utf8string);
    function  GetPublicKey_AsPEM: utf8string;
    procedure SetPublicKey_AsPEM( const Value: utf8string);

  protected
    procedure PreShutdown; override;

  public
    constructor Create( AOwner: TComponent); override;
    function  Sign( PreHash: TSigHashKind; HashedDocument, Signature: TStream): boolean;
    function  Verify( PreHash: TSigHashKind; HashedDocument, Signature: TStream): TVerifyResult;

    function  GenerateKeys: boolean;
    procedure StoreKeysToStream( Store: TStream; Parts: TKeyStoragePartSet);
    procedure LoadKeysFromStream( Store: TStream; Parts: TKeyStoragePartSet);
    function  HasParts: TKeyStoragePartSet;
    procedure Randomize;
    function  SignVerify_SelfTest: boolean;
    function  Print: string;

    property PrivateKey: utf8string   read GetPrivateKey_AsPEM write SetPrivateKey_AsPEM;
    property PublicKey : utf8string   read GetPublicKey_AsPEM write SetPublicKey_AsPEM;

  published
    property  AsymetricKeySizeInBits: cardinal    read  FAsymetricKeySizeInBits
                                                  write FAsymetricKeySizeInBits;

    property PrivateKeyStoragePassword: utf8string   read FStoragePassword write FStoragePassword;
    property PrivateKeyStorageCipher: TCipherToEncryptPrivateKeyWith read FStorageCipher write FStorageCipher;
    property OnProgress: TOnOpenSSLProgress   read FOnProgress write FOnProgress;
  end;

TOpenSSL_PaddingScheme = (
  padNone,
  padPKCS);

{$IF CompilerVersion >= 23.0}
  {$IFDEF OpenSSL_Win64_NotYetSupported}
  [ComponentPlatformsAttribute( pidWin32)] // Just 32-bit for the moment.
  {$ELSE}
  [ComponentPlatformsAttribute( pidWin32, pidWin64)]
  {$ENDIF}
{$IFEND}
TOpenSSL_Codec = class( TOpenSSL_Base)
  private
    FCipher: TCipherToEncryptPrivateKeyWith;
    FKey: TBytes;
    FIV: TBytes;
    FisInitialised: boolean;
    Fctx: EVP_CIPHER_CTX;
    FPad: TOpenSSL_PaddingScheme;
    procedure BeginEncDec( doEncrypt: Boolean);

  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;

    procedure SetKey(  const key1: TBytes);
    procedure SetIV (  const iv1 : TBytes);
    procedure Encrypt( PlaintextInStream, CiphertextOutStream: TStream);
    procedure Decrypt( PlaintextOutStream, CiphertextInStream: TStream);

  published
    property Cipher: TCipherToEncryptPrivateKeyWith  read FCipher write FCipher;
    property PaddingScheme: TOpenSSL_PaddingScheme   read FPad write FPad;
  end;

implementation



{$WARNINGS OFF}
uses SyncObjs, Contnrs, uTPLb_Random;


const
  RSA_F4: longint = $10001;

var
  OpenSSL_LibGate: TCriticalSection = nil;
  OpenSSL_Libs   : TObjectList      = nil; // of TOpenSSL_Lib

{$IFDEF HuntingMemoryLeaks}
  Allocated: int64 = 0;
{$ENDIF}

procedure InitUnit_OpenSSL_Lib;
begin
OpenSSL_LibGate := TCriticalSection.Create;
OpenSSL_Libs    := nil;
{$IFDEF HuntingMemoryLeaks}
Allocated       := 0
{$ENDIF}
end;

procedure DoneUnit_OpenSSL_Lib;
begin
FreeAndNil( OpenSSL_LibGate);
FreeAndNil( OpenSSL_Libs)
end;






function cb_2_SpringBoard( p1, p2 : longint; p3 : PBN_GENCB): longint; cdecl;
var
  InvokerObj: TObject;
begin
InvokerObj := TObject( p3.arg);
if InvokerObj is TOpenSSL_Signatory then
    result := TOpenSSL_Signatory( InvokerObj).OpenSSL_CallBack( p1, p2)
  else
    result := 0
end;


function pem_password_cb_SpringBoard(
  buf: PAnsistring; size, rwflag: longint;
  userdata: pointer): longint; cdecl;
var
  InvokerObj: TObject;
begin
InvokerObj := TObject( userdata);
if InvokerObj is TOpenSSL_Signatory then
    result := TOpenSSL_Signatory( InvokerObj)
      .PEM_Password_CallBack( buf, size, rwflag)
  else
    result := 0
end;


{ TBIO }

constructor TBIO.Create( Lib1: TOpenSSL_Signatory);
begin
FLib    := Lib1;
FBIOobj := InitBIO_Object
end;


destructor TBIO.Destroy;
begin
if assigned( FBIOobj) and assigned( FLib) and (FLib.FHandle <> 0) then
  begin
  FLib.FOpenSSLproc_BioFree( FBIOobj);
  FBIOobj := nil
  end;
inherited
end;

function TBIO.Readansistring: ansistring;
var
  Buffer: ansistring;
  Amount: longint;
begin
result := '';
if (not assigned( FLib)) or (FLib.FHandle = 0) then exit;
SetLength( Buffer, 1024);
repeat
  Amount := FLib.FOpenSSLproc_BioRead( FBIOobj, @Buffer[1], Length( Buffer));
  if Amount > 0 then
    result := result + Copy( Buffer, 1, Amount)
until Amount = -1
end;


{ TMemoryBIO }

function TMemoryBIO.InitBIO_Object: pBIO;
begin
result := FLib.FOpenSSLproc_BioNew( FLib.FOpenSSLproc_BioSMem)
end;

{ TReadOnlyBIO }

constructor TReadOnlyBIO.Create(
  Lib1: TOpenSSL_Signatory; Data: pointer; DataLen: integer);
begin
SetLength( FData, DataLen);
if DataLen > 0 then
  Move( Data^, FData[0], DataLen);
inherited Create( Lib1)
end;


destructor TReadOnlyBIO.Destroy;
begin
inherited;
SetLength( FData, 0)
end;

function TReadOnlyBIO.InitBIO_Object: pBIO;
begin
if Length( FData) > 0 then
    result := FLib.FOpenSSLProc_BIO_new_mem_buf( @FData[0], Length( FData))
  else
    result := FLib.FOpenSSLProc_BIO_new_mem_buf( nil, 0)
end;

{ TOpenSSL_Signatory }

procedure TOpenSSL_Base.App_Shutdown;

  procedure GetAndCallOneIntParam( const ProcName: string; Value: integer);
  type
    TProc_OneIntParam = procedure( P1: longint); cdecl;
  var
    Proc: TProc_OneIntParam;
  begin
  if (FHandle = 0) or (ProcName = '') then exit;
  @Proc := windows.GetProcAddress( FHandle, PChar( ProcName));
  if assigned( Proc) then
    Proc( Value)
  end;

  procedure GetAndCallOnePtrParam( const ProcName: string; Value: pointer);
  type
    TProc_OnePntrParam = procedure( P1: pointer); cdecl;
  var
    Proc: TProc_OnePntrParam;
  begin
  if (FHandle = 0) or (ProcName = '') then exit;
  @Proc := windows.GetProcAddress( FHandle, PChar( ProcName));
  if assigned( Proc) then
    Proc( Value)
  end;

begin
GetAndCallOneIntParam  ( 'CONF_modules_unload', 1);
GetAndCallParameterless( 'OBJ_cleanup');
GetAndCallParameterless( 'EVP_cleanup');
GetAndCallParameterless( 'ENGINE_cleanup');
GetAndCallParameterless( 'CRYPTO_cleanup_all_ex_data');
GetAndCallOnePtrParam  ( 'ERR_remove_thread_state', nil);
GetAndCallParameterless( 'ERR_free_strings')
end;

procedure TOpenSSL_Base.App_Start;
begin
GetAndCallParameterless( 'ERR_load_crypto_strings');
if FdoOPENSSL_LOAD_CONF then
    GetAndCallParameterless( 'OPENSSL_add_all_algorithms_conf')
  else
    GetAndCallParameterless( 'OPENSSL_add_all_algorithms_noconf');
GetAndCallParameterless( 'ENGINE_load_builtin_engines');
Randomize
end;

procedure TOpenSSL_Base.Clear;
begin
SetIsLoaded( False);
FhaveTriedToOpen := False
end;


procedure TOpenSSL_Base.ClearProcs;
begin
@FOpenSSLproc_BioSMem := nil;
@FOpenSSLproc_BioNew  := nil;
@FOpenSSLproc_BioFree := nil;
@FOpenSSLproc_BioRead := nil;
//@FOpenSSLProc_setup_engine := nil;
@FOpenSSLProc_RAND_seed := nil;
@FOpenSSLProc_RAND_status := nil;
@FOpenSSLProc_RSA_new        := nil;
//@FOpenSSLProc_RSA_new_method := nil;
@FOpenSSLProc_RSA_size       := nil;
@FOpenSSLProc_RSA_free       := nil;
@FOpenSSLProc_BN_set_word         := nil;
@FOpenSSLProc_RSA_generate_key_ex := nil;
@FOpenSSLProc_BN_free := nil;
@FOpenSSLProc_BN_new  := nil;
@FOpenSSLProc_ERR_get_err        := nil;
@FOpenSSLProc_ERR_error_string_n := nil;
@FOpenSSLProc_BN_get_word := nil;
@FOpenSSLProc_RSA_up_ref := nil;
@FOpenSSLProc_PEM_write_bio_RSAPrivateKey := nil;
@FOpenSSLProc_PEM_write_bio_RSAPublicKey := nil;
@FOpenSSLProc_RSA_sign   := nil;
@FOpenSSLProc_RSA_verify := nil;
@FOpenSSLProc_RSA_print  := nil;
@FOpenSSLProc_PEM_read_bio_RSAPrivateKey := nil;
@FOpenSSLProc_BIO_new_mem_buf := nil;
@FOpenSSLProc_PEM_read_bio_RSAPublicKey := nil;

@FOpenSSLProc_EVP_des_ede3_cbc     := nil;
@FOpenSSLProc_EVP_des_cbc          := nil;
@FOpenSSLProc_EVP_idea_cbc         := nil;
@FOpenSSLProc_EVP_seed_cbc         := nil;
@FOpenSSLProc_EVP_aes_128_cbc      := nil;
@FOpenSSLProc_EVP_aes_192_cbc      := nil;
@FOpenSSLProc_EVP_aes_256_cbc      := nil;
@FOpenSSLProc_EVP_aes_256_ofb       := nil;
@FOpenSSLProc_EVP_camellia_128_cbc := nil;
@FOpenSSLProc_EVP_camellia_192_cbc := nil;
@FOpenSSLProc_EVP_camellia_256_cbc := nil;

@FOpenSSLProc_EVP_CIPHER_CTX_init    := nil;
@FOpenSSLProc_EVP_CIPHER_CTX_cleanup := nil;
@FOpenSSLProc_EVP_CipherInit_ex      := nil;
@FOpenSSLProc_EVP_CIPHER_CTX_set_key_length := nil;
@FOpenSSLProc_EVP_CipherUpdate              := nil;
@FOpenSSLProc_EVP_CipherFinal_ex            := nil;
@FOpenSSLProc_EVP_CIPHER_CTX_set_padding    := nil;

end;

procedure TOpenSSL_Base.CloseLib;
begin
if FHandle <> 0 then
  begin
  FhaveTriedToOpen := False; // Only count failed attempts to open.
  OpenSSL_LibGate.Enter;
  try
    PreShutdown;
    if isLibraryUniqueInstance then
      App_Shutdown;
    FreeLibrary( FHandle);
    FHandle := 0;
    ClearProcs;
    if assigned( OpenSSL_Libs) and (OpenSSL_Libs.IndexOf( self) <> -1) then
      OpenSSL_Libs.Remove( self);
    if assigned( OpenSSL_Libs) and (OpenSSL_Libs.Count = 0) then
      FreeAndNil( OpenSSL_Libs)
  finally
    OpenSSL_LibGate.Leave
    end
  end;
FCachedSize := -1;
FFileName := '';
FErr := erNull;
FWindowsError := 0;
FErrMsg   := '';
FMajorV   := 0;
FMinorV   := 0;
FReleaseV := 0;
FBuildV   := 0
end;



procedure TOpenSSL_Signatory.CloseRSA;

  procedure ReleaseBigNumDataMember( var n: PBIGNUM);
  begin
  if assigned( n) and assigned( FOpenSSLProc_BN_free) then
    begin
    FOpenSSLProc_BN_free( n);
    n := nil
    end;
  end;

begin
FKeyStorageParts := [];
if assigned( FOpenSSLProc_RSA_free) and assigned( FRSA) then
  begin
  ReleaseBigNumDataMember( FRSA^.n);
  ReleaseBigNumDataMember( FRSA^.e);
  ReleaseBigNumDataMember( FRSA^.d);
  ReleaseBigNumDataMember( FRSA^.p);
  ReleaseBigNumDataMember( FRSA^.q);
  ReleaseBigNumDataMember( FRSA^.dmp1);
  ReleaseBigNumDataMember( FRSA^.dmq1);
  ReleaseBigNumDataMember( FRSA^.iqmp);
  if FisSafeToFreeRSA then
    try
      FOpenSSLProc_RSA_free( FRSA)
    except
      FisSafeToFreeRSA := False
    end;
  FRSA := nil
  end
end;

constructor TOpenSSL_Base.Create( AOwner: TComponent);
begin
inherited Create( AOwner);
FHandle := 0;
FLibPath := '';
FLibName := LibEay;
FFileName := '';
FDLLDir := '';
FRqrMajorV   := 1;
FRqrMinorV   := 0;
FRqrReleaseV := 0;
FRqrBuildV   := 0;
FErr := erNull;
FWindowsError := 0;
FErrMsg := '';
FMajorV := 0;
FMinorV := 0;
FReleaseV := 0;
FBuildV := 0;
FhaveTriedToOpen := False;
FdoOPENSSL_LOAD_CONF := False;
FisSafeToFreeRSA := False;
FCachedSize := -1;
ClearProcs;
FPassword := '';
FStoragePassword := '';
end;

destructor TOpenSSL_Base.Destroy;
begin
Clear;
inherited
end;



constructor TOpenSSL_Signatory.Create( AOwner: TComponent);
begin
inherited Create( AOwner);
FAsymetricKeySizeInBits := Default_AsymetricKeySizeInBits;
FRSA := nil;
FStorageCipher := cipher_InTheClear;
FKeyStorageParts := []
end;

function TOpenSSL_Signatory.GenerateKeys: boolean;
  var ErrorCode: integer;
  var ErrorMsg: string;
begin
GenRSA( FAsymetricKeySizeInBits, ErrorCode, ErrorMsg);
result := ErrorMsg = '';
if not result then
  FErrMsg := ErrorMsg
end;


procedure TOpenSSL_Signatory.GenRSA(
  KeySizeInBits: integer;
  var ErrorCode: integer; var ErrorMsg: string);
var
  bn: PBIGNUM;
  Ok: boolean;
  cb: BN_GENCB;
{$IFDEF HuntingMemoryLeaks}
  StartMemory, Leakage: int64;
{$ENDIF}
//  UserAborted: boolean;

begin
{$IFDEF HuntingMemoryLeaks}
StartMemory := Allocated;
{$ENDIF}
try
ErrorCode := 0;
//UserAborted := False;
CloseRSA;
FisSafeToFreeRSA := True;
ErrorMsg  := '(Error) TOpenSSL_Signatory not loaded.';
if not assigned( FOpenSSLProc_RSA_new) then exit;
ErrorMsg  := '';
FRSA := FOpenSSLProc_RSA_new;
bn := FOpenSSLProc_BN_new;
try
  Ok := FOpenSSLProc_BN_set_word( bn, RSA_F4) = 1;
  cb.ver   := 2;
  cb.arg   := pointer( self);
  @cb.cb_2 := @cb_2_SpringBoard;
  if Ok then
    begin
    Ok := FOpenSSLProc_RSA_generate_key_ex( FRSA, KeySizeInBits, bn, @cb) = 1;
    if Ok then
        begin
        FCachedSize := KeySizeInBits;
        FKeyStorageParts := [partPublic, partPrivate]
        end
      else
        GetError( ErrorCode, ErrorMsg)
    end;
finally
  if assigned( bn) then
    FOpenSSLProc_BN_free( bn);
  end;
// UserAborted := ErrorCode = $04081003; // = 67637251

finally
FisSafeToFreeRSA := False
end;
{$IFDEF HuntingMemoryLeaks}
Leakage := Allocated - StartMemory;
{$ENDIF}
end;


procedure TOpenSSL_Base.GetAndCallParameterless( const ProcName: string);
type
  TProc_NoParams = procedure; cdecl;
var
  Proc: TProc_NoParams;
begin
if (FHandle = 0) or (ProcName = '') then exit;
@Proc := windows.GetProcAddress( FHandle, PChar( ProcName));
if assigned( Proc) then
  Proc
end;



procedure TOpenSSL_Base.GetError(
  var ErrorCode: integer; var ErrorMsg: string);
var
  aErrorMsg: ansistring;
begin
ErrorCode := 0;
if not assigned( FOpenSSLProc_ERR_get_err) then
    begin
    ErrorCode := -1;
    ErrorMsg  := 'TOpenSSL_Lib not loaded.'
    end
  else
    begin
    aErrorMsg := '';
    ErrorCode := FOpenSSLProc_ERR_get_err;
    if ErrorCode <> 0 then
      begin
      SetLength( aErrorMsg, 500);
      FOpenSSLProc_ERR_error_string_n( ErrorCode, PAnsiChar( aErrorMsg), Length( aErrorMsg));
      SetLength( aErrorMsg, StrLen( PAnsiChar( aErrorMsg)))
      end;
    ErrorMsg := aErrorMsg // Coerce system code page ansi into utf16
    end;
end;



function TOpenSSL_Base.GetEVP(
  Cipher: TCipherToEncryptPrivateKeyWith): PEVP_CIPHER;
var
  Func: TOpenSLL_CipherFunc;

begin
case Cipher of
  cipher_InTheClear          : begin
                               result := nil;
                               exit;
                               end;
  cipher_des_ede3_cbc        : @Func := @FOpenSSLProc_EVP_des_ede3_cbc;
  cipher_des_cbc             : @Func := @FOpenSSLProc_EVP_des_cbc;
  cipher_idea_cbc            : @Func := @FOpenSSLProc_EVP_idea_cbc;
  cipher_seed_cbc            : @Func := @FOpenSSLProc_EVP_seed_cbc;
  cipher_aes_128_cbc         : @Func := @FOpenSSLProc_EVP_aes_128_cbc;
  cipher_aes_192_cbc         : @Func := @FOpenSSLProc_EVP_aes_192_cbc;
  cipher_aes_256_cbc         : @Func := @FOpenSSLProc_EVP_aes_256_cbc;
  cipher_aes_256_ofb         : @Func := @FOpenSSLProc_EVP_aes_256_ofb;
  cipher_camellia_128_cbc    : @Func := @FOpenSSLProc_EVP_camellia_128_cbc;
  cipher_camellia_192_cbc    : @Func := @FOpenSSLProc_EVP_camellia_192_cbc;
  cipher_camellia_256_cbc    : @Func := @FOpenSSLProc_EVP_camellia_256_cbc;
  end;
if assigned( Func) then
    result := Func // This is a function call.
  else
    result := nil
end;



function TOpenSSL_Base.GetIsLoaded: boolean;
begin
result := FHandle <> 0
end;


procedure TOpenSSL_Base.GetProcs;

  function ProcAddress( const ProcName: string): pointer;
  begin
  result := windows.GetProcAddress( FHandle, PChar( ProcName))
  end;

begin
ClearProcs;
if FHandle = 0 then exit;
@FOpenSSLproc_BioSMem := ProcAddress( 'BIO_s_mem');
@FOpenSSLproc_BioNew  := ProcAddress( 'BIO_new');
@FOpenSSLproc_BioFree := ProcAddress( 'BIO_free');
@FOpenSSLproc_BioRead := ProcAddress( 'BIO_read');
//@FOpenSSLProc_setup_engine := ProcAddress( 'setup_engine');
@FOpenSSLProc_RAND_seed := ProcAddress( 'RAND_seed');
@FOpenSSLProc_RAND_status := ProcAddress( 'RAND_status');
@FOpenSSLProc_RSA_new        := ProcAddress( 'RSA_new');
//@FOpenSSLProc_RSA_new_method := ProcAddress( 'RSA_new_method');
@FOpenSSLProc_RSA_size       := ProcAddress( 'RSA_size');
@FOpenSSLProc_RSA_free       := ProcAddress( 'RSA_free');
@FOpenSSLProc_BN_set_word         := ProcAddress( 'BN_set_word');
@FOpenSSLProc_RSA_generate_key_ex := ProcAddress( 'RSA_generate_key_ex');
@FOpenSSLProc_BN_free := ProcAddress( 'BN_free');
@FOpenSSLProc_BN_new  := ProcAddress( 'BN_new');
@FOpenSSLProc_ERR_get_err        := ProcAddress( 'ERR_get_error');
@FOpenSSLProc_ERR_error_string_n := ProcAddress( 'ERR_error_string_n');
@FOpenSSLProc_BN_get_word := ProcAddress('BN_get_word');
@FOpenSSLProc_RSA_up_ref := ProcAddress( 'RSA_up_ref');
@FOpenSSLProc_PEM_write_bio_RSAPrivateKey := ProcAddress( 'PEM_write_bio_RSAPrivateKey');
@FOpenSSLProc_PEM_write_bio_RSAPublicKey := ProcAddress( 'PEM_write_bio_RSAPublicKey');
@FOpenSSLProc_RSA_sign   := ProcAddress( 'RSA_sign');
@FOpenSSLProc_RSA_verify := ProcAddress( 'RSA_verify');
@FOpenSSLProc_RSA_print  := ProcAddress( 'RSA_print');
@FOpenSSLProc_PEM_read_bio_RSAPrivateKey := ProcAddress( 'PEM_read_bio_RSAPrivateKey');
@FOpenSSLProc_BIO_new_mem_buf := ProcAddress( 'BIO_new_mem_buf');
@FOpenSSLProc_PEM_read_bio_RSAPublicKey := ProcAddress( 'PEM_read_bio_RSAPublicKey');

@FOpenSSLProc_EVP_des_ede3_cbc     := ProcAddress( 'EVP_des_ede3_cbc');
@FOpenSSLProc_EVP_des_cbc          := ProcAddress( 'EVP_des_cbc');
@FOpenSSLProc_EVP_idea_cbc         := ProcAddress( 'EVP_idea_cbc');
@FOpenSSLProc_EVP_seed_cbc         := ProcAddress( 'EVP_seed_cbc');
@FOpenSSLProc_EVP_aes_128_cbc      := ProcAddress( 'EVP_aes_128_cbc');
@FOpenSSLProc_EVP_aes_192_cbc      := ProcAddress( 'EVP_aes_192_cbc');
@FOpenSSLProc_EVP_aes_256_cbc      := ProcAddress( 'EVP_aes_256_cbc');
@FOpenSSLProc_EVP_aes_256_ofb      := ProcAddress( 'EVP_aes_256_ofb');
@FOpenSSLProc_EVP_camellia_128_cbc := ProcAddress( 'EVP_camellia_128_cbc');
@FOpenSSLProc_EVP_camellia_192_cbc := ProcAddress( 'EVP_camellia_192_cbc');
@FOpenSSLProc_EVP_camellia_256_cbc := ProcAddress( 'EVP_camellia_256_cbc');

@FOpenSSLProc_EVP_CIPHER_CTX_init    := ProcAddress( 'EVP_CIPHER_CTX_init');
@FOpenSSLProc_EVP_CIPHER_CTX_cleanup := ProcAddress( 'EVP_CIPHER_CTX_cleanup');
@FOpenSSLProc_EVP_CipherInit_ex      := ProcAddress( 'EVP_CipherInit_ex');
@FOpenSSLProc_EVP_CIPHER_CTX_set_key_length := ProcAddress( 'EVP_CIPHER_CTX_set_key_length');
@FOpenSSLProc_EVP_CipherUpdate              := ProcAddress( 'EVP_CipherUpdate');
@FOpenSSLProc_EVP_CipherFinal_ex            := ProcAddress( 'EVP_CipherFinal_ex');
@FOpenSSLProc_EVP_CIPHER_CTX_set_padding    := ProcAddress( 'EVP_CIPHER_CTX_set_padding');

end;

function TOpenSSL_Base.GetRequiredVersionStr: string;
begin
result := Format( '%d.%d.%d.%d',
  [FRqrMajorV, FRqrMinorV, FRqrReleaseV, FRqrBuildV])
end;



function TOpenSSL_Base.isLibraryUniqueInstance: boolean;
var
  j: integer;
  OpenSSL_Lib: TOpenSSL_Base;

begin
result := True;
if FHandle = 0 then exit;
OpenSSL_LibGate.Enter;
try
  for j := 0 to OpenSSL_Libs.Count - 1 do
    begin
    OpenSSL_Lib := OpenSSL_Libs[j] as TOpenSSL_Base;
    result := (OpenSSL_Lib = self) or
              (OpenSSL_Lib.FHandle = 0) or
              (OpenSSL_Lib.FHandle <> FHandle);
    if not result then break
    end
finally
  OpenSSL_LibGate.Leave
  end
end;



function TOpenSSL_Signatory.HasParts: TKeyStoragePartSet;
begin
if assigned( FRSA) then
    result := FKeyStorageParts
  else
    result := []
end;


function ReadString( Source: TStream; const TerminatingPattern: utf8string): utf8string;
var
  Ch: ansichar;
  s: utf8string;
  FoundMatch: boolean;
  ReadCount: integer;
  L: integer;
begin
Ch := #0;
s  := '';
ReadCount := 0;
FoundMatch := False;
L := Length( TerminatingPattern);
if L = 0 then exit;
while (not FoundMatch) and (Source.Read( Ch, 1) = 1) do
  begin
  Inc( ReadCount);
  if Length( s) = L then
      begin
      Move( s[2], s[1], L - 1);
      s[ L] := Ch
      end
    else
      s := s + Ch;
 FoundMatch := s = TerminatingPattern
 end;
if FoundMatch then
    begin
    SetLength( result, ReadCount);
    if ReadCount > 0 then
      begin
      Source.Seek( -ReadCount, soCurrent);
      if Source.Read( result[1], ReadCount) <> ReadCount then
        result := '(Error) Streaming error.'
      end
    end
  else
    result := ''
end;



procedure TOpenSSL_Signatory.LoadRSA_PrivateKey(
  var ErrorMessage: string;
  Cipher: TCipherToEncryptPrivateKeyWith; const sPrivateKey,
  Password: utf8string);
var
  bp: TBIO;
  cb: BN_GENCB;
  Code: integer;
  isException: boolean;
  enc: PEVP_CIPHER;
begin
ErrorMessage := '';
CloseRSA;
if assigned( FOpenSSLProc_PEM_read_bio_RSAPrivateKey) and (sPrivateKey <> '') then
    begin
    FPassword := Password;
    if (FPassword = '') and (Cipher <> cipher_InTheClear) then
      ErrorMessage := 'A non-empty password is required.';
    if (FPassword <> '') and (Cipher = cipher_InTheClear) then
      FPassword := '';
    if ErrorMessage <> '' then
      enc := GetEVP( Cipher);
    if (not assigned( enc)) and (Cipher <> cipher_InTheClear) then
      ErrorMessage := 'Encryption function not found in OpenSSL';

    if ErrorMessage <> '' then exit;

    cb.ver    := 2;
    cb.arg    := pointer( self);
    @cb.cb_2  := @cb_2_SpringBoard;
    isException := False;
    bp := TReadOnlyBIO.Create( self, @sPrivateKey[1], Length( sPrivateKey));
    try
      try
        if FPassword <> '' then
            // The below line was contributed to user Trupka of the LockBox website.
            //  Thanks Trupka.
            FRSA := FOpenSSLProc_PEM_read_bio_RSAPrivateKey( bp.FBIOobj, nil, @pem_password_cb_SpringBoard, Pointer(Self))
          else
            FRSA := FOpenSSLProc_PEM_read_bio_RSAPrivateKey( bp.FBIOobj, nil, nil, nil)
      finally
        bp.Free;
        FPassword := ''
      end
    except on e: Exception do
      begin
      isException := True;
      FRSA := nil;
      ErrorMessage := e.Message
      end;
    end;
    if (not assigned( FRSA)) and (not isException) then
      GetError( Code, ErrorMessage);
    if assigned( FRSA) and (ErrorMessage = '') and (not SignVerify_SelfTest) then
      ErrorMessage := '(Error) TOpenSSL_Signatory.LoadRSA_PrivateKey failed self-test.'
    end
  else if assigned( FOpenSSLProc_PEM_read_bio_RSAPrivateKey) then
      ErrorMessage := '(Error) Private key file corrupt, empty or the wrong type.'
    else
      ErrorMessage := '(Error) TOpenSSL_Signatory not loaded.';

if ErrorMessage = '' then
  FKeyStorageParts := [partPublic, partPrivate]
end;



procedure TOpenSSL_Signatory.LoadRSA_PublicKey(
  var ErrorMessage: string; const sPublicKey: utf8string);
var
  bp: TBIO;
  isException: boolean;
  Code: integer;
begin
ErrorMessage := '';
CloseRSA;

if assigned( FOpenSSLProc_PEM_read_bio_RSAPublicKey) and (sPublicKey <> '') then
    begin
    isException := False;
    bp := TReadOnlyBIO.Create( self, @sPublicKey[1], Length( sPublicKey));
    try
      try
        FRSA := FOpenSSLProc_PEM_read_bio_RSAPublicKey( bp.FBIOobj, nil, nil, nil)
      finally
        bp.Free;
      end
    except on e: Exception do
      begin
      isException := True;
      FRSA := nil;
      ErrorMessage := e.Message
      end;
    end;
    if (not assigned( FRSA)) and (not isException) then
      GetError( Code, ErrorMessage)
    end
  else if assigned( FOpenSSLProc_PEM_read_bio_RSAPublicKey) then
      ErrorMessage := '(Error) Public key file corrupt, empty or the wrong type.'
    else
      ErrorMessage := '(Error) TOpenSSL_Signatory not loaded.';

if ErrorMessage = '' then
  FKeyStorageParts := [partPublic]
end;



//function TOpenSSL_Signatory.NewMemoryBio: TMemoryBIO;
//begin
//if FHandle = 0 then
//    result := nil
//  else
//    result := TMemoryBIO.Create( self)
//end;



procedure TOpenSSL_Base.OpenLib;
begin
OpenSSL_LibGate.Enter;
try
CloseLib;
if TryOpen(
     FLibPath, FLibName, FDLLDir,
     FRqrMajorV, FRqrMinorV, FRqrReleaseV, FRqrBuildV,
     'RSA_generate_key_ex',
     FErr, FHandle, FWindowsError, FFileName, FErrMsg,
     FMajorV, FMinorV, FReleaseV, FBuildV) then
  begin
  GetProcs;
  if not assigned( OpenSSL_Libs) then
    OpenSSL_Libs := TObjectList.Create( False);
  if OpenSSL_Libs.IndexOf( self) = -1 then
    OpenSSL_Libs.Add( self);
  if isLibraryUniqueInstance then
    begin
    SetMemoryManager;
    App_Start
    end
  end;
finally
OpenSSL_LibGate.Leave
end end;




function TOpenSSL_Base.OpenSSL_CallBack( p1, p2: integer): integer;
begin
if (not assigned( FOnProgress)) or FOnProgress( self, p1, p2) then
    result := 1
  else
    result := 0
end;


procedure TOpenSSL_Base.PreShutdown;
begin
end;

function TOpenSSL_Signatory.PEM_Password_CallBack(
  buf: PAnsistring; size, rwflag: longint): longint;
begin
// rwflag is 0 when reading and 1 when writing.
result := Length( FPassword);
if result > size then
  result := size;
if result > 0 then
  Move( FPassword[1], buf^, result)
end;


function TOpenSSL_Signatory.GetPublicKey_AsPEM: utf8string;
var
  PublicKeyBIO: TBIO;
begin
if assigned( FOpenSSLProc_PEM_write_bio_RSAPublicKey) and
   assigned( FRSA) and (partPublic in FKeyStorageParts) then
    begin
    PublicKeyBIO := TMemoryBIO.Create( self);
    try
      FOpenSSLProc_PEM_write_bio_RSAPublicKey( PublicKeyBIO.FBIOobj, FRSA);
      result := PublicKeyBIO.ReadAnsiString
    finally
      PublicKeyBIO.Free
      end
    end
  else
    result := '(Error)'
end;



procedure TOpenSSL_Signatory.PreShutdown;
begin
inherited;
CloseRSA
end;

function TOpenSSL_Signatory.Print: string;
var
  BIO: TBIO;
  ErrorCode: integer;
begin
if assigned( FOpenSSLProc_RSA_print) and assigned( FRSA) then
    begin
    BIO := TMemoryBIO.Create( self);
    try
      if FOpenSSLProc_RSA_print( BIO.FBIOobj, FRSA, 0) = 1 then
          result := BIO.ReadAnsiString  // Coerce system code page ansi into utf16
        else
          GetError( ErrorCode, result)
    finally
      BIO.Free
      end
    end
  else
    result := '(Error) TOpenSSL_Signatory not loaded.'
end;



function TOpenSSL_Signatory.PrivateKey_AsPEM(
  Cipher: TCipherToEncryptPrivateKeyWith;
  const Password: utf8string): utf8string;
var
  Ok: boolean;
  PrivateKeyBIO: TBIO;
  enc: PEVP_CIPHER;
  ErrorCode: integer;
  sResult: string;
begin
result := '';
if assigned( FOpenSSLProc_PEM_write_bio_RSAPrivateKey) and
   assigned( FRSA) and assigned( FOpenSSLProc_EVP_des_ede3_cbc) then
  begin
  FPassword := Password;
  if (FPassword = '') and (Cipher <> cipher_InTheClear) then
    result := '(Error) A non-empty password is required.';
  if (FPassword <> '') and (Cipher = cipher_InTheClear) then
    FPassword := '';
  enc := GetEVP( Cipher);
  if (not assigned( enc)) and (Cipher <> cipher_InTheClear) then
    result := '(Error) Encryption function not found in OpenSSL';
  if result <> '' then exit;

  PrivateKeyBIO := TMemoryBIO.Create( self);
  if FPassword <> '' then
      Ok := FOpenSSLProc_PEM_write_bio_RSAPrivateKey(
         PrivateKeyBIO.FBIOobj, FRSA,
         enc, nil, 0, pem_password_cb_SpringBoard, pointer( self)) = 1
    else
      Ok := FOpenSSLProc_PEM_write_bio_RSAPrivateKey(
         PrivateKeyBIO.FBIOobj, FRSA,
         nil, nil, 0, nil, nil) = 1;
  FPassword := '';
  if Ok then
      result := PrivateKeyBIO.ReadAnsiString
    else
      begin
      GetError( ErrorCode, sResult);
      result := '(Error) ' + sResult
      end;
  PrivateKeyBIO.Free
  end
else
result := '(Error) TOpenSSL_Signatory not loaded.';
end;




procedure TOpenSSL_Signatory.Randomize;
var
  Buffer: TBytes;
  L, RandStatus, j: integer;
begin
if not assigned( FOpenSSLProc_RAND_seed) or
   not assigned( FOpenSSLProc_RAND_status) then exit;
L := 1000;
SetLength( Buffer, L);
for j := 1 to 100 do
  begin
  TRandomStream.Instance.Read( Buffer[0], L);
  FOpenSSLProc_RAND_seed( @Buffer[0], L);
  RandStatus := FOpenSSLProc_RAND_status;
  if RandStatus = 1 then break
  end;
end;


//procedure TOpenSSL_Signatory.RSA_IncRef;
//begin
//if assigned( FRSA) and assigned( FOpenSSLProc_RSA_up_ref) then
//  FOpenSSLProc_RSA_up_ref( FRSA)
//end;

const TypeNumbers: array[ TSigHashKind] of longint = (
  { hshSHA1         ==> } 64,
  { hshRipemd160    ==> } 117,
  { hshMD5          ==> } 4);

procedure TOpenSSL_Signatory.RSA_Sign(
  PreHash: TSigHashKind; Message1: pointer;
  MessageLen: cardinal; var Signature: utf8string;
  var ErrorMessage: string);
var
  SigLen: longword;
  ErrorCode: integer;
begin
Signature    := '';
ErrorMessage := '';
if assigned( FOpenSSLProc_RSA_sign) and assigned( FRSA) then
    begin
    if FCachedSize = -1 then
      FCachedSize := RSA_Size;
    SigLen := (FCachedSize + 7) div 8;
    if SigLen < 2048 then
      SigLen := 2048;
    SetLength( Signature, SigLen);
    if  FOpenSSLProc_RSA_sign( TypeNumbers[PreHash], Message1, MessageLen,
            PAnsiChar( Signature), SigLen, FRSA) = 1 then
        SetLength( Signature, SigLen)
      else
        begin
        Signature := '';
        GetError( ErrorCode, ErrorMessage)
        end;
    end
  else
    ErrorMessage := '(Error) TOpenSSL_Signatory not loaded.'
end;



function TOpenSSL_Signatory.RSA_Size: integer;
var
  s, Prefix: string;
  P, V, Code: integer;
begin
result := 0;
if not assigned( FRSA) then exit;

result := FCachedSize;
if result <> -1 then exit;

result := 0;
if not assigned( FRSA^.n)  then
    begin
    if assigned( FRSA^.d) and assigned( FRSA^.e) then
        begin
        s := Print;
        // 1st line of Print = 'Private-Key: (2048 bit)'
        Prefix := 'Private-Key: (';
        P := Pos( Prefix, s);
        if P > 0 then
          Delete( s, 1, P + Length(Prefix) - 1);
        P := Pos(' bit)',s);
        if P > 0 then
          SetLength( s, P - 1);
        s := Trim( s);
        Val( s, V, Code);
        if (s <> '') and (Code = 0) and (V > 0) then
            result := V
        end
    end
  else
    try
      if assigned( FOpenSSLProc_RSA_size) then
        result := FOpenSSLProc_RSA_size( FRSA)
    except
      result := 0
    end;

if result <> 0 then
  FCachedSize := result
end;



function TOpenSSL_Signatory.RSA_Verify(
  PreHash: TSigHashKind; Message1: pointer;
  MessageLen: cardinal; const Signature: utf8string): boolean;
begin
if assigned( FOpenSSLProc_RSA_verify) and assigned( FRSA) then
    result := FOpenSSLProc_RSA_verify(
                TypeNumbers[PreHash], Message1, MessageLen,
                PAnsiChar( Signature), Length( Signature), FRSA) = 1
  else
    result := False
end;


procedure TOpenSSL_Base.SetDLLDir( const Value: string);
begin
if FDLLDir = Value then exit;
Assert( not GetIsLoaded, '(Error) Do not set SupportDLLDir while the library is loaded.');
FDLLDir := Value
end;


procedure TOpenSSL_Base.SetIsLoaded( Value: boolean);
begin
if (FHandle <> 0) = Value then exit;
if Value then
    begin
    if not FhaveTriedToOpen then
      begin
      FhaveTriedToOpen := True;
      OpenLib
      end;
    end
  else
    CloseLib
end;


procedure TOpenSSL_Base.SetLibName( const Value: string);
var
  sCanonicalValue: string;
begin
sCanonicalValue := Value;
if sCanonicalValue = '' then
  sCanonicalValue := LibEay;
if FLibName = sCanonicalValue then exit;
Assert( not GetIsLoaded, 'Do not set LibName while the library is loaded.');
FLibName := sCanonicalValue
end;

procedure TOpenSSL_Base.SetLibPath(const Value: string);
begin
if FLibName = Value then exit;
Assert( not GetIsLoaded, 'Do not set LibPath while the library is loaded.');
FLibPath := Value
end;


type
  TOpenSSLproc_Malloc = function( Size: longword) : Pointer; cdecl;

function Delphi_Malloc( Size: longword): pointer cdecl;
begin
{$IFDEF HuntingMemoryLeaks}
result := AllocMem( Size + 4);
plongword( result)^ := Size;
result := pointer( cardinal( result) + 4);
Allocated := Allocated + Size
{$ELSE}
result := AllocMem( Size)
{$ENDIF}
end;


type
  TOpenSSLproc_Realloc = function( Ptr: pointer; Size: longword): pointer; cdecl;

function Delphi_Realloc( Ptr: pointer; Size: longword): pointer cdecl;
{$IFDEF HuntingMemoryLeaks}
var
  OldSize: longword;
begin
result := pointer( cardinal( Ptr) - 4);
OldSize := plongword( result)^;
ReallocMem( result, Size);
plongword( result)^ := Size;
result := pointer( cardinal( result) + 4);
Allocated := Allocated - OldSize + Size
end;
{$ELSE}
begin
result := Ptr;
ReallocMem( result, Size);
end;
{$ENDIF}

type
  TOpenSSLproc_Free = procedure( Ptr: pointer); cdecl;

procedure Delphi_Free( Ptr: pointer) cdecl;
{$IFDEF HuntingMemoryLeaks}
var
  RealPtr: pointer;
  OldSize: longword;
begin
RealPtr := pointer( cardinal( Ptr) - 4);
OldSize := plongword( RealPtr)^;
FreeMem( RealPtr);
Allocated := Allocated - OldSize
end;
{$ELSE}
begin
FreeMem( Ptr)
end;
{$ENDIF}

procedure TOpenSSL_Base.SetMemoryManager;
var
  openSSLproc_CRYPTO_set_mem_functions : function(
    m: TOpenSSLproc_Malloc;
    r: TOpenSSLproc_Realloc;
    f: TOpenSSLproc_Free): longint cdecl;
begin
@openSSLproc_CRYPTO_set_mem_functions := GetProcAddress( FHandle, 'CRYPTO_set_mem_functions');
if assigned( openSSLproc_CRYPTO_set_mem_functions) then
  openSSLproc_CRYPTO_set_mem_functions(
    @Delphi_Malloc, @Delphi_Realloc, @Delphi_Free)
end;

procedure TOpenSSL_Base.SetRequiredVersionStr( const Value: string);
var
  s: string;
  isOk1: boolean;
  isEmpty1: boolean;
  RqrMajorV1, RqrMinorV1, RqrReleaseV1, RqrBuildV1: integer;

  procedure ExtractNumberBeforeDot(
    var sDotSeparatedNumbers: string;
    var FirstNumber: integer;
    var isOk: boolean;
    var isEmpty: boolean);

  var
    P: integer;
    sVal: string;
    Code: integer;
  begin
  P := Pos( '.', sDotSeparatedNumbers);
  if P > 0 then
      begin
      sVal := Copy( sDotSeparatedNumbers, 1, P - 1);
      Delete( sDotSeparatedNumbers, 1, P)
      end
    else
      begin
      sVal := sDotSeparatedNumbers;
      sDotSeparatedNumbers := ''
      end;
  isEmpty := sVal = '';
  if isEmpty then
      begin
      FirstNumber := 0;
      isOk := True
      end
    else
      begin
      Val( sVal, FirstNumber, Code);
      isOk := Code = 0;
      if not isOk then
        FirstNumber := 0
      end;
  if isOk and (FirstNumber < 0) then
    begin
    isOk := False;
    FirstNumber := 0
    end
  end;

begin
s := Trim( Value);
RqrMajorV1   := 0;
RqrMinorV1   := 0;
RqrReleaseV1 := 0;
RqrBuildV1   := 0;
ExtractNumberBeforeDot( s, RqrMajorV1, isOk1, isEmpty1);

if isOk1 and (not isEmpty1) and (s <> '') then
  ExtractNumberBeforeDot( s, RqrMinorV1, isOk1, isEmpty1);

if isOk1 and (not isEmpty1) and (s <> '') then
  ExtractNumberBeforeDot( s, RqrReleaseV1, isOk1, isEmpty1);

if isOk1 and (not isEmpty1) and (s <> '') then
  ExtractNumberBeforeDot( s, RqrBuildV1, isOk1, isEmpty1);

if isOk1 and (s <> '') then
  isOk1 := False;

if not isOk1 then
  raise Exception.CreateFmt('"%s" is invalid format for ' +
        'TOpenSSL_Lib.RequiredVersion',[Trim(Value)]);

if (FRqrMajorV   = RqrMajorV1  ) and
   (FRqrMinorV   = RqrMinorV1  ) and
   (FRqrReleaseV = RqrReleaseV1) and
   (FRqrBuildV   = RqrBuildV1  ) then
      exit;

Assert( not GetIsLoaded, 'Do not set RequiredVersion while the library is loaded.');

FRqrMajorV   := RqrMajorV1;
FRqrMinorV   := RqrMinorV1;
FRqrReleaseV := RqrReleaseV1;
FRqrBuildV   := RqrBuildV1
end;



function TOpenSSL_Signatory.SignVerify_SelfTest: boolean;
var
  TestMessage, Signature: utf8string;
  ErrorMsg: string;
  Idx: TSigHashKind;
begin
TestMessage := 'Hello worldHello worldHello worldHello worldHello';
SetLength( TestMessage, 20);
for Idx := Low( TSigHashKind) to High( TSigHashKind) do
  begin
  RSA_Sign( Idx, @TestMessage[1], Length( TestMessage), Signature, ErrorMsg);
  result := (ErrorMsg = '') and
            RSA_Verify( Idx, @TestMessage[1], Length( TestMessage), Signature);
  if not result then break
  end;
end;

{$IF CompilerVersion < 21}
  {$IFDEF UNICODE}
    function SetDllDirectory( lpPathName: PWideChar): BOOL;
      stdcall; external kernel32 name 'SetDllDirectoryW';
  {$ELSE}
    function SetDllDirectory( lpPathName: PAnsiChar): BOOL;
      stdcall; external kernel32 name 'SetDllDirectoryA';
  {$ENDIF}
{$IFEND}

function TOpenSSL_Base.TryOpen( const LibPath, LibName, DLLDir: string;
  RqrMajorV, RqrMinorV, RqrReleaseV, RqrBuildV: integer;
  const RqrSignatureProc: string; var Err: TOpenLibError; var Handle: HMODULE;
  var WindowsError: DWORD; var FileName, ErrMsg: string; var MajorV, MinorV,
  ReleaseV, BuildV: integer): boolean;
var
  iLibSize, iValueSize: DWord;
  Buf: ansiString;
  Ok: boolean;
  fvip: pointer;

begin
Err := erNull;
Handle := 0;
WindowsError := 0;
FileName := LibName;
if LibPath <> '' then
  FileName := IncludeTrailingPathDelimiter( LibPath) + FileName;
ErrMsg := '';
MajorV   := 0;
MinorV   := 0;
ReleaseV := 0;
BuildV   := 0;
{$IFDEF WIN64} {$IFDEF OpenSSL_Win64_NotYetSupported}
  result := False;
  Err    := erPlatformNotSupported;
  ErrMsg := 'Win64 platform not yet supported by TOpenSSL_Signatory.';
  exit;
{$ENDIF} {$ENDIF}
{$IFDEF OSX32}
  result := False;
  Err    := erPlatformNotSupported;
  ErrMsg := 'OSX platform not supported by TOpenSSL_Signatory.';
  exit;
{$ENDIF}

if (DLLDir <> '') and (not SetDllDirectory( PChar( DLLDir))) then
  begin
  Err := erSetDLLDirectory;
  WindowsError := GetLastError;
  end;
if Err = erNull then
  try
    Handle := SysUtils.safeloadlibrary( FileName, SEM_FAILCRITICALERRORS  +
                                                  SEM_NOGPFAULTERRORBOX   +
                                                  SEM_NOOPENFILEERRORBOX);
    if Handle = 0 then
      begin
      Err := erLoadFailed;
      WindowsError := GetLastError
      end;
  except on e: exception do
    begin
    Err := erException;
    Handle := 0;
    ErrMsg := Format( '%s: %s', [e.ClassName, e.Message])
    end;
  end;
if (DLLDir <> '') and (Err <> erSetDLLDirectory) then
  SetDllDirectory(nil);
if Handle <> 0 then
  FileName := sysutils.GetModuleName( Handle);
if (Err in [erSetDLLDirectory, erLoadFailed]) and (WindowsError <> 0) then
  ErrMsg := SysUtils.SysErrorMessage( WindowsError);
result := Err = erNull;

if result then
  begin
  iLibSize := GetFileVersionInfoSize( PChar( FileName), iLibSize);
  Ok := iLibSize > 0;
  if Ok then
    begin
    SetLength( Buf, iLibSize);
    Ok := GetFileVersionInfo( PChar( FileName), 0, iLibSize, PAnsiChar( Buf)) and
          VerQueryValue( PAnsiChar( Buf), '\', fvip, iValueSize) and
          (iValueSize >= SizeOf( TVSFixedFileInfo))
    end;
  if Ok then
    begin
    MajorV   := HiWord( TVSFixedFileInfo( fvip^).dwFileVersionMS);
    MinorV   := LoWord( TVSFixedFileInfo( fvip^).dwFileVersionMS);
    ReleaseV := HiWord( TVSFixedFileInfo( fvip^).dwFileVersionLS);
    BuildV   := LoWord( TVSFixedFileInfo( fvip^).dwFileVersionLS)
    end
  end;
if result and (
   (MajorV < RqrMajorV) or
  ((MajorV = RqrMajorV) and (MinorV < RqrMinorV)) or
  ((MajorV = RqrMajorV) and (MinorV = RqrMinorV) and (ReleaseV < RqrReleaseV)) or
  ((MajorV = RqrMajorV) and (MinorV = RqrMinorV) and (ReleaseV < RqrReleaseV) and (BuildV < RqrBuildV))) then
  begin
  Err := erVersionTooLow;
  result := False
  end;

if result and (RqrSignatureProc <> '') and (not assigned(
  windows.GetProcAddress( Handle, PChar( RqrSignatureProc)))) then
  begin
  Err := erSignatureAbsent;
  result := False
  end;

if (Err in [erVersionTooLow, erSignatureAbsent]) and (Handle <> 0) then
  begin
  FreeLibrary( Handle);
  Handle := 0
  end;
end;

procedure TOpenSSL_Signatory.StoreKeysToStream(
  Store: TStream; Parts: TKeyStoragePartSet);
var
  sPrivateKey, sPublicKey: utf8string;

begin
if partPrivate in Parts then
    begin
    sPrivateKey := PrivateKey_AsPEM( FStorageCipher, FStoragePassword);
    if Pos( '(Error) ', sPrivateKey) > 0 then
      begin
      FErrMsg := sPrivateKey;
      raise Exception.Create( FErrMsg)
      end;
    if sPrivateKey <> '' then
      Store.Write( sPrivateKey[1], Length( sPrivateKey))
    end

  else if partPublic in Parts then
    begin
    sPublicKey := PublicKey;
    if Pos( '(Error) ', sPublicKey) > 0 then
      begin
      FErrMsg := sPublicKey;
      raise Exception.Create( FErrMsg)
      end;
    if sPublicKey <> '' then
      Store.Write( sPublicKey[1], Length( sPublicKey))
    end

  else begin end
end;

procedure TOpenSSL_Signatory.LoadKeysFromStream(
  Store: TStream; Parts: TKeyStoragePartSet);
var
  ErrorMessage: string;
begin
if partPrivate in Parts then
    begin
    LoadRSA_PrivateKey( ErrorMessage, FStorageCipher, ReadString( Store,
      '-----END RSA PRIVATE KEY-----'#10), FStoragePassword);
    if ErrorMessage = '' then exit;
    FErrMsg := ErrorMessage;
    raise Exception.Create( FErrMsg)
    end

  else if partPublic in Parts then
    begin
    LoadRSA_PublicKey( ErrorMessage, ReadString( Store,
      '-----END RSA PUBLIC KEY-----'#10));
    if ErrorMessage = '' then exit;
    FErrMsg := ErrorMessage;
    raise Exception.Create( FErrMsg)
    end

  else begin end
end;

function TOpenSSL_Signatory.Sign(
  PreHash: TSigHashKind; HashedDocument, Signature: TStream): boolean;
var
  Message1: pointer;
  MessageLen: cardinal;
  sSignature: utf8string;
  ErrorMessage: string;
  MemStream: TMemoryStream;
begin
MessageLen := HashedDocument.Size - HashedDocument.Position;
if HashedDocument is TMemoryStream then
    begin
    Message1   := TMemoryStream( HashedDocument).Memory;
    MemStream  := nil
    end
  else
    begin
    MemStream := TMemoryStream.Create;
    MemStream.Size := MessageLen;
    MemStream.Position := 0;
    MemStream.CopyFrom( HashedDocument, MessageLen);
    Message1 := MemStream.Memory
    end;
try
  RSA_Sign( PreHash, Message1, MessageLen, sSignature, ErrorMessage);
finally
  MemStream.Free
  end;
result := (ErrorMessage = '') and (sSignature <> '');
if result then
    Signature.Write( sSignature[1], Length( sSignature))
  else if ErrorMessage <> '' then
    FErrMsg := ErrorMessage
  else
    FErrMsg := '(Error) Unknown error'
end;


function TOpenSSL_Signatory.Verify(
  PreHash: TSigHashKind; HashedDocument, Signature: TStream): TVerifyResult;
var
  Message1: pointer;
  MessageLen: cardinal;
  sSignature: utf8string;
  MemStream: TMemoryStream;
  SigLen: integer;
begin
MessageLen := HashedDocument.Size - HashedDocument.Position;
if HashedDocument is TMemoryStream then
    begin
    Message1   := TMemoryStream( HashedDocument).Memory;
    MemStream  := nil
    end
  else
    begin
    MemStream := TMemoryStream.Create;
    MemStream.Size := MessageLen;
    MemStream.Position := 0;
    MemStream.CopyFrom( HashedDocument, MessageLen);
    Message1 := MemStream.Memory
    end;
try
  SigLen := Signature.Size - Signature.Position;
  SetLength( sSignature, SigLen);
  if SigLen > 0 then
    Signature.Read( sSignature[1], SigLen);
  if RSA_Verify( PreHash, Message1, MessageLen, sSignature) then
      result := vPass
    else
      result := vFail
finally
  MemStream.Free
  end
end;


function TOpenSSL_Signatory.GetPrivateKey_AsPEM: utf8string;
begin
if partPrivate in FKeyStorageParts then
    result := PrivateKey_AsPEM( FStorageCipher, FStoragePassword)
  else
    result := '(Error) No private key'
end;


procedure TOpenSSL_Signatory.SetPrivateKey_AsPEM( const Value: utf8string);
var
  Err: string;
begin
LoadRSA_PrivateKey( Err, FStorageCipher, Value, FStoragePassword);
if Err <> '' then
  begin
  FErrMsg := Err;
  raise Exception.Create( FErrMsg)
  end
end;


procedure TOpenSSL_Signatory.SetPublicKey_AsPEM( const Value: utf8string);
var
  Err: string;
begin
LoadRSA_PublicKey( Err, Value);
if Err <> '' then
  begin
  FErrMsg := Err;
  raise Exception.Create( FErrMsg)
  end
end;



//procedure TOpenSSL_Signatory.OpenSSL_Encrypt( const key, iv: TBytes; Cipher: TCipherToEncryptPrivateKeyWith; InStream, OutStream: TStream);
//var
//  Key: rawbytestring;
//  IV : rawbytestring;
//  ctx: EVP_CIPHER_CTX;
//  ret: integer;
//  s: string;
//  j: integer;
//  outbuf, inbuf: TBytes;
//  inLen, outLen: Integer;
//  Ok: boolean;
//begin
//Key := '1234567812345678';
//IV  := '1234567812345678';
//SetLength(  inbuf, Length( Key));
//SetLength( outbuf, Length( Key));

//  FOpenSSLProc_EVP_CipherInit_ex( ctx, GetEVP( cipher_aes_256_ofb), nil, nil, nil, 1);
//  FOpenSSLProc_EVP_CIPHER_CTX_set_key_length( ctx, Length( Key));
//  FOpenSSLProc_EVP_CipherInit_ex( ctx, nil, nil, @Key[1], @IV[1], 1);
//  for j := 1 to 10 do
//    begin
//    inLen  := Length( inBuf);
//    outLen := Length( outBuf);
//    Ok := FOpenSSLProc_EVP_CipherUpdate( ctx, @outbuf[1], outlen, @inBuf[1], inlen) <> 0;
//    if not Ok then break;
//    s := '1';
//    end;
//  outLen := Length( outBuf);
//  Ok := FOpenSSLProc_EVP_CipherFinal_ex( ctx, @outbuf[1], outLen) <> 0;

//end;

{ TOpenSSL_Codec }

constructor TOpenSSL_Codec.Create( AOwner: TComponent);
begin
inherited Create( AOwner);
FCipher := cipher_aes_256_ofb;
SetLength( FKey, 0);
SetLength( FIV , 0);
FisInitialised := False;
FPad := padPKCS;
end;

destructor TOpenSSL_Codec.Destroy;
begin
if isLoaded and FisInitialised then
  begin
  FisInitialised := False;
  FOpenSSLProc_EVP_CIPHER_CTX_cleanup( Fctx); // Equivalent to a burn.
  end;
inherited
end;

procedure TOpenSSL_Codec.SetKey( const key1: TBytes);
begin
SetLength( FKey, Length( key1));
if Length( key1) > 0 then
  Move( key1[1], FKey[1], Length( key1));
end;

procedure TOpenSSL_Codec.SetIV( const iv1: TBytes);
begin
SetLength( FIV, Length( iv1));
if Length( iv1) > 0 then
  Move( iv1[1], FIV[1], Length( iv1));
end;

procedure TOpenSSL_Codec.BeginEncDec( doEncrypt: Boolean);
var
  EncValue: integer;
  iPadParm: integer;
begin
if not FisInitialised then
  begin
  FisInitialised := True;
  FOpenSSLProc_EVP_CIPHER_CTX_init( Fctx);
  end;
FOpenSSLProc_EVP_CipherInit_ex( Fctx, GetEVP( FCipher), nil, nil, nil, 1);
FOpenSSLProc_EVP_CIPHER_CTX_set_key_length( Fctx, Length( FKey));
if doEncrypt then
    EncValue := 1
  else
    EncValue := 0;
FOpenSSLProc_EVP_CipherInit_ex( Fctx, nil, nil, @FKey[1], @FIV[1], EncValue);
iPadParm := Ord( FPad);
FOpenSSLProc_EVP_CIPHER_CTX_set_padding( Fctx, iPadParm);
end;

procedure TOpenSSL_Codec.Encrypt(
  PlaintextInStream, CiphertextOutStream: TStream);
var
  InBuf, OutBuf: TBytes;
  InLen, OutLen: Integer;
  Ok: boolean;
begin
BeginEncDec( True);
SetLength( InBuf , Length( FIV));
SetLength( OutBuf, Length( FIV) * 2);
Ok := True;
repeat
  InLen  := PlaintextInStream.Read( InBuf[1], Length( InBuf));
  OutLen := Length( OutBuf);
  If InLen = 0 then break;
  if InLen < Length( InBuf) then
    begin
    case FPad of
      padNone:
        begin

        end;

      padPKCS:
        begin

        end;

      end;
    end;
  Ok := FOpenSSLProc_EVP_CipherUpdate( Fctx, @outbuf[1], outlen, @inBuf[1], inlen) <> 0;
  if Ok and (OutLen > 0) then
    CiphertextOutStream.Write( OutBuf[1], OutLen)
until (InLen < Length( InBuf)) or (not Ok);
if Ok then
    begin
    OutLen := Length( outBuf);
    Ok := FOpenSSLProc_EVP_CipherFinal_ex( Fctx, @Outbuf[1], OutLen) <> 0;
    if Ok and (OutLen > 0) then
      CiphertextOutStream.Write( OutBuf[1], OutLen)
    end
  else
    raise Exception.Create( 'OpenSSL encryption error')
end;

procedure TOpenSSL_Codec.Decrypt(
  PlaintextOutStream, CiphertextInStream: TStream);
const
  MaxTries = 2;
var
  InBuf, OutBuf: TBytes;
  InLen, OutLen: Integer;
  Ok: boolean;
  PlainPos, CipherPos, CipherSz: Int64;
  Trial: integer;
begin
PlainPos  := PlaintextOutStream.Position;
CipherPos := CiphertextInStream.Position;
CipherSz  := CiphertextInStream.Size;
for Trial := 1 to MaxTries do
  begin
  BeginEncDec( False);
  SetLength( InBuf , Length( FIV));
  SetLength( OutBuf, Length( FIV) * 2);
  Ok := True;
  repeat
    InLen  := CiphertextInStream.Read( InBuf[1], Length( InBuf));
    OutLen := Length( OutBuf);
    If InLen = 0 then break;
    Ok := FOpenSSLProc_EVP_CipherUpdate( Fctx, @outbuf[1], outlen, @inBuf[1], inlen) <> 0;
    if Ok and (OutLen > 0) then
      PlaintextOutStream.Write( OutBuf[1], OutLen)
  until (InLen < Length( InBuf)) or (not Ok);
  if Ok then
      begin
      OutLen := Length( outBuf);
      Ok := FOpenSSLProc_EVP_CipherFinal_ex( Fctx, @Outbuf[1], OutLen) <> 0;
      if Ok and (OutLen > 0) then
        PlaintextOutStream.Write( OutBuf[1], OutLen)
      end;
  if Ok then break;
  if Trial < MaxTries then
      begin
      PlaintextOutStream.Position := PlainPos;
      CiphertextInStream.Position := CipherPos;
      CiphertextInStream.Size     := CipherSz;
      FisInitialised              := False;
      FOpenSSLProc_EVP_CIPHER_CTX_cleanup( Fctx)
      end
    else
      raise Exception.Create( 'OpenSSL encryption error')
  end
end;

initialization
InitUnit_OpenSSL_Lib;

finalization
DoneUnit_OpenSSL_Lib
end.
