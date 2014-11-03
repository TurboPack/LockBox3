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

unit uTPLb_I18n;

interface

resourcestring

// Exception messages
ES_NoPassword = 'TSimpleCodec.Init - No password.';
ES_AsymPassword = 'TSimpleCodec.Init - Asymetric codecs are not initialized by string.';
ES_HashFailed = 'TSimpleCodec.Init - Hash failed.';
ES_WrongModeReset = 'TSimpleCodec.Init - Reset when not intialized.';
ES_WrongModeEncrypt = 'TSimpleCodec.Begin_EncryptMemory - Wrong mode.';
ES_EncryptNoAlg = 'TSimpleCodec.Begin_EncryptMemory - Algorithms not set.';
ES_WrongModeDecrypt = 'TSimpleCodec.Begin_DecryptMemory - Wrong mode.';
ES_DecryptNoAlg = 'TSimpleCodec.Begin_DecryptMemory - Algorithms not set.';
ES_WrongModeEncryptMem = 'TSimpleCodec.EncryptMemory - Wrong mode.';
ES_WrongModeDecryptMem = 'TSimpleCodec.DecryptMemory - Wrong mode.';
ES_WrongModeEndDecryptMem = 'TSimpleCodec.End_DecryptMemory - Wrong mode.';
ES_WrongModeEndEncryptMem = 'TSimpleCodec.End_EncryptMemory - Wrong mode.';
ES_Asym_CannotSetParam = 'TSimpleCodec.AsymetricKeySizeInBits - Cannot set parameter whilst enc/decrypting.';
ES_Asym_CannotSetCipher = 'TSimpleCodec.Init - Cannot set Cipher whilst enc/decrypting.';
ES_AsymNotInitByStr = 'TSimpleCodec.Init - Asymetric codecs are not initialized by string.';
ES_NotImplementedNot_Suffix = '(not implemented yet!)';
ES_LibsChainTooDeep = 'TCryptographicLibrarys cannot chain more than %d deep.';
ES_CircularLibs = 'Circular TCryptographicLibrary chaining is prohibited.';
ES_HugeCardinal_CloneOverflow = 'THugeCardinal.CreateAsSizedClone overflow.';
ES_HugeCardinal_AddOverflow = 'THugeCardinal.Add overflow.';
ES_CannotAssignHuge_BecauseSourceTooBig =
  'Cannot assign THugeCardinal because source is too big' +
  ' and resizing is not allowed.';
ES_HugeCardinal_IncrementOverflow = 'THugeCardinal.Increment overflow.';
ES_HugeCardinal_DivideOverflow = 'THugeCardinal.Divide overflow.';
ES_HugeCardinal_Power2 = 'MulPower2 overflow.';
ES_HugeCardinal_MulSmallOverflow = 'MulSmall overflow.';
ES_HugeCardinal_ResizeOverflow = 'THugeCardinal.Resize overflow';
ES_HugeCardinal_PowerOverflow = 'Power overflow';
ES_HugeCardinal_SubractOverflow = 'THugeCardinal.Subtract overflow.';
ES_HugeCardinal_StreamOutOverflow = 'THugeCardinal.StreamOut overflow.';
ES_HugeCardinal_StreamInOverflow = 'THugeCardinal.StreamIn overflow.';
ES_EratosthenesSievSizeTooSmall =
      'EratosthenesSieveSize value (%d) too small for required number of ' +
      'pre-computed small primes (%d primes)';
ES_InternalError_Suffix = 'internal error.';
ES_InvalidKey = 'Invalid key.';
ES_PlaintextFN_Empty = 'TSimpleCodec.EncryptFile - plaintext filename parameter was empty.';
ES_CiphertextFN_Empty = 'TSimpleCodec.EncryptFile - ciphertext filename parameter was empty.';
ES_NoOpenSSLCompat = 'Cipher %s does not support OpenSSL Compatibility mode.';
ES_OpenSSLCompat_RequiresIV = 'OpenSSL Compability mode requires an explicit IV (except ECB mode).';


// Assert failure messages
AS_HugeCardinal_DivideLogicFail = 'THugeCardinal.Divide internal logic failure.';
AS_ZeroToZero = 'Zero raised to zero is an invalid operation.';
AS_LCM_Fail = 'function lcm failed.';

AS_HugeCardinal_N_tooSmall =  'RequiredBitLengthOfN too small.';
AS_HugeCardinal_N_tooBig =  'RequiredBitLengthOfN absurdly large.';
AS_HugeCardinal_N_tooSmall_for_n = 'RequiredBitLengthOfN too small for n.';

AS_BlockToStream_EndEncrypt_InternalMarshalling =
  'TNoncibleEncryptor.End_Encrypt - Internal marshalling error.';
AS_BlockToStream_EndDecrypt_InternalMarshalling =
  'TNoncibleDecryptor.End_Encrypt - Internal marshalling error.';

AS_BlockPaddingCorrupt = 'Invalid ciphertext - block padding is corrupted.';

// SmartInspect
{$IFDEF SI};
SIS_Hello = 'Hello';
{$ENDIF}


// Algorithmic Display Names
RSA_DisplayName = 'RSA public key encryption system';
BlockCipher_DisplayName = 'Block mode';
Base64_DisplayName = 'Base64';
CFB8bit_DisplayName = 'CFB 8-bit';
ECB_DisplayName = 'ECB (with block padding)';


// Design-time messages
DS_Email = 'email: ';
DS_Web = 'web: ';
DS_RunTimeIs = 'Run-time package %s is version %s .';
DS_DesignTimeIs = 'Design-time package %s is version %s .';
DS_HashNotSelected = '(THash not selected)';
DS_BlockSizeEqs = 'Block size = %d bits';
DS_DigestSizeEqs = 'Digest size = %d bits';
DS_ChainModeNotSelected = '(Chain mode not selected)';
DS_CodecNotSelected = '(TCodec not selected)';
DS_MixedLibraries = '(Libraries mixed or not set)';


implementation

end.
