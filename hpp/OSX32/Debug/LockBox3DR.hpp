// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'LockBox3DR.dpk' rev: 30.00 (MacOS)

#ifndef Lockbox3drHPP
#define Lockbox3drHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// (rtl)
#include <SysInit.hpp>
#include <uTPLb_BlockCipher.hpp>
#include <uTPLb_StreamUtils.hpp>
#include <uTPLb_StreamCipher.hpp>
#include <uTPLb_CryptographicLibrary.hpp>
#include <uTPLb_BaseNonVisualComponent.hpp>
#include <uTPLb_Codec.hpp>
#include <uTPLb_ECB.hpp>
#include <uTPLb_CBC.hpp>
#include <uTPLb_PCBC.hpp>
#include <uTPLb_CFB_Block.hpp>
#include <uTPLb_CFB_8Bit.hpp>
#include <uTPLb_OFB.hpp>
#include <uTPLb_CTR.hpp>
#include <uTPLb_HashDsc.hpp>
#include <uTPLb_SHA1.hpp>
#include <uTPLb_BinaryUtils.hpp>
#include <uTPLb_Hash.hpp>
#include <uTPLb_MD5.hpp>
#include <uTPLb_Random.hpp>
#include <uTPLb_StreamToBlock.hpp>
#include <uTPLb_Base64.hpp>
#include <uTPLb_CipherUtils.hpp>
#include <uTPLb_AES.hpp>
#include <uTPLb_PointerArithmetic.hpp>
#include <uTPLb_HugeCardinal.hpp>
#include <uTPLb_IntegerUtils.hpp>
#include <uTPLb_HugeCardinalUtils.hpp>
#include <uTPLb_MemoryStreamPool.hpp>
#include <uTPLb_RSA_Primitives.hpp>
#include <uTPLb_RSA_Engine.hpp>
#include <uTPLb_Asymetric.hpp>
#include <uTPLb_Signatory.hpp>
#include <uTPLb_CodecIntf.hpp>
#include <uTPLb_Constants.hpp>
#include <uTPLb_I18n.hpp>
#include <uTPLb_SimpleBlockCipher.hpp>
#include <uTPLb_DES.hpp>
#include <uTPLb_3DES.hpp>
#include <uTPLb_BlowFish.hpp>
#include <uTPLb_Decorators.hpp>
#include <DCPtwofish_LB3Modified.hpp>
#include <uTPLb_TwoFish.hpp>
#include <uTPLb_XXTEA.hpp>
#include <uTPLb_SHA2.hpp>
#include <uTPLb_SVN_Keywords.hpp>
#include <uTPLb_StrUtils.hpp>
#include <System.Types.hpp>	// (rtl)
#include <Posix.Base.hpp>	// (rtl)
#include <Posix.Dlfcn.hpp>	// (rtl)
#include <Posix.StdDef.hpp>	// (rtl)
#include <Posix.SysTypes.hpp>	// (rtl)
#include <Posix.Fcntl.hpp>	// (rtl)
#include <Posix.SysStat.hpp>	// (rtl)
#include <Posix.Signal.hpp>	// (rtl)
#include <Posix.Time.hpp>	// (rtl)
#include <Posix.SysTime.hpp>	// (rtl)
#include <System.Internal.Unwinder.hpp>	// (rtl)
#include <Macapi.Mach.hpp>	// (rtl)
#include <Macapi.CoreServices.hpp>	// (rtl)
#include <Macapi.CoreFoundation.hpp>	// (rtl)
#include <System.SysConst.hpp>	// (rtl)
#include <Posix.Iconv.hpp>	// (rtl)
#include <Posix.Dirent.hpp>	// (rtl)
#include <Posix.Errno.hpp>	// (rtl)
#include <Posix.Fnmatch.hpp>	// (rtl)
#include <Posix.Locale.hpp>	// (rtl)
#include <Posix.Langinfo.hpp>	// (rtl)
#include <Posix.Sched.hpp>	// (rtl)
#include <Posix.Pthread.hpp>	// (rtl)
#include <Posix.Stdio.hpp>	// (rtl)
#include <Posix.Stdlib.hpp>	// (rtl)
#include <Posix.String_.hpp>	// (rtl)
#include <Posix.SysSysctl.hpp>	// (rtl)
#include <Posix.Unistd.hpp>	// (rtl)
#include <Posix.Utime.hpp>	// (rtl)
#include <Posix.Wordexp.hpp>	// (rtl)
#include <Posix.Pwd.hpp>	// (rtl)
#include <Posix.SysUio.hpp>	// (rtl)
#include <Posix.SysSocket.hpp>	// (rtl)
#include <Posix.ArpaInet.hpp>	// (rtl)
#include <Posix.NetinetIn.hpp>	// (rtl)
#include <System.RTLConsts.hpp>	// (rtl)
#include <Posix.Wchar.hpp>	// (rtl)
#include <Posix.Wctype.hpp>	// (rtl)
#include <System.Character.hpp>	// (rtl)
#include <System.Internal.MachExceptions.hpp>	// (rtl)
#include <System.Internal.ExcUtils.hpp>	// (rtl)
#include <System.SysUtils.hpp>	// (rtl)
#include <System.VarUtils.hpp>	// (rtl)
#include <System.Variants.hpp>	// (rtl)
#include <System.StrUtils.hpp>	// (rtl)
#include <System.AnsiStrings.hpp>	// (rtl)
#include <System.Hash.hpp>	// (rtl)
#include <System.Math.hpp>	// (rtl)
#include <System.Generics.Defaults.hpp>	// (rtl)
#include <System.Generics.Collections.hpp>	// (rtl)
#include <Posix.SysMman.hpp>	// (rtl)
#include <System.Internal.Unwind.hpp>	// (rtl)
#include <System.Rtti.hpp>	// (rtl)
#include <System.TypInfo.hpp>	// (rtl)
#include <Posix.StrOpts.hpp>	// (rtl)
#include <Posix.SysSelect.hpp>	// (rtl)
#include <Macapi.ObjCRuntime.hpp>	// (rtl)
#include <System.Classes.hpp>	// (rtl)
#include <Posix.Semaphore.hpp>	// (rtl)
#include <System.TimeSpan.hpp>	// (rtl)
#include <System.Diagnostics.hpp>	// (rtl)
#include <System.SyncObjs.hpp>	// (rtl)

//-- user supplied -----------------------------------------------------------

namespace Lockbox3dr
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
}	/* namespace Lockbox3dr */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_LOCKBOX3DR)
using namespace Lockbox3dr;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Lockbox3drHPP
