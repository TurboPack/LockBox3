unit uInnoStringSupport;
interface

// The following functions are for use as exported to Inno-Unicode script.
//  This is only valid and tested for Inno version 5.4.0(u) .
// All the methods relating to ansistrings include the correct handling
//  of Far-Eastern syste code pages. In other words, ansistring may validly
//  be MBCS.

type
  PUTF8Char = PAnsiChar;

function Convert_Ansi_To_UTF8( AnsiValue: PAnsiChar): PUTF8Char; stdcall;
// Converts ansistring in system code page to UTF-8 string.

function Convert_UTF8_To_Ansi( UTF8Value: PUTF8Char): PAnsiChar; stdcall;
// Converts UTF-8 string to ansistring in system code page.

function Convert_UTF16_To_Ansi( UTF16Value: PWideChar): PAnsiChar; stdcall;
// Converts UTF-16 string to ansistring in system code page.

function Convert_Ansi_To_UTF16_Length( AnsiValue: PAnsiChar): integer; stdcall;
// Converts ansistring in system code page to UTF-16 string, which is
//  held locally. Returns the length in characters of the UTF-16 string.

function Convert_Ansi_To_UTF16_IndexedChar( CharIndex: integer): widechar; stdcall;
// Subsequent to a call to Convert_Ansi_To_UTF16_Length, return the indexed
//  UTF-16 character. The index physical origin is 1.
//  Out of range indicies will return the nil character.

function Convert_UTF16_To_UTF8( UTF16Value: PWideChar): PUTF8Char; stdcall;
// Convert a UTF-16 string to a UTF-8 string.

function Convert_UTF8_To_UTF16_Length( UTF8Value: PUTF8Char): integer; stdcall;
// Converts a UTF-8 string to UTF-16 string, which is
//  held locally. Returns the length in characters of the UTF-16 string.

function Convert_UTF8_To_UTF16_IndexedChar( CharIndex: integer): widechar; stdcall;
// Subsequent to a call to Convert_UTF8_To_UTF16_Length, return the indexed
//  UTF-16 character. The index physical origin is 1.
//  Out of range indicies will return the nil character.

function Convert_Oem_To_Ansi( OemValue: PAnsiChar): PAnsiChar; stdcall;
// Converts OEM ansi (CodePage 447) to system codepage ansi.


function ReadAndXOR_GlobalOptions( XOR_Prism: longword): longword; stdcall;


var
  GlobalOptions: longword;
  doWorkAround_Inno5_4_0u_ReturnAnsiString_Bug: boolean;

const
  OPTIONFLAG_WorkAroundReturnAnsiStringBug = 1;
  OPTIONFLAG_Diagnostics = 2;

implementation












uses Windows, SysUtils, StrUtils, uDiagnostics;
var
  UTF8Hold: UTF8string;
  AnsiHold, AnsiHold2: Ansistring;
  Ansi_To_UTF16_Hold: unicodestring;
  UTF8_To_UTF16_Hold: unicodestring;


{$IFNDEF UNICODE} Stop! Wrong compiler. {$ENDIF}
{$WARNINGS OFF}

procedure AdjustForBug( var FunctionReturnValue: ansistring); overload;
// Only required for function return parameters of type ansistring.
// Not required for procedure/function list parameters.
const
  DummyChar: ansichar = 'X';
var
  L: integer;
begin
if not doWorkAround_Inno5_4_0u_ReturnAnsiString_Bug then exit;
L := Length( FunctionReturnValue);
if L = 0 then exit;
Inc( L);
SetLength( FunctionReturnValue, L);
FunctionReturnValue[L] := DummyChar
end;


procedure AdjustForBug( var FunctionReturnValue: utf8string); overload;
// Only required for function return parameters of type ansistring.
// Not required for procedure/function list parameters.
const
  DummyChar: ansichar = 'X';
var
  L: integer;
begin
if not doWorkAround_Inno5_4_0u_ReturnAnsiString_Bug then exit;
L := Length( FunctionReturnValue);
if L = 0 then exit;
Inc( L);
SetLength( FunctionReturnValue, L);
FunctionReturnValue[L] := DummyChar
end;



function Convert_Ansi_To_UTF8( AnsiValue: PAnsiChar): PUTF8Char;
begin
// Internally, this first converts the ansi to UTF-16,
//  using the system call MultiByteToWideChar and CodePage assumption
//  of  System.DefaultSystemCodePage.
// The second step then calls WideCharToMultiByte with a Codepage of CP_UTF8.
UTF8Hold := System.AnsiToUtf8( AnsiValue);
AdjustForBug( UTF8Hold);
result   := PUTF8Char( UTF8Hold)
// The Hold is given a CodePage of CP_UTF8
end;


function Convert_UTF8_To_Ansi( UTF8Value: PUTF8Char): PAnsiChar;
// Converts UTF8 to UTF16 and then back to ansi with a codepage of
//  System.DefaultSystemCodePage
begin
AnsiHold2 := System.Utf8ToAnsi( UTF8Value);
AdjustForBug( AnsiHold2);
result   := PAnsiChar( AnsiHold2)
end;


function Convert_UTF16_To_Ansi( UTF16Value: PWideChar): PAnsiChar;
var
  s: unicodestring;
begin
s := UTF16Value;
AnsiHold := s;
AdjustForBug( AnsiHold);
result := PAnsiChar( AnsiHold)
end;


function Convert_Ansi_To_UTF16_Length( AnsiValue: PAnsiChar): integer;
var
  s: ansistring;
begin
s := AnsiValue;
Ansi_To_UTF16_Hold := s;
result := Length( Ansi_To_UTF16_Hold)
end;


function Convert_Ansi_To_UTF16_IndexedChar( CharIndex: integer): widechar;
begin
if (CharIndex > 0) and (CharIndex <= Length( Ansi_To_UTF16_Hold)) then
    result := Ansi_To_UTF16_Hold[ CharIndex]
  else
    result := #0
end;


function Convert_UTF16_To_UTF8( UTF16Value: PWideChar): PUTF8Char;
begin
UTF8Hold := System.UTF8Encode( UTF16Value);
AdjustForBug( UTF8Hold);
result   := PUTF8Char( UTF8Hold)
end;


function Convert_UTF8_To_UTF16_Length( UTF8Value: PUTF8Char): integer;
begin
UTF8_To_UTF16_Hold := System.UTF8ToString( UTF8Value);
result := Length( UTF8_To_UTF16_Hold)
end;


function Convert_UTF8_To_UTF16_IndexedChar( CharIndex: integer): widechar;
begin
if (CharIndex > 0) and (CharIndex <= Length( UTF8_To_UTF16_Hold)) then
    result := UTF8_To_UTF16_Hold[ CharIndex]
  else
    result := #0
end;



function Convert_Oem_To_Ansi( OemValue: PAnsiChar): PAnsiChar; stdcall;
var
  sSystemAnsi: ansistring;
  L: integer;
begin
L := StrLen( OemValue);
SetLength( sSystemAnsi, L);
Windows.OemToCharBuffA( OemValue, PAnsiChar( sSystemAnsi), L);
AnsiHold := sSystemAnsi;
AdjustForBug( AnsiHold);
result := PAnsiChar( AnsiHold)
end;


function hasOption( Flag: longword): boolean;
begin
result := (GlobalOptions and Flag) = Flag
end;


procedure UpdateGlobalOptions;
begin
doWorkAround_Inno5_4_0u_ReturnAnsiString_Bug :=
  hasOption( OPTIONFLAG_WorkAroundReturnAnsiStringBug);
SetDiagLogOn( hasOption( OPTIONFLAG_Diagnostics))
end;


function ReadAndXOR_GlobalOptions( XOR_Prism: longword): longword;
begin
result := GlobalOptions xor XOR_Prism;
GlobalOptions := result;
if XOR_Prism <> 0 then
  UpdateGlobalOptions
end;



procedure InitUnit_InnoStringSupport;
begin
GlobalOptions := 0; // OPTIONFLAG_WorkAroundReturnAnsiStringBug;
UpdateGlobalOptions
end;

procedure DoneUnit_InnoStringSupport;
begin
end;


initialization
InitUnit_InnoStringSupport;

finalization
DoneUnit_InnoStringSupport;

end.
