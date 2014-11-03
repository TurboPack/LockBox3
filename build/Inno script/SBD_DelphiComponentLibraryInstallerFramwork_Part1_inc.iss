[Code]
// This file is an include file for an Inno project. In goes in the Code section.
// This include file is part of the SBD Delphi Component Library Installer Framework.

function GetAppId( Value: string): string;
begin
result := AppId
end;


var
    BPL_DLL_Dir: string;

function DelphiRegKey( compiler: string): string;
begin
{
The following information was provided by Eugene Mayevski 'EldoS Corp
on StackOverflow (http://stackoverflow.com/questions/4348399/how-to-compute-the-delphi-root-directory-for-a-range-of-compilers).
Thanks Eugene.

Delphi5: HKEY_LOCAL_MACHINE\SOFTWARE\Borland\Delphi\5.0\RootDir
Delphi6: HKEY_LOCAL_MACHINE\SOFTWARE\Borland\Delphi\6.0\RootDir
Delphi7: HKEY_LOCAL_MACHINE\SOFTWARE\Borland\Delphi\7.0\RootDir
Delphi 8 (.NET-only product): HKEY_LOCAL_MACHINE\SOFTWARE\Borland\BDS\2.0\RootDir
Delphi 2005: HKEY_LOCAL_MACHINE\SOFTWARE\Borland\BDS\3.0\RootDir
BDS 2006: HKEY_LOCAL_MACHINE\SOFTWARE\Borland\BDS\4.0\RootDir
Delphi 2007: HKEY_LOCAL_MACHINE\SOFTWARE\Borland\BDS\5.0\RootDir
Delphi 2009: HKEY_LOCAL_MACHINE\SOFTWARE\CodeGear\BDS\6.0\RootDir
Delphi 2010: HKEY_LOCAL_MACHINE\SOFTWARE\CodeGear\BDS\7.0\RootDir
Delphi XE: HKEY_LOCAL_MACHINE\SOFTWARE\Embarcadero\BDS\8.0\RootDir
}
compiler := Uppercase( compiler);
if compiler = 'D5' then
    result := 'SOFTWARE\Borland\Delphi\5.0'
  else if compiler = 'D6' then
    result := 'SOFTWARE\Borland\Delphi\6.0'
  else if compiler = 'D7' then
    result := 'SOFTWARE\Borland\Delphi\7.0'
  else if compiler = 'D2005' then
    result := 'SOFTWARE\Borland\BDS\3.0'
  else if compiler = 'D2006' then
    result := 'SOFTWARE\Borland\BDS\4.0'
  else if compiler = 'D2007' then
    result := 'SOFTWARE\Borland\BDS\5.0' //='C:\Program Files\CodeGear\RAD Studio\5.0\'
  else if compiler = 'D2009' then
    result := 'SOFTWARE\CodeGear\BDS\6.0'
  else if compiler = 'D2010' then
    result := 'SOFTWARE\CodeGear\BDS\7.0' //='C:\Program Files\Embarcadero\RAD Studio\7.0\'
  else if compiler = 'DXE' then
    result := 'SOFTWARE\Embarcadero\BDS\8.0'
  else if compiler = 'DXE2' then
    result := 'SOFTWARE\Embarcadero\BDS\9.0'
  else
    result := ''
// Note: The ValueName is always 'RootDir'
end;
