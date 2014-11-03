;TO DO:
;  Uninstall icon.
#define MyAppName "TurboPower LockBox 3"
#define MyAppIconFlagshipName "LockBox 3 Demonstration Program"
#define MyAppPublisher "TurboPower LockBox 3 Open Source Project"
#define MyAppURL "http://lockbox.seanbdurkin.id.au/"
#define MyAppExeName "Lockbox3_Demo.exe"
#define SourceProductPath "V:\projects\turbopower\lockbox3\Executable bay\Staging bay - Input to install engine"
#define HelperPath "V:\Projects\TurboPower\LockBox3\Executable bay" 
#define LockBoxSourceRoot "V:\projects\turbopower\lockbox3\trunc"     
#define OutputDir "V:\Projects\TurboPower\LockBox3\Executable bay\Workspace - Install engine output"

[InnoIDE_Settings]
LogFile="{#OutputDir}\InstallerLog.txt"

[Setup]
AppName={#MyAppName}
AppVersion={code:ComputeAppVersion}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
DefaultDirName={pf}\{#MyAppName}
DefaultGroupName={#MyAppName}
OutputDir={#OutputDir}
; Manually rename like "LB 3.1.5 7-Jan-2011.exe" and then zip into like-named zip file.
; http://sourceforge.net/projects/tplockbox/files/TurboPower%20LockBox/LockBox%20version%203/3.1.5/LB%203.1.5%207-Jan-2011.zip/download
OutputBaseFilename=DebugSetup
Compression=lzma/Max
SolidCompression=true
Uninstallable=yes
UsePreviousAppDir=yes
AppId={code:GetAppId}
UsePreviousLanguage=no
PrivilegesRequired=none

[Languages]
Name: "en"; MessagesFile: "compiler:Default.isl"; LicenseFile: "{#LockBoxSourceRoot}\build\Inno script\license.txt"; InfoBeforeFile: "Before.txt"; InfoAfterFile: "After.txt"


[Components]
Name: Demo; Description: "Demonstration programs"; Types: full
Name: Test; Description: "DUnit test and confidence programs and constant precomputation proofs"; Types: full

[Tasks]
; AdditionalIcons:
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked

; This file is an include file for an Inno project. In goes in the Tasks section.
; This include file is part of the SBD Delphi Component Library Installer Framework.

; Automated Component Library Installation:
Name: Compile; Description: "Compile packages"; GroupDescription: "Automated Component Library Installation:"; Flags: checkablealone
Name: Compile\Install; Description: "Install design-time packages"; Flags: dontinheritcheck
Name: Compile\LibPath; Description: "Add DCU directory to library search path"; Flags: dontinheritcheck

[Files]
; This file is an include file for an Inno project. In goes in the files section.
; This include file is part of the SBD Delphi Component Library Installer Framework.
; It is required that HelperPath be defined in the main .iss file.

Source: "{#HelperPath}\CommandLine_d2010.dll"; DestDir: {tmp}; Flags: dontcopy ignoreversion; 

Source: "{#SourceProductPath}\{#MyAppExeName}"; DestDir: {app}; Components: Demo
Source: {#LockBoxSourceRoot}\*.txt; DestDir: {app}; 
; D7
Source: {#LockBoxSourceRoot}\packages\d7\TP_LockBox3.dpk; DestDir: {app}\packages\d7;           AfterInstall: PackageDeployed('{app}\packages\d7','TP_LockBox3.dpk','d7');
Source: {#LockBoxSourceRoot}\packages\d7\TP_LockBox3.res; DestDir: {app}\packages\d7; 
Source: {#LockBoxSourceRoot}\packages\d7\TP_LockBox3.dof; DestDir: {app}\packages\d7; 
Source: {#LockBoxSourceRoot}\packages\d7\dclTP_LockBox3.dpk; DestDir: {app}\packages\d7;         AfterInstall: PackageDeployed('{app}\packages\d7','dclTP_LockBox3.dpk','d7');
Source: {#LockBoxSourceRoot}\packages\d7\dclTP_LockBox3.res; DestDir: {app}\packages\d7 
Source: {#LockBoxSourceRoot}\packages\d7\dclTP_LockBox3.dof; DestDir: {app}\packages\d7 
Source: {#LockBoxSourceRoot}\packages\d7\TP_LockBox3_d7.bpg; DestDir: {app}\packages\d7; 
Source: {#LockBoxSourceRoot}\packages\d7\LockBox_DUnit_Tests.dpr; DestDir: {app}\packages\d7;    Components: Test   
; D2005
Source: {#LockBoxSourceRoot}\packages\d2005\TP_LockBox3.dpk; DestDir: {app}\packages\d2005;           AfterInstall: PackageDeployed('{app}\packages\d2005','TP_LockBox3.dpk','d2005');
Source: {#LockBoxSourceRoot}\packages\d2005\TP_LockBox3.bdsproj; DestDir: {app}\packages\d2005; 
Source: {#LockBoxSourceRoot}\packages\d2005\TP_LockBox3.res; DestDir: {app}\packages\d2005; 
Source: {#LockBoxSourceRoot}\packages\d2005\dclTP_LockBox3.dpk; DestDir: {app}\packages\d2005;         AfterInstall: PackageDeployed('{app}\packages\d2005','dclTP_LockBox3.dpk','d2005');
Source: {#LockBoxSourceRoot}\packages\d2005\dclTP_LockBox3.bdsproj; DestDir: {app}\packages\d2005; 
Source: {#LockBoxSourceRoot}\packages\d2005\dclTP_LockBox3.res; DestDir: {app}\packages\d2005 
Source: {#LockBoxSourceRoot}\packages\d2005\TP_LockBox3_d2005.bdsgroup; DestDir: {app}\packages\d2005; 
Source: {#LockBoxSourceRoot}\packages\d2005\LockBox_DUnit_Tests.dpr; DestDir: {app}\packages\d2005;    Components: Test   
; D2007
Source: {#LockBoxSourceRoot}\packages\d2007\Precompute.dpr; DestDir: {app}\packages\d2007;     Components: Test
Source: {#LockBoxSourceRoot}\packages\d2007\Precompute.dproj; DestDir: {app}\packages\d2007;   Components: Test
Source: {#LockBoxSourceRoot}\packages\d2007\Precompute.res; DestDir: {app}\packages\d2007;     Components: Test
Source: {#LockBoxSourceRoot}\packages\d2007\TP_LockBox3.dpk; DestDir: {app}\packages\d2007;           AfterInstall: PackageDeployed('{app}\packages\d2007','TP_LockBox3.dpk','d2007');
Source: {#LockBoxSourceRoot}\packages\d2007\TP_LockBox3.dproj; DestDir: {app}\packages\d2007; 
Source: {#LockBoxSourceRoot}\packages\d2007\TP_LockBox3.res; DestDir: {app}\packages\d2007; 
Source: {#LockBoxSourceRoot}\packages\d2007\dclTP_LockBox3.dpk; DestDir: {app}\packages\d2007;         AfterInstall: PackageDeployed('{app}\packages\d2007','dclTP_LockBox3.dpk','d2007');
Source: {#LockBoxSourceRoot}\packages\d2007\dclTP_LockBox3.dproj; DestDir: {app}\packages\d2007; 
Source: {#LockBoxSourceRoot}\packages\d2007\dclTP_LockBox3.res; DestDir: {app}\packages\d2007 
Source: {#LockBoxSourceRoot}\packages\d2007\TP_LockBox3_d2007.groupproj; DestDir: {app}\packages\d2007; 
Source: {#LockBoxSourceRoot}\packages\d2007\LockBox_DUnit_Tests.dpr; DestDir: {app}\packages\d2007;    Components: Test   
; D2009
Source: {#LockBoxSourceRoot}\packages\d2009\Precompute.dpr; DestDir: {app}\packages\d2009;       Components: Test   
Source: {#LockBoxSourceRoot}\packages\d2009\Precompute.dproj; DestDir: {app}\packages\d2009;     Components: Test
Source: {#LockBoxSourceRoot}\packages\d2009\Precompute.res; DestDir: {app}\packages\d2009;       Components: Test
Source: {#LockBoxSourceRoot}\packages\d2009\TP_LockBox3.dpk; DestDir: {app}\packages\d2009;          AfterInstall: PackageDeployed('{app}\packages\d2009','TP_LockBox3.dpk','d2009');
Source: {#LockBoxSourceRoot}\packages\d2009\TP_LockBox3.dproj; DestDir: {app}\packages\d2009; 
Source: {#LockBoxSourceRoot}\packages\d2009\TP_LockBox3.res; DestDir: {app}\packages\d2009; 
Source: {#LockBoxSourceRoot}\packages\d2009\dclTP_LockBox3.dpk; DestDir: {app}\packages\d2009;      AfterInstall: PackageDeployed('{app}\packages\d2009','dclTP_LockBox3.dpk','d2009');
Source: {#LockBoxSourceRoot}\packages\d2009\dclTP_LockBox3.dproj; DestDir: {app}\packages\d2009; 
Source: {#LockBoxSourceRoot}\packages\d2009\dclTP_LockBox3.res; DestDir: {app}\packages\d2009; 
Source: {#LockBoxSourceRoot}\packages\d2009\TP_LockBox3_d2009.groupproj; DestDir: {app}\packages\d2009; 
; D2010
Source: {#LockBoxSourceRoot}\packages\d2010\Precompute.dpr; DestDir: {app}\packages\d2010;       Components: Test   
Source: {#LockBoxSourceRoot}\packages\d2010\Precompute.dproj; DestDir: {app}\packages\d2010;     Components: Test
Source: {#LockBoxSourceRoot}\packages\d2010\Precompute.res; DestDir: {app}\packages\d2010;       Components: Test
Source: {#LockBoxSourceRoot}\packages\d2010\TP_LockBox3.dpk; DestDir: {app}\packages\d2010;          AfterInstall: PackageDeployed('{app}\packages\d2010','TP_LockBox3.dpk','d2010');
Source: {#LockBoxSourceRoot}\packages\d2010\TP_LockBox3.dproj; DestDir: {app}\packages\d2010; 
Source: {#LockBoxSourceRoot}\packages\d2010\TP_LockBox3.res; DestDir: {app}\packages\d2010; 
Source: {#LockBoxSourceRoot}\packages\d2010\dclTP_LockBox3.dpk; DestDir: {app}\packages\d2010;       AfterInstall: PackageDeployed('{app}\packages\d2010','dclTP_LockBox3.dpk','d2010');
Source: {#LockBoxSourceRoot}\packages\d2010\dclTP_LockBox3.dproj; DestDir: {app}\packages\d2010; 
Source: {#LockBoxSourceRoot}\packages\d2010\dclTP_LockBox3.res; DestDir: {app}\packages\d2010; 
Source: {#LockBoxSourceRoot}\packages\d2010\LockBox_DUnit_Tests.dpr; DestDir: {app}\packages\d2010;    Components: Test   
Source: {#LockBoxSourceRoot}\packages\d2010\LockBox_DUnit_Tests.dproj; DestDir: {app}\packages\d2010;  Components: Test
Source: {#LockBoxSourceRoot}\packages\d2010\LockBox_DUnit_Tests.res; DestDir: {app}\packages\d2010;    Components: Test
Source: {#LockBoxSourceRoot}\packages\d2010\MakeSampleKey.dpr; DestDir: {app}\packages\d2010;   Components: Demo;    
Source: {#LockBoxSourceRoot}\packages\d2010\MakeSampleKey.dproj; DestDir: {app}\packages\d2010; Components: Demo; 
Source: {#LockBoxSourceRoot}\packages\d2010\MakeSampleKey.res; DestDir: {app}\packages\d2010;   Components: Demo; 
Source: {#LockBoxSourceRoot}\packages\d2010\Lockbox3_Demo.dpr; DestDir: {app}\packages\d2010;   Components: Demo;    AfterInstall: PackageDeployed('{app}\packages\d2010','Lockbox3_Demo.dpr','d2010');
Source: {#LockBoxSourceRoot}\packages\d2010\Lockbox3_Demo.dproj; DestDir: {app}\packages\d2010; Components: Demo; 
Source: {#LockBoxSourceRoot}\packages\d2010\Lockbox3_Demo.res; DestDir: {app}\packages\d2010;   Components: Demo; 
Source: {#LockBoxSourceRoot}\packages\d2010\TP_LockBox3_d2010.groupproj; DestDir: {app}\packages\d2010
Source: {#LockBoxSourceRoot}\packages\d2010\RSA_Encryption_Demo.dpr; DestDir: {app}\packages\d2010; Components: Demo;  
Source: {#LockBoxSourceRoot}\packages\d2010\RSA_Encryption_Demo.dproj; DestDir: {app}\packages\d2010; Components: Demo; 
Source: {#LockBoxSourceRoot}\packages\d2010\RSA_Encryption_Demo.res; DestDir: {app}\packages\d2010; Components: Demo; 
Source: {#LockBoxSourceRoot}\packages\d2010\UInt64_to_Int64_Convert_Utility.dpr; DestDir: {app}\packages\d2010;   
Source: {#LockBoxSourceRoot}\packages\d2010\UInt64_to_Int64_Convert_Utility.dproj; DestDir: {app}\packages\d2010;  

; D2010 + SI
;Source: {#LockBoxSourceRoot}\packages\D2010 with SmartInspect\Precompute.dpr; DestDir: {app}\packages\D2010 with SmartInspect; 
;Source: {#LockBoxSourceRoot}\packages\D2010 with SmartInspect\Precompute.dproj; DestDir: {app}\packages\D2010 with SmartInspect; 
;Source: {#LockBoxSourceRoot}\packages\D2010 with SmartInspect\Precompute.res; DestDir: {app}\packages\D2010 with SmartInspect; 
;Source: {#LockBoxSourceRoot}\packages\D2010 with SmartInspect\TP_LockBox3.dpk; DestDir: {app}\packages\D2010 with SmartInspect; 
;Source: {#LockBoxSourceRoot}\packages\D2010 with SmartInspect\TP_LockBox3.dproj; DestDir: {app}\packages\D2010 with SmartInspect; 
;Source: {#LockBoxSourceRoot}\packages\D2010 with SmartInspect\TP_LockBox3.res; DestDir: {app}\packages\D2010 with SmartInspect; 
;Source: {#LockBoxSourceRoot}\packages\D2010 with SmartInspect\dclTP_LockBox3.dpk; DestDir: {app}\packages\D2010 with SmartInspect; 
;Source: {#LockBoxSourceRoot}\packages\D2010 with SmartInspect\dclTP_LockBox3.dproj; DestDir: {app}\packages\D2010 with SmartInspect; 
;Source: {#LockBoxSourceRoot}\packages\D2010 with SmartInspect\dclTP_LockBox3.res; DestDir: {app}\packages\D2010 with SmartInspect; 
;Source: {#LockBoxSourceRoot}\packages\D2010 with SmartInspect\LockBox_DUnit_Tests.dpr; DestDir: {app}\packages\D2010 with SmartInspect; 
;Source: {#LockBoxSourceRoot}\packages\D2010 with SmartInspect\LockBox_DUnit_Tests.dproj; DestDir: {app}\packages\D2010 with SmartInspect; 
;Source: {#LockBoxSourceRoot}\packages\D2010 with SmartInspect\LockBox_DUnit_Tests.res; DestDir: {app}\packages\D2010 with SmartInspect; 
;Source: {#LockBoxSourceRoot}\packages\D2010 with SmartInspect\MakeSampleKey.dpr; DestDir: {app}\packages\D2010 with SmartInspect; 
;Source: {#LockBoxSourceRoot}\packages\D2010 with SmartInspect\MakeSampleKey.dproj; DestDir: {app}\packages\D2010 with SmartInspect; 
;Source: {#LockBoxSourceRoot}\packages\D2010 with SmartInspect\MakeSampleKey.res; DestDir: {app}\packages\D2010 with SmartInspect; 
;Source: {#LockBoxSourceRoot}\packages\D2010 with SmartInspect\TP_LockBox3_d2010.groupproj; DestDir: {app}\packages\D2010 with SmartInspect

; DXE
Source: {#LockBoxSourceRoot}\packages\DXE\TP_LockBox3.dpk; DestDir: {app}\packages\DXE;          AfterInstall: PackageDeployed('{app}\packages\DXE','TP_LockBox3.dpk','DXE');
Source: {#LockBoxSourceRoot}\packages\DXE\TP_LockBox3.res; DestDir: {app}\packages\DXE; 
Source: {#LockBoxSourceRoot}\packages\DXE\dclTP_LockBox3.dpk; DestDir: {app}\packages\DXE;       AfterInstall: PackageDeployed('{app}\packages\DXE','dclTP_LockBox3.dpk','DXE');
Source: {#LockBoxSourceRoot}\packages\DXE\dclTP_LockBox3.res; DestDir: {app}\packages\DXE; 
Source: {#LockBoxSourceRoot}\packages\DXE\Lockbox3_Demo.dpr; DestDir: {app}\packages\DXE;   Components: Demo;    AfterInstall: PackageDeployed('{app}\packages\DXE','Lockbox3_Demo.dpr','DXE');
Source: {#LockBoxSourceRoot}\packages\DXE\RSA_Encryption_Demo.dpr; DestDir: {app}\packages\DXE; Components: Demo;  
Source: {#LockBoxSourceRoot}\packages\DXE\MakeSampleKey.dpr; DestDir: {app}\packages\DXE;   Components: Demo;    
Source: {#LockBoxSourceRoot}\packages\DXE\LockBox_DUnit_Tests.dpr; DestDir: {app}\packages\DXE;    Components: Test   

; DXE2
Source: {#LockBoxSourceRoot}\packages\DXE2\TP_LockBox3.dpk; DestDir: {app}\packages\DXE2;          AfterInstall: PackageDeployed('{app}\packages\DXE2','TP_LockBox3.dpk','DXE2');
Source: {#LockBoxSourceRoot}\packages\DXE2\TP_LockBox3.res; DestDir: {app}\packages\DXE2; 
Source: {#LockBoxSourceRoot}\packages\DXE2\dclTP_LockBox3.dpk; DestDir: {app}\packages\DXE2;       AfterInstall: PackageDeployed('{app}\packages\DXE2','dclTP_LockBox3.dpk','DXE2');
Source: {#LockBoxSourceRoot}\packages\DXE2\dclTP_LockBox3.res; DestDir: {app}\packages\DXE2; 
Source: {#LockBoxSourceRoot}\packages\DXE2\Lockbox3_Demo.dpr; DestDir: {app}\packages\DXE2;   Components: Demo;    AfterInstall: PackageDeployed('{app}\packages\DXE2','Lockbox3_Demo.dpr','DXE2');
Source: {#LockBoxSourceRoot}\packages\DXE2\LockBox_DUnit_Tests.dpr; DestDir: {app}\packages\DXE2;    Components: Test   
Source: {#LockBoxSourceRoot}\packages\DXE2\TP_LockBox3.dproj; DestDir: {app}\packages\DXE2;
Source: {#LockBoxSourceRoot}\packages\DXE2\TP_LockBox3_DXE2.groupproj; DestDir: {app}\packages\DXE2;
Source: {#LockBoxSourceRoot}\packages\DXE2\dclTP_LockBox3.dproj; DestDir: {app}\packages\DXE2;
Source: {#LockBoxSourceRoot}\packages\DXE2\Lockbox3_Demo.dproj; DestDir: {app}\packages\DXE2;   Components: Demo;
Source: {#LockBoxSourceRoot}\packages\DXE2\Lockbox3_Demo.res; DestDir: {app}\packages\DXE2;     Components: Demo;
Source: {#LockBoxSourceRoot}\packages\DXE2\LockBox_DUnit_Tests.dproj; DestDir: {app}\packages\DXE2;  Components: Test
Source: {#LockBoxSourceRoot}\packages\DXE2\LockBox_DUnit_Tests.res; DestDir: {app}\packages\DXE2;    Components: Test

;; units and other compilands
Source: {#LockBoxSourceRoot}\run\*.pas; DestDir: {app}\run; Flags: recursesubdirs; 
Source: {#LockBoxSourceRoot}\run\*.dfm; DestDir: {app}\run; Flags: recursesubdirs skipifsourcedoesntexist; 
Source: {#LockBoxSourceRoot}\design\*.pas; DestDir: {app}\design; Flags: recursesubdirs; 
Source: {#LockBoxSourceRoot}\design\*.dfm; DestDir: {app}\design; Flags: recursesubdirs; 
Source: {#LockBoxSourceRoot}\design\*.bmp; DestDir: {app}\design; Flags: recursesubdirs; 
Source: {#LockBoxSourceRoot}\design\*.jpg; DestDir: {app}\design; Flags: recursesubdirs; 
Source: {#LockBoxSourceRoot}\design\*.dcr; DestDir: {app}\design; Flags: recursesubdirs; 
Source: {#LockBoxSourceRoot}\demo\*.pas; DestDir: {app}\demo; Flags: recursesubdirs;   Components: Demo; 
Source: {#LockBoxSourceRoot}\demo\*.dfm; DestDir: {app}\demo; Flags: recursesubdirs;   Components: Demo; 
Source: {#LockBoxSourceRoot}\test\*.pas; DestDir: {app}\test; Flags: recursesubdirs; 
Source: {#LockBoxSourceRoot}\test\*.dfm; DestDir: {app}\test; Flags: recursesubdirs; 
Source: {#LockBoxSourceRoot}\resources\*.bmp; DestDir: {app}\resources; Flags: recursesubdirs; 

[Icons]
Name: "{group}\{#MyAppIconFlagshipName}"; Filename: "{app}\{#MyAppExeName}"
Name: "{commondesktop}\{#MyAppIconFlagshipName}"; Filename: "{app}\{#MyAppExeName}"; Tasks: desktopicon
Name: "{group}\{cm:UninstallProgram,{#MyAppName}}"; Filename: {uninstallexe}

[Run]
Filename: "{app}\{#MyAppExeName}"; Description: "{cm:LaunchProgram,{#StringChange(MyAppName, "&", "&&")}} demo program (compiled with Delphi 2010)"; Flags: nowait postinstall skipifsilent; Components: Demo


[Code]
const
  AppId = 'TurboPowerLockBox3';

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

procedure DeregisterDesignTimePackages;
begin
RegDeleteValue( HKEY_CURRENT_USER,
  DelphiRegKey( 'd7') + '\Known Packages',
  BPL_DLL_Dir + '\dclTP_LockBox3_d7.bpl');

RegDeleteValue( HKEY_CURRENT_USER,
  DelphiRegKey( 'd2005') + '\Known Packages',
  BPL_DLL_Dir + '\dclTP_LockBox3_d2005.bpl');

RegDeleteValue( HKEY_CURRENT_USER,
  DelphiRegKey( 'd2007') + '\Known Packages',
  BPL_DLL_Dir + '\dclTP_LockBox3_d2007.bpl');

RegDeleteValue( HKEY_CURRENT_USER,
  DelphiRegKey( 'd2009') + '\Known Packages',
  BPL_DLL_Dir + '\dclTP_LockBox3_d2009.bpl');

RegDeleteValue( HKEY_CURRENT_USER,
  DelphiRegKey( 'd2010') + '\Known Packages',
  BPL_DLL_Dir + '\dclTP_LockBox3_d2010.bpl');

RegDeleteValue( HKEY_CURRENT_USER,
  DelphiRegKey( 'dXE') + '\Known Packages',
  BPL_DLL_Dir + '\dclTP_LockBox3_dXE.bpl')

RegDeleteValue( HKEY_CURRENT_USER,
  DelphiRegKey( 'dXE2') + '\Known Packages',
  BPL_DLL_Dir + '\dclTP_LockBox3_dXE2.bpl')
end;


function isCompilerActiveForThisScript( Compiler: string): boolean;
begin
if Compiler = 'D5' then
    result := False

  else if Compiler = 'D6' then
    result := False

  else if Compiler = 'D7' then
    result := True

  else if Compiler = 'D2005' then
    result := True

  else if Compiler = 'D2006' then
    result := False

  else if Compiler = 'D2007' then
    result := True

  else if Compiler = 'D2009' then
    result := True

  else if Compiler = 'D2010' then
    result := True

  else if Compiler = 'DXE' then
    result := True

  else if Compiler = 'DXE2' then
    result := True

  else 
    result := False
end;


function GetMSBuildSchemeFileName: string;
begin
result := 'TP_LockBox3.dproj';
end;

[Code]
// This file is an include file for an Inno project. In goes in the Code section.
// This include file is part of the SBD Delphi Component Library Installer Framework.

type
  TCompiler = record
    FProgId: string;
    FDisplayName: string;
    Fdcc32exe_FileName: string;
    FhasInstalled: boolean; // Is the compiler even installed on the host system?
    FisUserSelected: boolean; // Is the compiler selected by the user for package installation?
    FisEnabledForThisScript: boolean; // Some scripts may only be interested in a subrange of compilers.
    FWizardPageIdx: integer;
    FActive: boolean; // A combination of the above gates.
    F_DCU_Dir: string;
    F_DCP_Dir: string;
    FDefault_BPL_DLL_Suffix: string;
    F_EXE_Dir: string;
    FDirsPage: TInputDirWizardPage;
    FRequiresDCP_in_U_SearchPath: boolean;
    FRequiresNS: boolean;
    FWin32LibKey: string;
    end;
  TCompilerList = array of TCompiler;

  TPackageKind = (pkBPL, pkDLL, pkEXE);

  TPackageManifest = record
    FIdx: integer;
    FDirectory: string;
    FPackageName: string;
    FFullFileName: string;
    FCompilerStr: string;
    FisRunTime: boolean;
    FisDesignTime: boolean;
    FDescription: string;
    FPackageKind: TPackageKind;
    F_BPL_DLL_Suffix: string;
    FdoesOverrideDefault_BPL_DLL_Suffix: boolean;
    FCompiledOk: boolean;
    end;

  TMemoLineKind = (lkUndecorated, lkProcessOutput, lkCommandEcho, lkDiagnostic);
  PUTF8Char = PAnsichar;
  UTF8string = ansistring;

  TDelphiTokenType = (
    tokenNull,          // NULL. Not a token.
    tokenIdentifier,    // Keywords and identifiers.
    tokenWord,          // Operators, statement terminators and other guff.
    tokenWhiteSpace,
    tokenCommentBrace,  // { Comment }       May be multi-line comment.
    tokenCommentStar,   // (* Comment *)     May be multi-line comment.
    tokenCommentSlash,  // // Comment        Single line comment only.
    tokenLiteralString, // 'You''r in'       Single line only.
    tokenDirective);    // {$DIRECTIVE param param ...} or {$DIRECTIVE}

  TDelphiToken = record
    FTokenType: TDelphiTokenType;
    FValue: string;
    end;

  TDelphiTokenList = array of TDelphiToken;

  TJobKind = (jobRemoveKey, jobSetKey, jobExternalProcess, jobCreateCfgFile, jobEraseTempFiles);
  TJobStatus = (statusQueued, statusInWork, statusSucceeded, statusFailed);
  TJob = record
    FJobKind: TJobKind;
    FStatus: TJobStatus;
    FJobDescription: string;
    FisFinalJob: boolean;
    FCmdLine: string;
    FDir: string;
    FdoesUnderstandCntrlC: boolean;
    FisOEMCP: boolean;
    FPollRate_ms: integer;
    FPackageManifestIdx: integer;
    FCompilerIdx: integer;
    FRootKey: integer;
    FSubKeyName: string;
    FValueName: string;
    FData: string;
    end;
  TJobList = array of TJob;

TWatcher = record
  FDir: string;
  FFilterName: string;
  FBeforeFiles: TStrings;
  end;                                 

  TWatcherList = array of TWatcher;

var
  doCommandLineDiagnostics: boolean;
  AbortButton: TNewButton;
  Compilers: TCompilerList;
  DoAbort: boolean;
  CmdPage, CompilePage: TWizardPage;
  Memo, Memo2: TNewMemo;
  CmdEdit, DirEdit: TNewEdit;
  KindPrefix: array of string;
  Manifests: array of TPackageManifest;
  INC_Dir: string;
  SelectCompilerPage: TInputOptionWizardPage;
  CommonDirsPage: TInputDirWizardPage;
  FirstCompilerDirsPageId, LastCompilerDirsPageId: integer;
  FirstTime_ExitedSelectDirPage: boolean;
  PrevPageID: integer;
  CmdBar: TNewProgressBar;
  Jobs: TJobList;
  JobCount: integer;
  ConcludedJobCount: integer;
  CurrentJobIndex: integer;
  TempFiles: TStrings;
  SpawnedFiles: TStrings;
  CompileInstruction: TLabel;
  HighestCompilerIdx: integer;


procedure PrepareCommand(
      CmdLine, Dir: unicodestring; // UTF-16
      doesUnderstandCntrlC, isOEMCP: boolean;                
      PollRate_ms: integer;
      var ActivityToken: integer);
    external 'SBDIH_PrepareCommand@files:CommandLine_d2010.dll stdcall';




procedure API_QuantumExecute(
        var ActivityToken: integer;
        var hasOutput: boolean;
        var Line: PAnsiChar;
        var isDone: boolean;
        var RetCode: integer);
   external 'SBDIH_QuantumExecute@files:CommandLine_d2010.dll stdcall';

function QuantumExecute(
        var ActivityToken: integer;
        var hasOutput: boolean;
        var Line: ansistring;  // Ansistring using the system codepage.
        var RetCode: integer): boolean;
var
   Line1: PAnsiChar;
begin
API_QuantumExecute( ActivityToken, hasOutput, Line1, result, RetCode);
Line := Line1
end;




procedure AbortActivity(
        var ActivityToken: integer);
    external 'SBDIH_AbortActivity@files:CommandLine_d2010.dll stdcall';


function API_Diag: PAnsiChar;
    external 'Diag@files:CommandLine_d2010.dll stdcall';
// API_Diag returns a UTF-8 encoding of the diagnostic strings and then
//  clears the diagnostic strings.



function API_GlobalOptions( 
        XOR_Prism: longword): longword;
    external 'SBDIH_globaloptions@files:CommandLine_d2010.dll stdcall';

function GetHelperGlobalOptions: longword;
begin
result := API_GlobalOptions( 0)
end;

const
  OPTIONFLAG_WorkAroundReturnAnsiStringBug = 1;
  OPTIONFLAG_Diagnostics = 2;

function hasFlag( Flag: longword): boolean;
begin
result := (GetHelperGlobalOptions and Flag) = Flag
end;

function doWorkAround_Inno5_4_0u_ReturnAnsiString_Bug: boolean;
begin
result := hasFlag( OPTIONFLAG_WorkAroundReturnAnsiStringBug)
end;

procedure Toggle_doWorkAround_Inno5_4_0u_ReturnAnsiString_Bug;
begin
API_GlobalOptions( OPTIONFLAG_WorkAroundReturnAnsiStringBug)
end;

procedure SetDiagLogOn( Value: boolean); 
begin
if hasFlag( OPTIONFLAG_Diagnostics) <> Value then
  API_GlobalOptions( OPTIONFLAG_Diagnostics)
end;


function API_Convert_Ansi_To_UTF8( AnsiValue: ansistring): PUTF8Char;
    external 'SBDIH_ansi_to_utf8@files:CommandLine_d2010.dll stdcall';
// Converts ansistring in system code page to UTF-8 string.
function Convert_Ansi_To_UTF8( const AnsiValue: ansistring): UTF8string;
var
  Res: PUTF8Char;
begin
Res := API_Convert_Ansi_To_UTF8( AnsiValue);
result := Res
end;



function API_Convert_UTF8_To_Ansi( UTF8Value: utf8string): PAnsiChar; 
    external 'SBDIH_utf8_to_ansi@files:CommandLine_d2010.dll stdcall';
// Converts UTF-8 string to ansistring in system code page.
function Convert_UTF8_To_Ansi( const UTF8Value: UTF8string): ansistring;
var
  Res: Pansichar;
begin
Res    := API_Convert_UTF8_To_Ansi( UTF8Value);
result := Res
end;


function CheckFor_Inno5_4_0u_ReturnAnsiString_Bug: boolean;
var
  Original: ansistring;
begin
Original := 'Verbatim';
result := Convert_UTF8_To_Ansi( Convert_Ansi_To_UTF8( Original)) = Original;
if not result then
  Toggle_doWorkAround_Inno5_4_0u_ReturnAnsiString_Bug
end;

function API_Convert_UTF16_To_Ansi( UTF16Value: string): PAnsiChar;
    external 'SBDIH_utf16_to_ansi@files:CommandLine_d2010.dll stdcall';
// Converts UTF-16 string to ansistring in system code page.
function Convert_UTF16_To_Ansi( UTF16Value: string): ansistring;
var
  Ret: PAnsiChar;
begin
Ret    := API_Convert_UTF16_To_Ansi( UTF16Value);
result := Ret
end;


function Convert_Ansi_To_UTF16_Length( AnsiValue: ansistring): integer;
    external 'SBDIH_ansi_to_utf16_len@files:CommandLine_d2010.dll stdcall';
// Converts ansistring in system code page to UTF-16 string, which is
//  held locally. Returns the length in characters of the UTF-16 string.

function Convert_Ansi_To_UTF16_IndexedChar( CharIndex: integer): widechar;
    external 'SBDIH_ansi_to_utf16_char@files:CommandLine_d2010.dll stdcall';
// Subsequent to a call to Convert_Ansi_To_UTF16_Length, return the indexed
//  UTF-16 character. The index physical origin is 1.
//  Out of range indicies will return the nil character.
function Convert_Ansi_To_UTF16( AnsiValue: ansistring): string;
var
  L, j: integer;
begin
L := Convert_Ansi_To_UTF16_Length( AnsiValue);
SetLength( result, L);
for j := 1 to L do
  result[j] := Convert_Ansi_To_UTF16_IndexedChar(j)
end;



function Convert_UTF16_To_UTF8( UTF16Value: string): PUTF8Char; 
    external 'SBDIH_utf16_to_utf8@files:CommandLine_d2010.dll stdcall';
// Convert a UTF-16 string to a UTF-8 string.

function Convert_UTF8_To_UTF16_Length( UTF8Value: utf8string): integer;
    external 'SBDIH_utf8_to_utf16_len@files:CommandLine_d2010.dll stdcall';
// Converts a UTF-8 string to UTF-16 string, which is
//  held locally. Returns the length in characters of the UTF-16 string.

function Convert_UTF8_To_UTF16_IndexedChar( CharIndex: integer): widechar;
    external 'SBDIH_utf8_to_utf16_char@files:CommandLine_d2010.dll stdcall';
// Subsequent to a call to Convert_UTF8_To_UTF16_Length, return the indexed
//  UTF-16 character. The index physical origin is 1.
//  Out of range indicies will return the nil character.
function Convert_UTF8_To_UTF16( UTF8Value: UTF8string): string;
var
  L, j: integer;
begin
L := Convert_UTF8_To_UTF16_Length( UTF8Value);
SetLength( result, L);
for j := 1 to L do
  result[j] := Convert_UTF8_To_UTF16_IndexedChar(j)
end;

//function Convert_Oem_To_Ansi( OemValue: PAnsiChar): PAnsiChar;
//    external 'Diag@files:CommandLine_d2010.dll stdcall';




function Diag: string;
var
  P: PAnsiChar;
  s: ansistring;
  w: widechar;
begin
P := API_Diag;
s := P; // UTF-8
result := Convert_UTF8_To_UTF16( s)
end;



function GetIsAbort: boolean;
begin
result := DoAbort
end;




function dcc32( compiler: string): string;
begin
result := DelphiRegKey( compiler);
if (result <> '') and RegQueryStringValue( HKEY_LOCAL_MACHINE, result, 'RootDir', result) then
    result := RemoveBackslash( result) + '\bin\dcc32.exe'
  else
    result := ''
end;


procedure ClearLog;
begin
if WizardForm.CurPageId = CompilePage.ID then
    Memo2.Clear
  else
    Memo .Clear
end;

procedure LogOneLine( Kind: TMemoLineKind; Prefix, Payload, CombinedLine: string);
var
  s: string;
begin
s := CombinedLine;
if WizardForm.CurPageId = CompilePage.ID then
    Memo2.Lines.Add( s)
  else
    Memo .Lines.Add( s)
end;

procedure LogMemo( Kind: TMemoLineKind; Line: string);
var
  Prefix: string;
  Lines: TStringList;
  j: integer;
begin
Prefix := KindPrefix[ Ord( Kind)];
if Prefix <> '' then
  Prefix := Prefix + ': ';
if Pos( #13, Line) = 0 then
    LogOneLine( Kind, Prefix, Line, Prefix + Line)
  else
    begin
    Lines := TStringList.Create;
    try
      Lines.Text := Line;
      for j := 0 to Lines.Count - 1 do
        LogOneLine( Kind, Prefix, Line, Prefix + Lines[j]);
    finally
      Lines.Free
      end
     end
end;


procedure CheckDiag;
var
  D: string;
begin
D := Diag;
if D <> '' then
  LogMemo( lkDiagnostic, D)
end;


function InputCmd: string;
begin
result := CmdEdit.Text
end;


function InputDir: string;
begin
result := DirEdit.Text
end;

procedure WriteLn( Line: string);
begin
LogMemo( lkProcessOutput, Line)
end;


function ExecuteCommand(
      CmdLine, Dir: string;
      doesUnderstandCntrlC, isOEMCP: boolean;
      PollRate_ms: integer): integer;
var
  ActivityToken: integer;
  hasOutput: boolean;
  Line: ansistring;  
begin
DoAbort := False;
WriteLn( 'DEBUG INFO: CmdLine = '+CmdLine);
WriteLn( 'DEBUG INFO: Dir = '+Dir);
PrepareCommand( CmdLine, Dir, doesUnderstandCntrlC, isOEMCP, PollRate_ms, ActivityToken);

while not QuantumExecute( ActivityToken, hasOutput, Line, result) do
  begin
  if hasOutput then
    WriteLn( Line);
  CheckDiag;
  if not GetIsAbort then continue;
  AbortActivity( ActivityToken);
  result := -3;
  break
  end;
DoAbort := False
end;


procedure ExecuteOnClick( Sender: TObject);
var
  a1: ansistring;
  w1: string;
begin
ClearLog;
CheckDiag;
LogMemo( lkCommandEcho, Format( '%s', [InputCmd]));
AbortButton.Enabled := True;
try
  LogMemo( lkCommandEcho, Format( 'Return Code = %d', [ExecuteCommand( InputCmd, InputDir, False, True, 80)]));
finally
  AbortButton.Enabled := False
  end;
CheckDiag
end;


procedure WatchForWorkProducts( var Watcher: TWatcher; Directory: string; NameFilter: string);
var
  FileName: String;
  FindRec: TFindRec;
  Dir: string;
  L: integer;
begin
Dir := Directory;
L := Length( Dir);
if (L > 0) and (Dir[ L] = '\') then
  SetLength( Dir, L-1);
FileName := Dir + '\' + NameFilter;
Watcher.FDir := Dir;
Watcher.FFilterName := FileName;
if assigned( Watcher.FBeforeFiles) then
    Watcher.FBeforeFiles.Clear
  else
    Watcher.FBeforeFiles := TStringList.Create;   
if FindFirst( FileName, FindRec) then
  try
    repeat
      FileName := Dir + '\' + FindRec.Name;
      Watcher.FBeforeFiles.Add( FileName)
    until not FindNext( FindRec)
  finally
    FindClose( FindRec);
    end
end;


procedure EndWatch( var Watcher: TWatcher);
var
  FileName: String;
  FindRec: TFindRec;
  Dir: string;
begin
Dir := Watcher.FDir;
FileName := Watcher.FFilterName;
if FindFirst( FileName, FindRec) then
  try
    repeat
      FileName := Dir + '\' + FindRec.Name;
      if Watcher.FBeforeFiles.IndexOf( FileName) = -1 then
        begin // New file spawned.
        if SpawnedFiles.IndexOf( FileName) = -1 then
          SpawnedFiles.Add( FileName)
        end;
    until not FindNext( FindRec)
  finally
    FindClose( FindRec);
    end;
Watcher.FBeforeFiles.Free;
Watcher.FBeforeFiles := nil
end;

const SpawnsFile_StructureVersion = 1;

procedure WriteIntegerToStream( var Stream: TStream; Value: integer);
var
  s: string;
begin
s := IntToStr( Value);
while Length( s) < 4 do
  s := '0' + s;
if Length( s) > 4 then
  SetLength( s, 4);
Stream.Write( s, 8)
end;


procedure WriteStream_CRLF( var Stream: TStream);
var
  s: string;
begin
s := #13#10;
Stream.Write( s, 4)
end;


procedure SaveSpawnedFiles;
// Only call when {app} is defined and stable.
var
  FN: string;
  SpawnsStream: TStream;
  j, L: integer;
  s, SpawnedFile: string;
begin
try
FN := ExpandConstant('{app}\unins_files.dat');
if FileExists( FN) then
    begin
    SpawnsStream := TFileStream.Create( FN, fmOpenReadWrite);
    SpawnsStream.Size := 0
    end
  else
    SpawnsStream := TFileStream.Create( FN, fmCreate);
try
WriteIntegerToStream( SpawnsStream, SpawnsFile_StructureVersion);
L := SpawnedFiles.Count;
WriteIntegerToStream( SpawnsStream, L);
WriteStream_CRLF( SpawnsStream);
for j := 0 to SpawnedFiles.Count - 1 do
  begin
  SpawnedFile := SpawnedFiles[j];
  L := Length( SpawnedFile);
  WriteIntegerToStream( SpawnsStream, L);
  s := SpawnedFile;
  SpawnsStream.Write( s, L*2);
  WriteStream_CRLF( SpawnsStream);
  end;
finally
SpawnsStream.Free;
end
except
// silent catch - Its not that important.
end
end;

function ReadUnicodeFromStream( var Stream: TStream; CharLen: integer): string;
begin
result := '';
SetLength( result, CharLen);
if CharLen > 0 then
  Stream.Read( result, CharLen * 2)
end;


procedure DiscardCRLF( var Stream: TStream);
var
  s: string;
begin
s := ReadUnicodeFromStream( Stream, 2)
end;

function ReadIntegerFromStream( var Stream: TStream): integer;
var
  s: string;
begin
s := ReadUnicodeFromStream( Stream, 4);
result := StrToInt( s)
end;

procedure LoadSpawnedFiles;
// Only call when {app} is defined and stable.
var
  FN: string;
  SpawnsStream: TStream;
  s: string;
  L, SpawnedFilesCount, j: integer;
  StructureVersionIsAsExpected: boolean;
begin
SpawnedFiles.Clear;
SpawnsStream := nil;
try try
FN := ExpandConstant('{app}\unins_files.dat');
if FileExists( FN) then
    begin
    SpawnsStream := TFileStream.Create( FN, fmOpenRead);
    SpawnsStream.Position := 0
    end
  else
    exit;
StructureVersionIsAsExpected := ReadIntegerFromStream( SpawnsStream) = SpawnsFile_StructureVersion;
SpawnedFilesCount := ReadIntegerFromStream( SpawnsStream);
DiscardCRLF( SpawnsStream);
for j := 0 to SpawnedFilesCount - 1 do
  begin
  L := ReadIntegerFromStream( SpawnsStream);
  s := ReadUnicodeFromStream( SpawnsStream, L);
  SpawnedFiles.Add( s);
  s := '';
  DiscardCRLF( SpawnsStream);
  end;
finally
if assigned( SpawnsStream) then
  SpawnsStream.Free
end
except end
end;

procedure ClearSpawnedFiles;
var
  FN: string;
  SpawnsStream: TStream;
begin
SpawnedFiles.Clear;
FN := ExpandConstant('{app}\unins_files.dat');
DeleteFile( FN)
end;


procedure PurgeSpawns;
// Only for the uninstaller.
var
  i: integer;
  s: string;
  Ok: boolean;
begin
for i := 0 to SpawnedFiles.Count - 1 do
  begin
  s  := SpawnedFiles[i];
  Ok :=DeleteFile( s)
  end
end;


function FindCompilerOfManifest( const ManifestIdx: integer): integer;
var
  j: integer;
begin
result := -1;
for j := 0 to Length( Compilers) - 1 do
  begin
  if Compilers[j].FProgId <> Manifests[ManifestIdx].FCompilerStr then continue;
  result := j;
  break
  end
end;

procedure AddCfgJob( const ManifestIdx: integer);
var
  CompileIdx, Idx: integer;
  s: string;
begin
CompileIdx := FindCompilerOfManifest( ManifestIdx);
Idx := Length( Jobs);
SetLength( Jobs, Idx + 1);
with Jobs[Idx] do
  begin
  FJobKind := jobCreateCfgFile;
  FStatus  := statusQueued;
  FPackageManifestIdx := ManifestIdx;
  FCompilerIdx := CompileIdx;
  FisFinalJob  := False;
  s := Manifests[ManifestIdx].FPackageName;
  s := Format( 'Configuring %s in preparation for compilation.', [s]);
  FJobDescription := s;
  FCmdLine        := '';
  FDir            := '';
  FdoesUnderstandCntrlC := False;
  FisOEMCP        := True;
  FPollRate_ms    := 50;
  FRootKey        := HKEY_LOCAL_MACHINE;
  FSubKeyName     := '';
  FValueName      := '';
  FData           := ''
  end;
end;


procedure Create_EraseTempFilesJob;
var
  Idx: integer;
begin
Idx := Length( Jobs);
SetLength( Jobs, Idx + 1);
with Jobs[Idx] do
  begin
  FJobKind := jobEraseTempFiles;
  FStatus  := statusQueued;
  FPackageManifestIdx := -1;
  FCompilerIdx := -1;
  FisFinalJob  := True;
  FJobDescription := 'Erasing temporary configuration files.';
  FCmdLine        := '';
  FDir            := '';
  FdoesUnderstandCntrlC := False;
  FisOEMCP        := True;
  FPollRate_ms    := 50;
  FRootKey        := HKEY_LOCAL_MACHINE;
  FSubKeyName     := '';
  FValueName      := '';
  FData           := ''
  end;
end;


procedure AddCompileJob( const ManifestIdx: integer);
var
  CompileIdx, Idx: integer;
  s1, s2: string;
begin
CompileIdx := FindCompilerOfManifest( ManifestIdx);
Idx := Length( Jobs);
SetLength( Jobs, Idx + 1);
with Jobs[Idx] do
  begin
  FJobKind := jobExternalProcess;
  FStatus  := statusQueued;
  FPackageManifestIdx := ManifestIdx;
  FCompilerIdx := CompileIdx;
  FisFinalJob  := False;
  s1 := Manifests[ManifestIdx].FPackageName;
  s2 := Compilers[CompileIdx].FDisplayName;
  FJobDescription := Format( 'Compiling package %s with compiler %s.', [s1,s2]);
  s1              := Compilers[CompileIdx].Fdcc32exe_FileName;
  s2              := Manifests[ManifestIdx].FPackageName;
  FCmdLine        := Format( '"%s" "%s"', [s1,s2]);
  FDir            := Manifests[ManifestIdx].FDirectory;
  FdoesUnderstandCntrlC := False;
  FisOEMCP        := True;
  FPollRate_ms    := 80;
  FRootKey        := HKEY_LOCAL_MACHINE;
  FSubKeyName     := '';
  FValueName      := '';
  FData           := ''
  end;
end;                                           

procedure AddRegisterPackageJob( const ManifestIdx: integer);
// 
// C:\Program Files\Common Files\bpl\dclTP_LockBox3_d2010.bpl
// TurboPower LockBox 3 design-time package
var
  CompileIdx, Idx: integer;
  s1, s2: string;
  Suffix: string;
begin
CompileIdx := FindCompilerOfManifest( ManifestIdx);
Idx := Length( Jobs);
SetLength( Jobs, Idx + 1);
with Jobs[Idx] do
  begin
  FJobKind := jobSetKey;
  FStatus  := statusQueued;
  FPackageManifestIdx := ManifestIdx;
  FCompilerIdx := CompileIdx;
  FisFinalJob  := False;
  s1 := Manifests[ManifestIdx].FPackageName;
  s2 := Compilers[CompileIdx].FDisplayName;
  s1 := Format( 'Installing the design-time package %s with compiler %s', [s1,s2]);
  FJobDescription := s1;
  FCmdLine        := '';
  FDir            := '';
  FdoesUnderstandCntrlC := False;
  FisOEMCP        := True;
  FPollRate_ms    := 50;
  FRootKey        := HKEY_CURRENT_USER;
  s1 := Compilers[CompileIdx].FProgId;
  s1 := DelphiRegKey( s1);
  s1 := Format( '%s\Known Packages', [s1]);
  if s1[1] = '\' then
    Delete( s1, 1, 1);
  FSubKeyName     := s1;
  s1 := Manifests[ManifestIdx].FPackageName;
  s1 := ChangeFileExt( s1, '');
  if Manifests[ManifestIdx].FdoesOverrideDefault_BPL_DLL_Suffix then
      Suffix := Manifests[ManifestIdx].F_BPL_DLL_Suffix
    else
      Suffix := Compilers[CompileIdx].FDefault_BPL_DLL_Suffix;
  s1 := s1 + Suffix + '.bpl';
  s2 := BPL_DLL_Dir;
  if (s2 = '') or (s2[Length(s2)] <> '\') then
    s2 := s2 + '\';
  FValueName      := s2 + s1;
  s1 := Manifests[ManifestIdx].FDescription;
  if s1 = '' then
    s1 := '(Untitled)';
  FData := s1 
  end;
end;


procedure AddDeregisterPackageJob( const ManifestIdx: integer);
var
  CompileIdx, Idx: integer;
  s1, s2: string;
  Suffix: string;
begin
CompileIdx := FindCompilerOfManifest( ManifestIdx);
Idx := Length( Jobs);
SetLength( Jobs, Idx + 1);
with Jobs[Idx] do
  begin
  FJobKind := jobRemoveKey;
  FStatus  := statusQueued;
  FPackageManifestIdx := ManifestIdx;
  FCompilerIdx := CompileIdx;
  FisFinalJob  := False;
  s1 := Manifests[ManifestIdx].FPackageName;
  s2 := Compilers[CompileIdx].FDisplayName;
  s1 := Format( 'De-installing the design-time package %s with compiler %s', [s1,s2]);
  FJobDescription := s1;
  FCmdLine        := '';
  FDir            := '';
  FdoesUnderstandCntrlC := False;
  FisOEMCP        := True;
  FPollRate_ms    := 50;
  FRootKey        := HKEY_CURRENT_USER;
  s1 := Compilers[CompileIdx].FProgId;
  s1 := DelphiRegKey( s1);
  s1 := Format( '%s\Known Packages', [s1]);
  if s1[1] = '\' then
    Delete( s1, 1, 1);
  FSubKeyName     := s1;
  s1 := Manifests[ManifestIdx].FPackageName;
  s1 := ChangeFileExt( s1, '');
  if Manifests[ManifestIdx].FdoesOverrideDefault_BPL_DLL_Suffix then
      Suffix := Manifests[ManifestIdx].F_BPL_DLL_Suffix
    else
      Suffix := Compilers[CompileIdx].FDefault_BPL_DLL_Suffix;
  s1 := s1 + Suffix + '.bpl';
  s2 := BPL_DLL_Dir;
  if (s2 = '') or (s2[Length(s2)] <> '\') then
    s2 := s2 + '\';
  FValueName := s2 + s1
  end;
end;



function AddAppendDCUtoSearchPathJob( const CompileIdx: integer): boolean;
// Return True, only if we should really do this.
// Add DCU_Dir to Library path list for each active compiler. But only once.
//   HKCU\Software\CodeGear\BDS\7.0\Library\Search Path: semicolon separated list string
var       
  Idx: integer;    
  s1, s2: string;
  OriginalValue: string;
begin
Idx := Length( Jobs);
SetLength( Jobs, Idx + 1);
with Jobs[Idx] do
  begin
  FJobKind := jobSetKey;
  FStatus  := statusQueued;
  FPackageManifestIdx := -1;
  FCompilerIdx := CompileIdx;
  FisFinalJob  := False;
  s2 := Compilers[CompileIdx].FDisplayName;
  s1 := Format( 'Appending the DCU path to the %s Library Search Path list', [s2]);
  FJobDescription := s1;
  FCmdLine        := '';
  FDir            := '';
  FdoesUnderstandCntrlC := False;
  FisOEMCP        := True;
  FPollRate_ms    := 50;
  FRootKey        := HKEY_CURRENT_USER;
  s1 := Compilers[CompileIdx].FProgId;
  s1 := DelphiRegKey( s1);
  s1 := Format( '%s\Library', [s1]);
  if s1[1] = '\' then
    Delete( s1, 1, 1);
  FSubKeyName     := s1;
  FValueName      := 'Search Path';
  with Compilers[ CompileIdx] do
    begin
    result := RegQueryStringValue( FRootKey, FSubKeyName, FValueName, OriginalValue) and
              (Pos( F_DCU_Dir + ';', OriginalValue) = 0); // Do not add if redundant.
    FData  := OriginalValue + ';' + F_DCU_Dir
    if result and (Length( OriginalValue) >= ((Length( F_DCU_Dir) + 1))) then
      begin
      Delete( OriginalValue, 1, Length( OriginalValue) - Length( F_DCU_Dir) - 1);
      result := OriginalValue <> (';' + F_DCU_Dir) // Another check for redundancy.
      end
    end
  end;
if not result then
  SetLength( Jobs, Idx)
end;


procedure ConstructJobs;
var
  i, j: integer;
  CompileJobCount: integer;
begin
TempFiles.Clear;
SetLength( Jobs, 0);
CompileJobCount := 0;
if IsTaskSelected( 'Compile\Install') then
  for i := 0 to Length( Manifests) - 1 do
    with Manifests[i] do
      begin
      j := FindCompilerOfManifest(i);
      if (j <> -1) and Compilers[j].FActive and FisDesignTime then
        AddDeregisterPackageJob( i)
      end;
if IsTaskSelected( 'Compile') then
  for i := 0 to Length( Manifests) - 1 do
    with Manifests[i] do
      begin
      j := FindCompilerOfManifest(i);
      if (j <> -1) and Compilers[j].FActive then
        begin
        AddCfgJob( i);
        AddCompileJob( i);
        Inc( CompileJobCount);
        if FisDesignTime then
          AddRegisterPackageJob( i)
        end
      end;
if isTaskSelected( 'Compile\LibPath') then
  for i := 0 to Length( Compilers) - 1 do
    if Compilers[i].FActive then
      AddAppendDCUtoSearchPathJob( i);
if CompileJobCount > 0 then
  Create_EraseTempFilesJob;
JobCount := Length( Jobs);
ConcludedJobCount := 0;
CurrentJobIndex := -1
end;


function ExecuteRemoveKeyJob( var Job: TJob): boolean;
begin
with Job do
  RegDeleteValue( FRootKey, FSubKeyName, FValueName);
result := True;
end;


function ExecuteSetKeyJob( var Job: TJob): boolean;
begin
with Job do
  result := RegWriteStringValue( FRootKey, FSubKeyName, FValueName, FData)
end;                                               


function BeforeExecuteExternalJob( const Job: TJob): TWatcherList;
begin
SetLength( result, 0);
if (Job.FJobKind <> jobExternalProcess) or (Job.FCompilerIdx = -1) then exit;
SetLength( result, 3);
WatchForWorkProducts( result[0], Compilers[Job.FCompilerIdx].F_DCU_Dir, '*.dcu');
WatchForWorkProducts( result[1], Compilers[Job.FCompilerIdx].F_DCP_Dir, '*.dcp');
case Manifests[Job.FPackageManifestIdx].FPackageKind of
  pkBPL: WatchForWorkProducts( result[2], BPL_DLL_Dir, '*.bpl');
  pkDLL: WatchForWorkProducts( result[2], BPL_DLL_Dir, '*.dll');
  pkEXE: WatchForWorkProducts( result[2], Compilers[Job.FCompilerIdx].F_EXE_Dir, '*.exe');
  end;
end;
 




procedure AfterExecuteExternalJob( var Watchers: TWatcherList);
var
  j: integer;
begin
for j := 0 to Length( Watchers) - 1 do
  EndWatch( Watchers[j]);
SetLength( Watchers, 0);
end;



function ExecuteExternalProcess( var Job: TJob): boolean;
var
  WatchList: TWatcherList;
begin      
with Job do    
  begin              
  WatchList := BeforeExecuteExternalJob( Job);                                  
  result := ExecuteCommand(
    FCmdLine, FDir, FdoesUnderstandCntrlC, FisOEMCP, FPollRate_ms) = 0;
  if FPackageManifestIdx <> -1 then
    Manifests[FPackageManifestIdx].FCompiledOk := result;
  AfterExecuteExternalJob( WatchList)
  end
end;


procedure AddCfgLine( Cfg: TStrings; Name, Param: string; isPath: boolean);
var
  s: ansistring;
begin
if isPath and (Param <> '') and (Param[1] <> '"') then
  Param := '"' + Param + '"';
//s := '-' + Name + Param + #13#10;
//Cfg.Write( s, Length( s))
s := '-' + Name + Param;
Cfg.Add( s)
end;


procedure AddCfg_Conditional( Cfg: TStrings; Value: string);
begin
AddCfgLine( Cfg, 'D', Value, False)
end;

procedure AddCfg_BuildAll( Cfg: TStrings);
begin
AddCfgLine( Cfg, 'B', '', False)
end;

procedure AddCfg_Option( Cfg: TStrings; Name: string; Value: boolean);
begin
if Value then
    AddCfgLine( Cfg, Name, '+', False)
  else
    AddCfgLine( Cfg, Name, '-', False)
end;


procedure AddCfg_EXE_DLL_OutputDir( Cfg: TStrings; Dir: string);
begin
AddCfgLine( Cfg, 'E', Dir, True)
end;

procedure AddCfg_BPL_OutputDir( Cfg: TStrings; Dir: string);
begin
AddCfgLine( Cfg, 'LE', Dir, True)
end;

procedure AddCfg_IncludeDirs( Cfg: TStrings; Dirs: string);
begin
AddCfgLine( Cfg, 'I', Dirs, False)
end;

procedure AddCfg_DCP_OutputDir( Cfg: TStrings; Dir: string);
begin
AddCfgLine( Cfg, 'LN', Dir, True)
end;

procedure AddCfg_DCU_OutputDir( Cfg: TStrings; Dir: string);
begin
AddCfgLine( Cfg, 'N0', Dir, True)
end;

procedure AddCfg_UnitSearchDir( Cfg: TStrings; Dir: string);
begin
AddCfgLine( Cfg, 'U' , Dir, True)
end;

function BoolToStr( Value: boolean): string;
begin
if Value then
    result := 'True'
  else
    result := 'False'
end;

procedure ShowMessage( Msg: string);
begin
SuppressibleMsgBox( Msg, mbInformation, MB_OK, 0)
end;

function Create_DCC32CFG_File( var Manifest: TPackageManifest; LogNotes: TStrings): string;
var
  FN: string;
  Cfg: TStrings;
  CompilerIdx, j: integer;
  BPL_DLL_Suffix: string;
  isOpen: boolean;
begin
result := '';
CompilerIdx := FindCompilerOfManifest( Manifest.FIdx);
if (CompilerIdx = -1) or (not Compilers[CompilerIdx].FActive) then
  begin
  LogNotes.Add( Compilers[CompilerIdx].FDisplayName + ' compilation disabled.');
  exit
  end;
LogNotes.Add( Manifest.FPackageName + ':');
LogNotes.Add( '  Compiler = ' + Compilers[CompilerIdx].FDisplayName);
case Manifest.FPackageKind of
  pkBPL:
    begin
    LogNotes.Add( '  Type = Package (BPL output)');
    LogNotes.Add( '  RunTime = ' + BoolToStr( Manifest.FisRunTime));
    LogNotes.Add( '  DesignTime = ' + BoolToStr( Manifest.FisDesignTime));
    end;

  pkEXE:
    begin
    LogNotes.Add( '  Type = Application (EXE output)');
    end;

  pkDLL:
    begin
    LogNotes.Add( '  Type = Library (DLL output)');
    end;
  end;
if Manifest.FDescription <> '' then
  LogNotes.Add( '  Description = ' + Manifest.FDescription);
LogNotes.Add( '  Project directory = "' + Manifest.FDirectory + '"');
FN  := RemoveBackslash( Manifest.FDirectory) + '\' + 'dcc32.cfg';
isOpen := False;
{try
  if FileExists( FN) then
      begin
      Cfg := TFileStream.Create( FN, fmOpenReadWrite);
      isOpen := True;
      Cfg.Size := 0
      end
    else
      Cfg := TFileStream.Create( FN, fmCreate);
except
  begin
  if isOpen then
      begin
      Cfg.Free;
      LogNotes.Add( '  Error: Failed to rewrite file "' + FN + '"."');
      end
    else
      LogNotes.Add( '  Error: Failed to open file "' + FN + '"."');
  exit
  end end;
result := FN;}
Cfg := TStringList.Create;
try
{Actually use:
  -B
  -DSBD_DelphiComponentLibrary_Installer
  -E<Compilers[CompilerIdx].F_EXE_Dir>/<BPL_DLL_Dir>
  -I<INC_Dir>
  -LE<BPL_DLL_Dir>
  -LN<Compilers[CompilerIdx].F_DCP_Dir>
  -N0<Compilers[CompilerIdx].F_DCU_Dir> 
  -U<Compilers[CompilerIdx].F_DCU_Dir> 
  -W+
  -Z
  -Q
  -$D-H+L-O+W-X+Y-     }
  if Manifest.FdoesOverrideDefault_BPL_DLL_Suffix then
      BPL_DLL_Suffix := Manifest.F_BPL_DLL_Suffix
    else
      BPL_DLL_Suffix := Compilers[CompilerIdx].FDefault_BPL_DLL_Suffix;
  case Manifest.FPackageKind of
    pkBPL: LogNotes.Add( '  BPL suffix = ' + BPL_DLL_Suffix);
    pkDLL: LogNotes.Add( '  DLL suffix = ' + BPL_DLL_Suffix);
    pkEXE: begin end;
    end;
  AddCfg_BuildAll( Cfg); // -B
  AddCfg_Conditional( Cfg, 'SBD_DelphiComponentLibrary_Installer'); // -DSBD_DelphiComponentLibrary_Installer
  case Manifest.FPackageKind of
    pkBPL: begin
           AddCfg_EXE_DLL_OutputDir( Cfg, BPL_DLL_Dir);
           LogNotes.Add( '  BPL output dir = "' + BPL_DLL_Dir + '"');
           end;
    pkDLL: begin
           AddCfg_EXE_DLL_OutputDir( Cfg, BPL_DLL_Dir);
           LogNotes.Add( '  DLL output dir = "' + BPL_DLL_Dir + '"');
           end;
    pkEXE: begin
           AddCfg_EXE_DLL_OutputDir( Cfg, Compilers[CompilerIdx].F_EXE_Dir);
           LogNotes.Add( '  EXE output dir = "' + Compilers[CompilerIdx].F_EXE_Dir + '"');
           end;
    end;
  if INC_Dir <> '' then
    begin
    AddCfg_IncludeDirs( Cfg, INC_Dir); // -I<Paths>
    LogNotes.Add( '  Include directories = ' + INC_Dir);
    end;
  AddCfg_BPL_OutputDir( Cfg, BPL_DLL_Dir); // -LE<Path>

  AddCfg_DCP_OutputDir( Cfg, Compilers[CompilerIdx].F_DCP_Dir); // -LN<Path>
  LogNotes.Add( '  DCP output dir = "' + Compilers[CompilerIdx].F_DCP_Dir + '"');

  AddCfg_DCU_OutputDir( Cfg, Compilers[CompilerIdx].F_DCU_Dir); // -L0<Path>
  LogNotes.Add( '  DCU output dir = "' + Compilers[CompilerIdx].F_DCU_Dir + '"');

  if not Compilers[CompilerIdx].FRequiresDCP_in_U_SearchPath then
      AddCfg_UnitSearchDir( Cfg, Compilers[CompilerIdx].F_DCU_Dir) // -U<Path>     
    else
      // Special case: Delphi 7 needs to include the dcp directory in the -U command line.
      AddCfg_UnitSearchDir( Cfg, Compilers[CompilerIdx].F_DCU_Dir + ';' + Compilers[CompilerIdx].F_DCP_Dir);

  if Compilers[CompilerIdx].FRequiresNS then
    AddCfgLine( Cfg, 'NS', 'system;vcl;System.Win;WinAPI;Vcl.Imaging;Data', False);
  AddCfg_Option( Cfg, 'W', True); // -W+
  AddCfgLine( Cfg, 'Q', '', False);
  AddCfgLine( Cfg, 'Z', '', False);
  AddCfgLine( Cfg, '$D-H+L-O+W-X+Y-', '', False);
  try
    Cfg.SaveToFile( FN);
    result := FN
  except
    begin
    LogNotes.Add( '  Error: Failed to write file "' + FN + '"."')
    end end;
  LogNotes.Add('DEBUG INFO: Cfg Fn = ' + FN);
  LogNotes.Add('DEBUG INFO: Cfg content BEGIN');
  LogNotes.Add( Cfg.Text);
  LogNotes.Add('DEBUG INFO: Cfg content END');
finally
  Cfg.Free
  end
end;


function ExecuteCreateCfg( var Job: TJob): boolean;
var
  LogNotes: TStrings;
  FN: string;
begin
LogNotes := TStringList.Create;
try
FN := Create_DCC32CFG_File( Manifests[Job.FPackageManifestIdx], LogNotes);
result := FN <> '';
if result and (TempFiles.IndexOf( FN) = -1) then
  TempFiles.Add( FN);
LogMemo( lkUndecorated, LogNotes.Text);
finally
LogNotes.Free
end end;


function ExecuteEraseTempFiles( var Job: TJob): boolean;
var
  i: integer;
begin
result := True;
for i := 0 to TempFiles.Count - 1 do
  result := result and DeleteFile( TempFiles[i]);
TempFiles.Clear
end;


function ExecuteJob( var Job: TJob): boolean;
begin    
result := True;
Job.FStatus := statusInWork; // statusQueued, , statusSucceeded, statusFailed
LogMemo( lkCommandEcho, Job.FJobDescription);
case Job.FJobKind of
  jobRemoveKey      : result := ExecuteRemoveKeyJob( Job);
  jobSetKey         : result := ExecuteSetKeyJob( Job);
  jobExternalProcess: result := ExecuteExternalProcess( Job);
  jobCreateCfgFile  : result := ExecuteCreateCfg( Job);
  jobEraseTempFiles : result := ExecuteEraseTempFiles( Job);
  end;                                            
if result then
    begin
    Job.FStatus := statusSucceeded;
    LogMemo( lkDiagnostic, 'Job completed ok.')
    end
  else
    begin
    Job.FStatus := statusFailed;
    LogMemo( lkDiagnostic, Format( 'Job "%s" failed!', [Job.FJobDescription]))
    end
end;


procedure CompileOnClick( Sender: TObject);
var
  a1: ansistring;
  w1: string;
  Ok: boolean;
  i: integer;
begin
WizardForm.NextButton.Enabled := False;
try
ClearLog;
CheckDiag;
ConstructJobs;
Ok := True;
LogMemo( lkUndecorated, Format( 'Will now attempt to execute %d jobs.', [JobCount]));
LoadSpawnedFiles;
CmdBar.Max := JobCount;
CmdBar.Position := 0;
for i := 0 to Length( Jobs) - 1 do
    begin
    CurrentJobIndex := i;
    Ok := ExecuteJob( Jobs[i]);
    CheckDiag;
    Inc( ConcludedJobCount);
    CmdBar.Position := ConcludedJobCount;
    if not Ok then break
    end;
if not Ok then
  for i := CurrentJobIndex + 1 to Length( Jobs) - 1 do
    begin
    CurrentJobIndex := i;
    if not Jobs[i].FisFinalJob then continue;
    ExecuteJob( Jobs[i]);
    CheckDiag;
    Inc( ConcludedJobCount);
    CmdBar.Position := ConcludedJobCount
    end;
CmdBar.Position := CmdBar.Max;
if Ok then
    LogMemo( lkUndecorated, 'All jobs executed successfully')
  else
    LogMemo( lkUndecorated, 'At least one job failed. Processing of remaining jobs aborted.');
finally
WizardForm.NextButton.Enabled := True;
end;
SaveSpawnedFiles;
SpawnedFiles.Clear
end;


procedure AbortOnClick( Sender: TObject);
begin
CheckDiag;
DoAbort := True;
end;

procedure InitializeHelperOptions;
begin
doCommandLineDiagnostics := False;
SetDiagLogOn( doCommandLineDiagnostics);
CheckFor_Inno5_4_0u_ReturnAnsiString_Bug;
if not CheckFor_Inno5_4_0u_ReturnAnsiString_Bug then
  RaiseException( 'Problem calling helper DLL.')
end;


procedure InitiateManifests;
begin
SetLength( Manifests, 0)
end;


procedure InitiateJobs;
begin
TempFiles := TStringList.Create;
SetLength( Jobs, 0);
JobCount := 0;
ConcludedJobCount := 0;
CurrentJobIndex := -1;
SpawnedFiles := TStringList.Create
end;


procedure InitiateCompilers;
var
  Idx: integer;
begin
SetLength( Compilers, 10);
with Compilers[0] do
  begin
  FProgId := 'D5';
  FDisplayName := 'Delphi 5';
  FRequiresDCP_in_U_SearchPath := True
  end;
with Compilers[1] do
  begin
  FProgId := 'D6';
  FDisplayName := 'Delphi 6';
  FRequiresDCP_in_U_SearchPath := True
  end;
with Compilers[2] do
  begin
  FProgId := 'D7';
  FDisplayName := 'Delphi 7';
  FRequiresDCP_in_U_SearchPath := True
  end;
with Compilers[3] do
  begin
  FProgId := 'D2005';
  FDisplayName := 'Delphi 2005'
  end;
with Compilers[4] do
  begin
  FProgId := 'D2006';
  FDisplayName := 'Delphi 2006'
  end;
with Compilers[5] do
  begin
  FProgId := 'D2007';
  FDisplayName := 'Delphi 2007'
  end;
with Compilers[6] do
  begin
  FProgId := 'D2009';
  FDisplayName := 'Delphi 2009'
  end;
with Compilers[7] do
  begin
  FProgId := 'D2010';
  FDisplayName := 'Delphi 2010'
  end;
with Compilers[8] do
  begin
  FProgId := 'DXE';
  FDisplayName := 'Delphi XE'
  end;
with Compilers[9] do
  begin
  FProgId := 'DXE2';
  FDisplayName := 'Delphi XE2';
  FRequiresNS := True
  end;

for Idx := 0 to Length( Compilers) - 1 do
  with Compilers[Idx] do
    begin
    FWin32LibKey := 'Library';
    Fdcc32exe_FileName := dcc32( FProgId);
    FhasInstalled := (Fdcc32exe_FileName <> '') and FileExists( Fdcc32exe_FileName);
     // Is the compiler even installed on the host system?
    FisUserSelected := False; // Is the compiler selected by the user for package installation?
    FisEnabledForThisScript := isCompilerActiveForThisScript( FProgId);
      // Some scripts may only be interested in a subrange of compilers.
    FActive := FhasInstalled and FisUserSelected and FisEnabledForThisScript;
    // The following are set later in the game.
    F_DCU_Dir := '';;
    F_DCP_Dir := '';
    FDefault_BPL_DLL_Suffix := '_' + Lowercase( FProgId);
    F_EXE_Dir := ''
    end;
Compilers[8].FWin32LibKey := 'Library\Win32';
Compilers[9].FWin32LibKey := 'Library\Win32';
end;


procedure SBD_InitializeWizard;
var
  Button: TNewButton;
  j, Idx: integer;
  PageId: integer;
  Label1: TNewStaticText;
begin      
FirstTime_ExitedSelectDirPage := True;
PrevPageID := -1;
InitializeHelperOptions;
InitiateManifests;
InitiateCompilers;
InitiateJobs;
SetLength( KindPrefix, 4);
KindPrefix[0] := ''; // lkUndecorated
KindPrefix[1] := ''; // lkProcessOutput
KindPrefix[2] := 'Cmd'; // lkCommandEcho
KindPrefix[3] := 'Diag'; // lkDiagnostic
 
SelectCompilerPage := CreateInputOptionPage( wpSelectTasks, 'Compilers',
  'What compilers do you wish to compile/install for?',
  'The following is a list of Delphi for win32 compilers installed on your system.' +
   ' Check those that you want this component library to be installed for.' +
   ' Uncheck if you don''t want the library to be installed for that compiler.',
  False, True);
// SelectCompilerPage.CheckListBox.AllowGrayed := False;
for j := 0 to Length( Compilers) - 1 do
  with Compilers[j] do
    begin
    if FhasInstalled and FisEnabledForThisScript then
        begin
        Idx := SelectCompilerPage.Add( FDisplayName);
        FisUserSelected := True;
        SelectCompilerPage.Values[ Idx] := FisUserSelected;
        FWizardPageIdx := Idx
        end
      else
        begin
        FisUserSelected := False;
        FWizardPageIdx := -1
        end;    
    FActive := FhasInstalled and FisEnabledForThisScript and FisUserSelected
    end;

CommonDirsPage := CreateInputDirPage( SelectCompilerPage.Id,
  'Directories',
  'Common-to-all-compilers BPL/DLL output dir',
  'The BPL/DLL output directory must be existant and on the system PATH.'#13#10#13#10 +
  'You may also identify a semi-colon separated list of searchable directories for Delphi include (.inc) files.' +
//  'The .inc directories list may be empty. If an include directory contains a space, then list item should be ' +
//  'delimited by double quotes (").'
  ''#13#10#13#10 +
  'NOTE: Environment variable expansion (like $(BDS) ) is not yet supported.',
  False, '');
CommonDirsPage.Add( 'BPL/DLL output directory:');
// CommonDirsPage.Add( 'Include (.inc) directories');

//Label1 := TNewStaticText.Create( CommonDirsPage);
//Label1.Top     := CommonDirsPage.Edits[0].Top + CommonDirsPage.Edits[0].Height + 16;
//Label1.Left    := CommonDirsPage.Edits[0].Left;
//Label1.Caption := 'Include (.inc) directories:';
//Label1.Parent  := CommonDirsPage.Surface;

//IncEdit := TNewEdit.Create( CommonDirsPage);
//IncEdit.Top    := Label1.Top + Label1.Height + 3;
//IncEdit.Left   := CommonDirsPage.Edits[0].Left;
//IncEdit.Width  := CommonDirsPage.Edits[0].Width;
//IncEdit.Parent := CommonDirsPage.Surface;

FirstCompilerDirsPageId := -1;
LastCompilerDirsPageId  := -1;
PageId := CommonDirsPage.Id;
for j := 0 to Length( Compilers) - 1 do
  with Compilers[j] do
    begin
    if FhasInstalled and FisEnabledForThisScript then
        begin
        FDirsPage := CreateInputDirPage( PageId,
          'Directories',
          FDisplayName + ' DCU, DCP and EXE output directories for Win32 target.',
          'These directories must be existant.'#13#10#13#10 +
          'NOTE: Environment variable expansion (like $(BDS) ) is not yet supported.',
          False, '');
        PageId := FDirsPage.ID;
        if FirstCompilerDirsPageId = -1 then
          FirstCompilerDirsPageId := PageId;
        LastCompilerDirsPageId := PageId;
        FDirsPage.Add( FDisplayName + ' DCU Output directory:');
        FDirsPage.Add( FDisplayName + ' DCP Output directory:');
        FDirsPage.Add( FDisplayName + ' EXE Output directory:');
        end
      else
        FDirsPage := nil
    end;

CmdPage := CreateCustomPage( wpReady, 'Command Line', 'Execute something console.');
 
CmdEdit := TNewEdit.Create( CmdPage);
CmdEdit.Top    := 0;
CmdEdit.Left   := 0;
CmdEdit.Width  := CmdPage.SurfaceWidth;
CmdEdit.Text   := '"C:\Program Files\CodeGear\RAD Studio\5.0\bin\dcc32"';
CmdEdit.Parent := CmdPage.Surface;

DirEdit := TNewEdit.Create( CmdPage);
DirEdit.Top    := CmdEdit.Top + CmdEdit.Height + ScaleY(8);
DirEdit.Left   := 0;
DirEdit.Width  := CmdPage.SurfaceWidth;
DirEdit.Text   := 'C:\Program Files\Common Files\BPL';
DirEdit.Parent := CmdPage.Surface;

Button := TNewButton.Create( CmdPage);
Button.Top    := DirEdit.Top + DirEdit.Height + ScaleY(8);
Button.Left   := 0;
Button.Width  := ScaleX( 75);
Button.Height := ScaleY( 23);
Button.Caption := 'Execute';
Button.OnClick := @ExecuteOnClick;
Button.Parent := CmdPage.Surface;

Button := TNewButton.Create( CmdPage);
Button.Top    := DirEdit.Top + DirEdit.Height + ScaleY(8);
Button.Left   := ScaleX( 100);
Button.Width  := ScaleX( 75);
Button.Height := ScaleY( 23);
Button.Caption := 'Abort';
Button.OnClick := @AbortOnClick;
Button.Parent := CmdPage.Surface;
AbortButton   := Button;
AbortButton.Enabled := False;

Memo := TNewMemo.Create( CmdPage);
Memo.Top := Button.Top + Button.Height + ScaleY(8);
Memo.Width := CmdPage.SurfaceWidth;
Memo.Height := CmdPage.SurfaceHeight - Memo.Top;
Memo.ScrollBars := ssVertical;
Memo.Text := 'TNewMemo';
Memo.Parent := CmdPage.Surface;

CompilePage := CreateCustomPage( wpInstalling, 'Compile', 'Compile packages and install design-time packages where required.');
 
CmdBar := TNewProgressBar.Create( CompilePage);
CmdBar.Top      := 0;
CmdBar.Left     := 0;
CmdBar.Width    := CmdPage.SurfaceWidth;
CmdBar.Min      := 0;
CmdBar.Max      := 10;
CmdBar.Position := 0;
CmdBar.Parent   := CompilePage.Surface;

Button := TNewButton.Create( CompilePage);
Button.Top    := CmdBar.Top + CmdBar.Height + ScaleY(8);
Button.Left   := 0;
Button.Width  := ScaleX( 75);
Button.Height := ScaleY( 23);
Button.Caption := 'Compile';
Button.OnClick := @CompileOnClick;
Button.Parent := CompilePage.Surface;

CompileInstruction := TLabel.Create( CompilePage);
CompileInstruction.Caption := 'Press the "Compile" button to begin compilation!';
CompileInstruction.Left    := Button.Left + Button.Width + 10;
CompileInstruction.Top     := Button.Top;
CompileInstruction.Parent := CompilePage.Surface;

Memo2 := TNewMemo.Create( CompilePage);
Memo2.Top := Button.Top + Button.Height + ScaleY(8);
Memo2.Width := CompilePage.SurfaceWidth;
Memo2.Height := CompilePage.SurfaceHeight - Memo.Top;
Memo2.ScrollBars := ssBoth;
Memo2.Text := '';
Memo2.Parent := CompilePage.Surface;
Memo2.ReadOnly := True;

BPL_DLL_Dir := GetPreviousData( 'BPL_dir', '');  
CommonDirsPage.Values[0] := BPL_DLL_Dir;
//INC_Dir := GetPreviousData( 'INC_dir', '');
//IncEdit.Text := INC_Dir;
for j := 0 to Length( Compilers) - 1 do
  with Compilers[j] do
    begin
    if not FisEnabledForThisScript then continue;
    F_DCU_Dir := GetPreviousData( FProgId + '.DCU_dir', '');
    F_DCP_Dir := GetPreviousData( FProgId + '.DCP_dir', '');
    F_EXE_Dir := GetPreviousData( FProgId + '.EXE_dir', '');  // Default to {app} when it becomes available.
    if assigned( FDirsPage) then
      begin
      FDirsPage.Values[0] := F_DCU_Dir;
      FDirsPage.Values[1] := F_DCP_Dir;
      FDirsPage.Values[2] := F_EXE_Dir;
      end
    end;
end;


function Between( OpenPattern, ClosePattern, Carrier: ansistring): ansistring;
var
  P: integer;
  s: ansistring;
begin
s := Carrier;
P := Pos( OpenPattern, s);
if P > 0 then
  Delete( s, 1, P - 1 + Length( OpenPattern));
P := Pos( ClosePattern, s);
if P > 0 then
  SetLength( s, P - 1);
result := s
end;

function IntegerBetween( OpenPattern, ClosePattern, Carrier: ansistring): integer;
begin
result := StrToIntDef( Between( OpenPattern, ClosePattern, Carrier), 0)
end;

function VersionInfoPiece( Carrier: ansistring; Property1: ansistring): integer;
begin
result := IntegerBetween( '<VersionInfo Name="' + Property1 + '">',
  '</VersionInfo>', Carrier)
end;

var AppVersion: string;

function ComputeAppVersion( Value: string): string;
// This function extracts the intended file version info from an MS-Build
//  2003 Schema file such as a Delphi 2007+ *.dproj file.
{Example content is as follows:
					<VersionInfo>
						<VersionInfo Name="IncludeVerInfo">True</VersionInfo>
						<VersionInfo Name="AutoIncBuild">False</VersionInfo>
						<VersionInfo Name="MajorVer">3</VersionInfo>
						<VersionInfo Name="MinorVer">1</VersionInfo>
						<VersionInfo Name="Release">4</VersionInfo>
						<VersionInfo Name="Build">0</VersionInfo>
						<VersionInfo Name="Debug">False</VersionInfo>
						<VersionInfo Name="PreRelease">False</VersionInfo>
						<VersionInfo Name="Special">False</VersionInfo>
						<VersionInfo Name="Private">False</VersionInfo>
						<VersionInfo Name="DLL">False</VersionInfo>
						<VersionInfo Name="Locale">3081</VersionInfo>
						<VersionInfo Name="CodePage">1252</VersionInfo>
					</VersionInfo>  }
var
  s: ansistring;
  P: integer;
  Pattern: ansistring;
  MajorVer, MinorVer, Release, Build: integer;
  MSBuildSchemeFN: string;
begin
if AppVersion = '' then
    begin
    MSBuildSchemeFN := GetMSBuildSchemeFileName;
    ExtractTemporaryFile( MSBuildSchemeFN);
    if not LoadStringFromFile( ExpandConstant( '{tmp}\' + MSBuildSchemeFN), s) then
      s := '';
    s := Between( '<VersionInfo>', '', s);
    MajorVer := VersionInfoPiece( s, 'MajorVer');
    MinorVer := VersionInfoPiece( s, 'MinorVer');
    Release  := VersionInfoPiece( s, 'Release');
    Build    := VersionInfoPiece( s, 'Build');
    result   := Format( '%d.%d.%d', [MajorVer, MinorVer, Release]);
    AppVersion := result
    end
  else
    result := AppVersion
end;


function ShouldDo_SelectCompilerPage: boolean;
begin
result := IsTaskSelected( 'Compile') and
          (SelectCompilerPage.CheckListBox.Items.Count >= 0)
end;

function ShouldDo_CompilePage: boolean;
begin
result := IsTaskSelected( 'Compile') and
          (SelectCompilerPage.CheckListBox.Items.Count >= 0)
end;


function ShouldDo_CommandPage: boolean;
begin
result := False // This page only for debug.
end;


function ShouldDo_CommonDirsPage: boolean;
begin
result := IsTaskSelected( 'Compile') and
          (SelectCompilerPage.CheckListBox.Items.Count >= 0)
end;


function ShouldDo_CompilerDirsPage( const Compiler: TCompiler): boolean;
begin
result := IsTaskSelected( 'Compile') and
   Compiler.FhasInstalled and Compiler.FisEnabledForThisScript and
   SelectCompilerPage.Values[ Compiler.FWizardPageIdx]
end;


function SBD_ShouldSkipPage( PageID: Integer): Boolean;
var
  j: integer;
begin
result := False;
case PageId of
  SelectCompilerPage.ID: result := not ShouldDo_SelectCompilerPage;
  CommonDirsPage.ID:     result := not ShouldDo_CommonDirsPage;
  CompilePage.ID:        result := not ShouldDo_CompilePage;  
  CmdPage.ID:            result := not ShouldDo_CommandPage;
  end;
if (FirstCompilerDirsPageId <> -1) and (PageId >= FirstCompilerDirsPageId) and
                                       (PageId <= LastCompilerDirsPageId) then
    begin
    result := True;
    for j := 0 to Length( Compilers) - 1 do
      begin
      if (not assigned(Compilers[j].FDirsPage)) or
         (Compilers[j].FDirsPage.ID <> PageID) then continue;
      result := not ShouldDo_CompilerDirsPage( Compilers[j])
      break
      end;
    end
end;


procedure AfterIsShown_SelectCompilerPage;
begin
end;


procedure AfterIsShown_CommonDirsPage;
var
  Idx, j: integer;
begin
//if BPL_DLL_Dir <> '' then exit;
Idx := -1;
for j := 0 to Length( Compilers) - 1 do
  with Compilers[j] do
    begin
    if FhasInstalled and FisEnabledForThisScript then
        begin
        Inc( Idx);
        if SelectCompilerPage.Values[ Idx] then
          HighestCompilerIdx := j
        end
    end;
if (HighestCompilerIdx <> -1) and
    RegQueryStringValue( HKEY_CURRENT_USER,
      DelphiRegKey( Compilers[HighestCompilerIdx].FProgId) + '\' + Compilers[HighestCompilerIdx].FWin32LibKey,
        'Package DPL Output', BPL_DLL_Dir) then
  begin
  if Pos('$',BPL_DLL_Dir) = 1 then
    begin
    if RegQueryStringValue( HKEY_CURRENT_USER,
      DelphiRegKey( Compilers[HighestCompilerIdx].FProgId) + '\Environment Variables',
        'BPL_32', BPL_DLL_Dir) then
       Idx := 3
    end;
  CommonDirsPage.Values[0] := BPL_DLL_Dir;
  end
end;


procedure AfterIsShown_CommandPage;
begin
end;


procedure AfterIsShown_CompilerDirsPage( var Compiler: TCompiler);
begin
end;


procedure Exit_SelectDirPage;
// We have just exited the wpSelectDir page. {app} is now available,
//  probably for the first time.
var
  CurAppDir, PrevAppDir: string;
  j: integer;
begin
CurAppDir := WizardDirValue;
if not RegQueryStringValue( HKEY_LOCAL_MACHINE,
           'SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\'+GetAppId('')+'_is1',
           'Inno Setup: App Path', PrevAppDir) then
  PrevAppDir := '';
for j := 0 to Length( Compilers) - 1 do
  with Compilers[j] do
    begin
    if not FisEnabledForThisScript then exit;
    if  (F_EXE_Dir = '') or
        (
         (F_EXE_Dir =  PrevAppDir) and      // Previous EXE dir linked to previous App dir.
         (F_EXE_Dir <> CurAppDir) and       // But the user has changed the App dir.
         FirstTime_ExitedSelectDirPage) then // Rule only applies at the first opportunity
                                             // for AppDir to be validly defined.
           begin
           // Discard the initial value of F_EXE_Dir (being the previous install value)
           //  as not helpful. Instead initialise F_EXE_Dir with the first user selection
           //  of AppDir.
           F_EXE_Dir := CurAppDir;
           if assigned( FDirsPage) then
             FDirsPage.Values[2] := F_EXE_Dir
           end
    end;
FirstTime_ExitedSelectDirPage := False
end;

procedure AfterIsShow_CompilePage;
begin
WizardForm.NextButton.Enabled := False;
end;

procedure SBD_CurPageChanged( CurPageID: Integer);
var
  j: integer;
begin
if (PrevPageID <> -1) and (PrevPageID <> CurPageID) then
  begin
  // Process exit from previous page.
  if (PrevPageId <= wpSelectDir) and (CurPageID > wpSelectDir) then
    // Exit the page, or at worst, skip over it for some reason.
    Exit_SelectDirPage;
  end;
case CurPageId of
  SelectCompilerPage.ID: AfterIsShown_SelectCompilerPage;
  CommonDirsPage.ID:     AfterIsShown_CommonDirsPage;
  CmdPage.ID:            AfterIsShown_CommandPage;
  CompilePage.ID:        AfterIsShow_CompilePage;
  end;
if (FirstCompilerDirsPageId <> -1) and (CurPageId >= FirstCompilerDirsPageId) and
                                       (CurPageId <= LastCompilerDirsPageId) then
    begin
    for j := 0 to Length( Compilers) - 1 do
      begin
      if (not assigned(Compilers[j].FDirsPage)) or
         (Compilers[j].FDirsPage.ID <> CurPageID) then continue;
      AfterIsShown_CompilerDirsPage( Compilers[j])
      break
      end;
    end;
PrevPageID := CurPageID;
end;


function SBD_UpdateReadyMemo(
  Space, NewLine, MemoUserInfoInfo, MemoDirInfo,
  MemoTypeInfo, MemoComponentsInfo, MemoGroupInfo, MemoTasksInfo:
  String): String;
var
  j: integer;
begin
for j := 0 to Length( Compilers) - 1 do
  with Compilers[j] do
    begin
    FisUserSelected := (FWizardPageIdx <> -1) and SelectCompilerPage.Values[ FWizardPageIdx]
    FActive := FhasInstalled and FisUserSelected and FisEnabledForThisScript;
    if assigned( FDirsPage) then
      begin
      F_DCU_Dir := FDirsPage.Values[0];
      F_DCP_Dir := FDirsPage.Values[1];
      F_EXE_Dir := FDirsPage.Values[2]; // Defaults to {app} for all compilers.
      end;
    end;                  
BPL_DLL_Dir := CommonDirsPage.Values[0];
//INC_Dir     := IncEdit.Text;
result := '';
if MemoDirInfo <> '' then
  result := result + MemoDirInfo + NewLine + NewLine;
if MemoTypeInfo <> '' then
  result := result + MemoTypeInfo + NewLine + NewLine;
if MemoComponentsInfo <> '' then
  result := result + MemoComponentsInfo + NewLine + NewLine;
if MemoGroupInfo <> '' then
  result := result + MemoGroupInfo + NewLine + NewLine;
if MemoTasksInfo <> '' then
  result := result + MemoTasksInfo + NewLine + NewLine;
if IsTaskSelected( 'Compile') then
  begin
  result := result + 'Compilers selected:' + NewLine;
  for j := 0 to Length( Compilers) - 1 do
    with Compilers[j] do
      begin
      if not FisUserSelected then continue;
      result := result + Space + FDisplayName + NewLine;
      end;
  result := result + NewLine;           
  result := result + 'BPL/DLL Output dir:' + NewLine + Space + '"' + BPL_DLL_Dir + '"' + NewLine + NewLine;
  if INC_Dir <> '' then
    result := result + 'INC search directories:' + NewLine + Space + INC_Dir + NewLine + NewLine;
  for j := 0 to Length( Compilers) - 1 do
    with Compilers[j] do
      begin
      if not FisUserSelected then continue;
      result := result + FDisplayName + ' directories:' + NewLine;
      result := result + Space + 'DCU Output dir: "' + F_DCU_Dir + '"' + NewLine;
      result := result + Space + 'DCP Output dir: "' + F_DCP_Dir + '"' + NewLine;
      result := result + Space + 'EXE Output dir: "' + F_EXE_Dir + '"' + NewLine;
      result := result + NewLine;           
      end;
  end
end;



procedure SBD_RegisterPreviousData( PreviousDataKey: Integer);
var
  j: integer;
begin
SetPreviousData( PreviousDataKey, 'BPL_dir', BPL_DLL_Dir);
SetPreviousData( PreviousDataKey, 'INC_dir', INC_Dir);
for j := 0 to Length( Compilers) - 1 do
  with Compilers[j] do
    begin
    if not FActive then continue;
    SetPreviousData( PreviousDataKey, FProgId + '.DCU_dir', F_DCU_Dir);
    SetPreviousData( PreviousDataKey, FProgId + '.DCP_dir', F_DCP_Dir);
    SetPreviousData( PreviousDataKey, FProgId + '.EXE_dir', F_EXE_Dir);
    end                  
end;



  function ReadCh( SourceCode: TStrings; var LineIdx: integer; var Line: string;
                   var PosIdx: integer; var Ch: Char): boolean;
  begin
  result := True;
  repeat
    if PosIdx <= Length( Line) then
        begin
        Ch := Line[ PosIdx];
        Inc( PosIdx);
        end
      else if PosIdx = (Length( Line) + 1) then
        begin
        Ch := #13;
        Inc( PosIdx)
        end
      else if LineIdx <= (SourceCode.Count - 2) then
        begin
        Inc( LineIdx);
        Line := SourceCode[ LineIdx];
        PosIdx := 1;
        continue
        end
      else
        begin
        Ch := #0;
        result := False
        end;
  until True
  end;

  function PeekCh( SourceCode: TStrings; LineIdx: integer; Line: string;
                   PosIdx: integer; var Ch: Char): boolean;
  var
    PeekLineIdx: integer;
    PeekLine: string;
    PeekPosIdx: integer;
  begin
  PeekLineIdx := LineIdx;
  PeekLine := Line;
  PeekPosIdx := PosIdx;
  result := True;
  repeat
    if PeekPosIdx <= Length( PeekLine) then
        Ch := PeekLine[ PeekPosIdx]
      else if PeekPosIdx = (Length( PeekLine) + 1) then
        Ch := #13
      else if PeekLineIdx <= (SourceCode.Count - 2) then
        begin
        Inc( PeekLineIdx);
        PeekLine := SourceCode[ PeekLineIdx];
        PeekPosIdx := 1;
        continue
        end
      else
        begin
        Ch := #0;
        result := False
        end;
  until True
  end;

function isLowercaseLetter( Ch: char): boolean;
begin
result := (Ch >= 'a') and (Ch <= 'z')
end;

function isUppercaseLetter( Ch: char): boolean;
begin
result := (Ch >= 'A') and (Ch <= 'Z')
end;

function isLetter( Ch: char): boolean;
begin
result := isLowercaseLetter( Ch) or isUppercaseLetter( Ch)
end;

function isDigit( Ch: char): boolean;
begin
result := (Ch >= '0') and (Ch <= '9')
end;

function isLetterOrDigit( Ch: char): boolean;
begin
result := isLetter( Ch) or isDigit( Ch)
end;

function ComputeNextToken(
  SourceCode: TStrings; LineIdx: integer; Line: string;
  PosIdx: integer; Ch: Char): TDelphiTokenType;
// Assume state is Null, Word or WhiteSpace.
var
  NextCh: Char;
begin
if not PeekCh( SourceCode, LineIdx, Line, PosIdx, NextCh) then
  NextCh := #0;
if (Ch = ' ') or (Ch = #9) or (Ch = #13) then
    result := tokenWhiteSpace
  else if Ch = '''' then
    result := tokenLiteralString
  else if (Ch = '{') and (NextCh = '$') then
    result := tokenDirective
  else if Ch = '{' then
    result := tokenCommentBrace
  else if (Ch = '(') and (NextCh = '*') then
    result := tokenCommentStar
  else if (Ch = '/') and (NextCh = '/') then
    result := tokenCommentSlash
  else if isLetter( Ch) then
    result := tokenIdentifier
  else
    result := tokenWord
end;



procedure AddToken( var Tokens: TDelphiTokenList; var TokenType: TDelphiTokenType; Next: TDelphiTokenType; var Value: string);
var
  Idx: integer;
begin
if TokenType <> TokenNull then
  begin
  Idx := Length( Tokens);
  SetLength( Tokens, Idx + 1);
  with Tokens[ Idx] do
    begin
    FTokenType := TokenType;
    FValue := Value
    end
  end;
TokenType := Next;
Value     := ''
end;


function ParseDelphiFile( SourceCode: TStrings): TDelphiTokenList;
var
  LineIdx: integer;
  Line: string;
  PosIdx: integer;
  Ch, NextCh: Char;
  State: TDelphiTokenType;
  NextToken: TDelphiTokenType;
  Value: string;

begin
SetLength( result, 0);
if SourceCode.Count = 0 then exit;
State := tokenNull;
LineIdx := 0;
Line := SourceCode[ 0];
PosIdx := 1;
Value := '';
while ReadCh( SourceCode, LineIdx, Line, PosIdx, Ch) do
  case State of
    tokenWord, tokenIdentifier, tokenNull:          // Keywords, identifiers, operators etc.
      begin
      NextToken := ComputeNextToken( SourceCode, LineIdx, Line, PosIdx, Ch);
      if (State = tokenIdentifier) and isDigit( Ch) then
        NextToken := tokenIdentifier;
      if NextToken = State then
          Value := Value + Ch
        else if (State = tokenNull) and ((NextToken = tokenWord) or (NextToken = tokenIdentifier)) then
          begin
          Value := Ch;
          State := NextToken
          end
        else
          begin
          AddToken( result, State, NextToken, Value);
          if State = tokenWhiteSpace then
              begin
              if Ch = #13 then
                  Value := #13#10
                else
                  Value := Ch
              end
            else if (State = tokenIdentifier) or (State = tokenWord) then
              Value := Ch;
          if (State = tokenCommentStar) or (State = tokenCommentSlash) or (State = tokenDirective) then
          // "if State in [tokenCommentStar, tokenCommentSlash, tokenDirective] then" not working due to bug in Inno.
            if not ReadCh( SourceCode, LineIdx, Line, PosIdx, Ch) then
              break
          end
      end;

    tokenWhiteSpace:
      begin
      NextToken := ComputeNextToken( SourceCode, LineIdx, Line, PosIdx, Ch);
      if NextToken = tokenWhiteSpace then
          begin
          if Ch = #13 then
              Value := Value  + #13#10
            else
              Value := Value + Ch
          end
        else
          begin
          AddToken( result, State, NextToken, Value);
          if (State = tokenWord) or (State = tokenIdentifier) then
              Value := Ch;
          if (State = tokenCommentStar) or (State = tokenCommentSlash) or (State = tokenDirective) then
            if not ReadCh( SourceCode, LineIdx, Line, PosIdx, Ch) then
              break
          end
      end;

    tokenCommentBrace:  // { Comment }       May be multi-line comment.
      begin
      if Ch = '}' then
          AddToken( result, State, tokenNull, Value)
        else if Ch = #13 then
          Value := Value + #13#10
        else
          Value := Value + Ch
      end;

    tokenDirective:     // {$DIRECTIVE param param ...} or {$DIRECTIVE}
      begin   // Like tokenCommentBrace, but muliline is forbidden.
      if (Ch = '}') or (Ch =#13) then
          AddToken( result, State, tokenNull, Value)
        else
          Value := Value + Ch
      end;

    tokenCommentStar:   // (* Comment *)     May be multi-line comment.
      begin
      if (Ch = '*') and PeekCh( SourceCode, LineIdx, Line, PosIdx, NextCh) and (NextCh = ')') then
          begin
          AddToken( result, State, tokenNull, Value)
          if not ReadCh( SourceCode, LineIdx, Line, PosIdx, Ch) then
            break
          end
        else if Ch = #13 then
          Value := Value + #13#10
        else
          Value := Value + Ch
      end;

    tokenCommentSlash:  // // Comment        Single line comment only.
      begin
      if Ch = #13 then
          AddToken( result, State, tokenNull, Value)
        else
          Value := value + Ch
      end;

    tokenLiteralString: // 'You''r in'       Single line only.
      begin
      if (Ch = '''') and PeekCh( SourceCode, LineIdx, Line, PosIdx, NextCh) and (NextCh = '''') then
        begin
        Value := Value + '''';
        ReadCh( SourceCode, LineIdx, Line, PosIdx, Ch)
        end
      else if (Ch = '''') or (Ch = #13) then
        AddToken( result, State, tokenNull, Value)
      else
        Value := Value + Ch
      end;
    end;
// EOF reached now. Now to tidy up.
AddToken( result, State, tokenNull, Value)
end; 



function FirstWord( Value: string; var Remainder: string): string;
var
  P: integer;
begin
Remainder := Value;
while (Remainder <> '') and (Remainder[1] = ' ') do
  Delete( Remainder, 1, 1);
P := Pos( ' ', Remainder);
if P = 0 then
    begin
    result := Remainder;
    Remainder := ''
    end
  else
    begin
    result := Copy( Remainder, 1, P-1);
    Delete( Remainder, 1, P)
    end
end;


function ExtractFirstCompilerDirective( Tokens: TDelphiTokenList; Directive: string; var Params: string): boolean;
var
  UpperDirective: string;
  Remainder, Token: string;
  Idx: integer;
begin
Params := '';
result := False;
UpperDirective := UpperCase( Directive);
for Idx := 0 to Length( Tokens) - 1 do
  begin
  if Tokens[Idx].FTokenType <> tokenDirective then continue;
  Token := UpperCase( FirstWord( Tokens[Idx].FValue, Remainder));
  result := Token = UpperDirective;
  if not result then continue;
  Params := Remainder;
  break
  end;
end;


function ExtractMustHaveDirective( Tokens: TDelphiTokenList; Directive: string): string;
begin
if not ExtractFirstCompilerDirective( Tokens, Directive, result) then
  result := ''
end;


function ExtractDescription( Tokens: TDelphiTokenList): string;
begin
result := ExtractMustHaveDirective( Tokens, 'DESCRIPTION')
end;


function ExtractLibSuffix( Tokens: TDelphiTokenList; var Suffix: string): boolean;
var
  L: integer;
begin
result := ExtractFirstCompilerDirective( Tokens, 'LIBSUFFIX', Suffix);
L := Length( Suffix);
if (L > 0) and (Suffix[L] = '''') then
  SetLength( Suffix, L-1);
if (L > 1) and (Suffix[1] = '''') then
  Delete( Suffix, 1, 1) 
end;

function ExtractIsRunTime( Tokens: TDelphiTokenList): boolean;
var
  Dummy: string;
begin
result := ExtractFirstCompilerDirective( Tokens, 'RUNONLY'   , Dummy) or
     (not ExtractFirstCompilerDirective( Tokens, 'DESIGNONLY', Dummy))
end;


function ExtractIsDesignTime( Tokens: TDelphiTokenList): boolean;
var
  Dummy: string;
begin
result := ExtractFirstCompilerDirective( Tokens, 'DESIGNONLY', Dummy) or
     (not ExtractFirstCompilerDirective( Tokens, 'RUNONLY'   , Dummy))
end;


function ExtractFirstKeyWord( Tokens: TDelphiTokenList): string;
var
  Idx: integer;
begin
result := '';
for Idx := 0 to Length( Tokens) - 1 do
  begin
  if Tokens[Idx].FTokenType <> tokenIdentifier then continue;
  result := LowerCase( Tokens[Idx].FValue);
  break
  end
end;

function ExtractIsLibrary( Tokens: TDelphiTokenList): boolean;
begin
result := ExtractFirstKeyWord( Tokens) = 'library'
end;


procedure PackageDeployed( Directory, PackageName, Compiler: string);
var                       
  FN, Description: string;
  LibSuffix: string;
  PackageSource: TStrings;
  Idx: integer;
  Tokens: TDelphiTokenList;
  Ext: string;
begin
Directory := ExpandConstant( Directory);
FN  := RemoveBackslash( Directory) + '\' + PackageName;
Ext := LowerCase( ExtractFileExt( PackageName));
PackageSource := TStringList.Create;
try
  PackageSource.LoadFromFile( FN);
  Tokens := ParseDelphiFile( PackageSource);
  Idx := Length( Manifests);
  SetLength( Manifests, Idx + 1);
  with Manifests[ Idx] do
    begin
    FIdx := Idx;
    FDirectory    := RemoveBackslashUnlessRoot( Directory)
    FPackageName  := PackageName;
    FFullFileName := FN;
    FCompilerStr  := Uppercase( Compiler);               
    FDescription  := ExtractDescription( Tokens);
    FdoesOverrideDefault_BPL_DLL_Suffix := ExtractLibSuffix( Tokens, F_BPL_DLL_Suffix);
    FCompiledOk   := False;
    if Ext = '.dpk' then
        FPackageKind := pkBPL
      else if ExtractIsLibrary( Tokens) then
        FPackageKind := pkDLL
      else
        FPackageKInd := pkEXE;
    if FPackageKind = pkBPL then
        begin
        FisRunTime    := ExtractIsRunTime   ( Tokens);
        FisDesignTime := ExtractIsDesignTime( Tokens)
        end
      else
        begin
        FisRunTime    := True;
        FisDesignTime := False
        end
    end;
finally
  PackageSource.Free
  end;
end;


procedure SBD_CurUninstallStepChanged( CurUninstallStep: TUninstallStep);
begin
case CurUninstallStep of
  usAppMutexCheck: begin end;

  usUninstall:     begin                   
                   LoadSpawnedFiles;
                   PurgeSpawns;
                   ClearSpawnedFiles;
                   DeregisterDesignTimePackages
                   end;

  usPostUninstall: begin end;  
  usDone:          begin end;
  end
end;

function SBD_InitializeUninstall: Boolean;
begin
result := True;
InitiateCompilers;
SpawnedFiles := TStringList.Create;
BPL_DLL_Dir := GetPreviousData( 'BPL_dir', '');  
end;






procedure InitializeWizard;
begin      
SBD_InitializeWizard;
end;



function ShouldSkipPage( PageID: Integer): Boolean;
// The wizard calls this event function to determine whether or not a
// particular page (specified by PageID) should be shown at all. If you
// return True, the page will be skipped; if you return False, the page
// may be shown.
// Note: This event function isn't called for the wpWelcome, wpPreparing,
// and wpInstalling pages, nor for pages that Setup has already determined
// should be skipped (for example, wpSelectComponents in an install
// containing no components).
begin
result := SBD_ShouldSkipPage( PageId)
end;

procedure CurPageChanged( CurPageID: Integer);
// Called after a new wizard page (specified by CurPageID) is shown.
begin
SBD_CurPageChanged( CurPageID)
end;


function UpdateReadyMemo(
  Space, NewLine, MemoUserInfoInfo, MemoDirInfo,
  MemoTypeInfo, MemoComponentsInfo, MemoGroupInfo, MemoTasksInfo:
  String): String;
// If Setup finds the UpdateReadyMemo event function in the Pascal script,
// it is called automatically when the Ready to Install wizard page becomes
// the active page. It should return the text to be displayed in the settings
// memo on the Ready to Install wizard page as a single string with lines
// separated by the NewLine parameter. Parameter Space contains a string with
// spaces. Setup uses this string to indent settings. The other parameters
// contain the (possibly empty) strings that Setup would have used as the
// setting sections. The MemoDirInfo parameter for example contains the string
// for the Selected Directory section.
begin
result := SBD_UpdateReadyMemo(
  Space, NewLine, MemoUserInfoInfo, MemoDirInfo,
  MemoTypeInfo, MemoComponentsInfo, MemoGroupInfo, MemoTasksInfo)
end;


procedure RegisterPreviousData( PreviousDataKey: Integer);
// To store user settings entered on custom wizard pages, place a
// RegisterPreviousData event function in the Pascal script and call
//SetPreviousData(PreviousDataKey, ...) inside it, once per setting.
begin
SBD_RegisterPreviousData( PreviousDataKey)
end;


function InitializeUninstall(): Boolean;
// Return False to abort Uninstall, True otherwise.
begin
result := SBD_InitializeUninstall
end;

procedure CurUninstallStepChanged( CurUninstallStep: TUninstallStep);
begin
SBD_CurUninstallStepChanged( CurUninstallStep);

end;