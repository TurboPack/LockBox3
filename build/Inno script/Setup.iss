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
OutputBaseFilename=setup
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

#include "SBD_DelphiComponentLibraryInstallerFramwork_TaskIncludes_inc.iss"

[Files]
#include "SBD_DelphiComponentLibraryInstallerFramwork_FileIncludes_inc.iss"

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

#include "SBD_DelphiComponentLibraryInstallerFramwork_Part1_inc.iss"

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

#include "SBD_DelphiComponentLibraryInstallerFramwork_Part2_inc.iss"


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



