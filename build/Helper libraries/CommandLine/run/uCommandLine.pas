unit uCommandLine;
interface

procedure Export_PrepareCommand(
  CmdLine, Dir: PWideChar;    // Input UTF-16
  doesUnderstandCntrlC, OutputIsOEM_CP: boolean;
  PollRate_ms: integer;
  var ActivityToken: integer);   stdcall;

procedure Export_QuantumExecute(
  var ActivityToken: integer;
  var hasOutput: boolean;
  var Line: PAnsiChar;    // Output Ansistring
  var isDone: boolean;
  var RetCode: integer); stdcall;

procedure Export_AbortActivity(
  var ActivityToken: integer); stdcall;


implementation









uses Windows, Classes, SysUtils, uDiagnostics, uInnoStringSupport;



const
  Control_C_WaitTime = 1000; // ms

type
TAnonymousPipe = class( TInterfacedPersistent)
  private
    FReadHandle : THandle;
    FWriteHandle: THandle;

    function GetIsOk: boolean;

  public
    constructor Create;
    destructor Destroy; override;

    function Read: ansistring;

    property ReadHandle : THandle  read FReadHandle;
    property WriteHandle: THandle  read FWriteHandle;
    property Ok: boolean   read GetIsOk;
  end;

TSpawnPhase = (phLaunch, phPoll, phClosing);

TSpawn = class( TInterfacedPersistent)
  private
    FCmdLine, FDir: string;
    FdoesUnderstandCntrlC: boolean;
    FPollRate_ms: integer;
    FProcessInfo: TProcessInformation;
    FNormalIO: TAnonymousPipe;
    FAbortIO: TAnonymousPipe;
    FSpawnPhase: TSpawnPhase;
    FCharacterBuffer: ansistring;
    FNeed_LF: boolean;
    FHoldString: ansistring;
    StartupInfo: TStartupInfo;
    FOutputIsOEM_CP: boolean;

    function EndProcess_ExitCode: integer;
    function CheckForLF: boolean;
    procedure QuantumExecute_Launch(
      var isDone: boolean; var RetCode: integer);
    procedure QuantumExecute_Poll(
      var hasOutput: boolean; var Line: ansistring);
    procedure QuantumExecute_Closing(
      var hasOutput: boolean; var Line: ansistring;
      var isDone: boolean; var RetCode: integer);
    function WaitFor( TimeOut: integer): boolean;
    function ExtractLine( CR_Pos: integer): ansistring;
    function ProcessHandle: THandle;
    procedure ClearProcessHandle;
    function Find_CR: integer;

  public
    constructor Create(
      const CmdLine1, Dir1: string;
      doesUnderstandCntrlC1, OutputIsOEM_CP1: boolean;
      PollRate_ms1: integer);

    destructor Destroy; override;

    procedure QuantumExecute(
      var hasOutput: boolean;
      var Line: ansistring;
      var isDone: boolean;
      var RetCode: integer);

    procedure AbortActivity;
  end;



procedure Export_PrepareCommand(
  CmdLine, Dir: PWideChar;
  doesUnderstandCntrlC, OutputIsOEM_CP: boolean;
  PollRate_ms: integer;
  var ActivityToken: integer);   stdcall;
var
  CmdLine1, Dir1: string;
begin
DiagLog( 'Enter Export_PrepareCommand');
DiagString( 'CmdLine', CmdLine);
DiagString( 'Dir', Dir);
DiagBoolean( 'Understands ^C', doesUnderstandCntrlC);
DiagBoolean( 'is OEM CodePage', OutputIsOEM_CP);
CmdLine1 := CmdLine;
Dir1     := Dir;
ActivityToken := integer( TSpawn
  .Create( CmdLine1, Dir1, doesUnderstandCntrlC, OutputIsOEM_CP, PollRate_ms))
;DiagLog( 'Exit Export_PrepareCommand');
end;




procedure Export_QuantumExecute(
  var ActivityToken: integer;
  var hasOutput: boolean;
  var Line: PAnsiChar;
  var isDone: boolean;
  var RetCode: integer); stdcall;
var
  Line1: ansistring;
begin
DiagLog( 'Enter Export_QuantumExecute');
if ActivityToken <> 0 then
    begin
    TSpawn( ActivityToken).QuantumExecute( hasOutput, Line1, isDone, retCode);
    if isDone then
      ActivityToken := 0;
    Line1 := Line1;
    Line  := PAnsiChar( Line1)
    end
  else
    begin
    DiagLog( 'Token was empty.');
    hasOutput := False;
    Line   := nil;
    isDone := True
    end;
;DiagLog( 'Exit Export_QuantumExecute');
end;





procedure Export_AbortActivity(
  var ActivityToken: integer); stdcall;
begin
DiagLog( 'Enter Export_AbortActivity');
if ActivityToken = 0 then exit;
TSpawn( ActivityToken).AbortActivity;
ActivityToken := 0
;DiagLog( 'Exit Export_AbortActivity');
end;




{ TAnonymousPipe }
var
  SecAttrib: TSecurityAttributes;
  SecAttrib_Initiated: boolean = False;

function PSecAttrib: PSecurityAttributes;
begin
if not SecAttrib_Initiated then
  begin
  SecAttrib_Initiated := True;
  FillChar( SecAttrib, SizeOf( SecAttrib), 0);
  SecAttrib.nLength := SizeOf( SecAttrib);
  SecAttrib.bInheritHandle := True
  end;
result := @SecAttrib
end;


constructor TAnonymousPipe.Create;
begin
FReadHandle  := 0;
FWriteHandle := 0;
if CreatePipe( FReadHandle, FWriteHandle, PSecAttrib, 0) then exit;
FReadHandle  := 0;
FWriteHandle := 0
end;

destructor TAnonymousPipe.Destroy;
begin
if FReadHandle <> 0 then
  begin
  CloseHandle( FReadHandle);
  FReadHandle := 0
  end;
if FWriteHandle <> 0 then
  begin
  CloseHandle( FWriteHandle);
  FWriteHandle := 0
  end;
inherited
end;

function TAnonymousPipe.GetIsOk: boolean;
begin
result := (FReadHandle <> 0) and (FWriteHandle <> 0)
end;



function TAnonymousPipe.Read: ansistring;
var
  BytesInPipe, n: Cardinal;
begin
BytesInPipe := 0;
if not PeekNamedPipe( FReadHandle, nil, 0, nil, @BytesInPipe, nil) then
  BytesInPipe := 0;
SetLength( result, BytesInPipe);
if result = '' then exit;
ReadFile( FReadHandle, result[1], BytesInPipe, n, nil);
SetLength( result, n)
end;





{ TSpawn }

constructor TSpawn.Create( const CmdLine1, Dir1: string;
  doesUnderstandCntrlC1, OutputIsOEM_CP1: boolean; PollRate_ms1: integer);
begin
FCmdLine := CmdLine1;
FDir     := Dir1;
FdoesUnderstandCntrlC := doesUnderstandCntrlC1;
FOutputIsOEM_CP := OutputIsOEM_CP1;
FPollRate_ms := PollRate_ms1;
FillChar( FProcessInfo, SizeOf( FProcessInfo), 0);
FNormalIO := nil;
FAbortIO  := nil;
FSpawnPhase := phLaunch;
FCharacterBuffer := '';
FNeed_LF := False;
FHoldString := ''
end;


destructor TSpawn.Destroy;
begin
FHoldString := '';
EndProcess_ExitCode;
FNormalIO.Free;
FAbortIO.Free;
inherited
end;


function TSpawn.EndProcess_ExitCode: integer;
begin
if ProcessHandle <> 0 then
    begin
    GetExitCodeProcess( ProcessHandle, Cardinal( result));
    CloseHandle( ProcessHandle);
    ClearProcessHandle
    end
  else
    result := 0
end;

function TSpawn.CheckForLF: boolean;
begin
result := False;
if (not FNeed_LF) or (FCharacterBuffer = '') then exit;
FNeed_LF := False;
result := FCharacterBuffer[1] = #10;
if result then
  Delete( FCharacterBuffer, 1, 1)
end;


procedure TSpawn.QuantumExecute_Launch(
  var isDone: boolean; var RetCode: integer);
begin
// isDone  = False
// RetCode = 0
// FSpawnPhase = phLaunch
FNormalIO := TAnonymousPipe.Create;
FAbortIO  := TAnonymousPipe.Create;
with StartupInfo do
      begin
      wShowWindow := SW_HIDE;
      hStdInput   := FAbortIO.ReadHandle;
      hStdOutput  := FNormalIO.WriteHandle;
      hStdError   := FNormalIO.WriteHandle;
      dwFlags     := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW
      end;
if FNormalIO.Ok and FAbortIO.Ok and CreateProcess(
         nil, PChar( FCmdLine), PSecAttrib, nil, True,
         CREATE_SUSPENDED, nil, PChar( FDir), StartupInfo, FProcessInfo) then
        begin
        DiagLog( 'Process created ok.');
        ResumeThread( FProcessInfo.hThread);
        CloseHandle ( FProcessInfo.hThread);
        FSpawnPhase := phPoll
        end
      else
        begin
        isDone := True;
        if FNormalIO.Ok and FAbortIO.Ok then
            RetCode := GetLastError
          else
            RetCode := -2;
        DiagInteger( 'Process creation failed', RetCode);
        if RetCode = 267 then
          DiagString( 'Invalid directory', FDir);
        FSpawnPhase := phClosing;
        RetCode     := EndProcess_ExitCode
        end
end;



function TSpawn.ExtractLine( CR_Pos: integer): ansistring;
var
  L: integer;
begin
// CR_Pos > 0
if CheckForLF then
  Dec( CR_Pos);
result := Copy( FCharacterBuffer, 1, CR_Pos-1);
Delete( FCharacterBuffer, 1, CR_Pos);
FNeed_LF := True;
CheckForLF;
L := Length( result);
if (L <> 0) and FOutputIsOEM_CP then
  begin
  Windows.OemToCharBuffA( PAnsichar( result), PAnsiChar( result), L);
  //PStrRec( Integer( Result) - SizeOf(StrRec)).codePage := CP_UTF8;
  // tbd set code page.
  end
end;



function TSpawn.ProcessHandle: THandle;
begin
result := FProcessInfo.hProcess
end;


procedure TSpawn.ClearProcessHandle;
begin
FProcessInfo.hProcess := 0
end;


function TSpawn.Find_CR: integer;
begin
CheckForLF;
result := Pos( AnsiChar(#13), FCharacterBuffer)
end;

var
  PollLogs: integer = 0;

procedure TSpawn.QuantumExecute_Poll(
  var hasOutput: boolean; var Line: ansistring);
var
  didTimeOut: boolean;
  CR_Pos: integer;
  TerminationWasSignalled: boolean;

begin
// hasOutput = False
// Line  = '';
// FSpawnPhase = phPoll
TerminationWasSignalled := False;
CR_Pos := Find_CR;
if CR_Pos = 0 then
    begin
    didTimeOut := WaitFor( FPollRate_ms);
    if not didTimeOut then
      TerminationWasSignalled := True;
    FCharacterBuffer := FCharacterBuffer + FNormalIO.Read;
    CR_Pos := Find_CR
    end;
//  else
//    didTimeOut := False;
hasOutput := CR_Pos > 0;
if hasOutput then
  begin
  Line := ExtractLine( CR_Pos);
  {$WARNINGS OFF}
  DiagString( 'Extracted line', Line)
  {$WARNINGS ON}
  end;
if TerminationWasSignalled then
  FSpawnPhase := phClosing
end;



function TSpawn.WaitFor( TimeOut: integer): boolean;
begin
result := WaitForSingleObject( ProcessHandle, TimeOut) = WAIT_TIMEOUT
end;


procedure TSpawn.QuantumExecute_Closing(
  var hasOutput: boolean; var Line: ansistring;
  var isDone: boolean; var RetCode: integer);
var
  CR_Pos: integer;
begin
// hasOutput = False
// Line    = ''
// isDone  = False
// RetCode = 0
// FSpawnPhase = phClosing
CR_Pos := Find_CR;
if CR_Pos > 0 then
    begin
    hasOutput := True;
    Line := ExtractLine( CR_Pos)
    end
  else if FCharacterBuffer <> '' then
    begin
    hasOutput := True;
    Line := FCharacterBuffer;
    FCharacterBuffer := ''
    end
  else
    begin
    isDone  := True;
    RetCode := EndProcess_ExitCode
    end
end;


procedure TSpawn.QuantumExecute(
  var hasOutput: boolean; var Line: ansistring;
  var isDone: boolean; var RetCode: integer);
begin
FHoldString := '';
hasOutput := False;
Line    := '';
isDone  := False;
RetCode := 0;
case FSpawnPhase of
  phLaunch : QuantumExecute_Launch( isDone, RetCode);
  phPoll   : QuantumExecute_Poll( hasOutput, Line);
  phClosing: QuantumExecute_Closing( hasOutput, Line, isDone, RetCode);
  end;
if hasOutput then
  FHoldString := Line;
if isDone and (FHoldString = '') then
  Destroy
end;


procedure TSpawn.AbortActivity;
var
  doTerminate: boolean;
begin
doTerminate := ProcessHandle <> 0;
if doTerminate and FdoesUnderstandCntrlC then
  begin
  DiagLog( 'Sending control-C');
  GenerateConsoleCtrlEvent( CTRL_C_EVENT, FProcessInfo.dwProcessId);
  doTerminate := WaitFor( Control_C_WaitTime)
  end;
if doTerminate then
  TerminateProcess( ProcessHandle, 1);
ClearProcessHandle;
Destroy
end;

end.
