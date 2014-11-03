unit uDiagnostics;
interface
uses uInnoStringSupport;

function  Diag: PAnsiChar; stdcall;
// Diag returns a UTF-8 encoding of the diagnostic strings and then
//  clears the diagnostic strings.

procedure SetDiagLogOn( Value: boolean);
// True = logging; False = Ignoring.


// The following methods are for internal use only.
// All string parameters are UTF-16 unicodestring.
procedure DiagLog( const Line: string);
procedure DiagLogFmt( const Line: string; const Args: array of const);
procedure DiagBoolean( const Name: string; Value: boolean);
procedure DiagString( const Name, Value: string);
procedure DiagInteger( const Name: string; Value: integer);


implementation



uses SysUtils;

var
  sDiag: UTF8String;          // The diagnostic strings state.
  DiagHold: UTF8String;       // Not state. Used as a reference holder.
  isDiagOn: boolean = False;  // The logging ON/OFF switch.


procedure DiagLog( const Line: string);
begin
if not isDiagOn then exit;
if sDiag <> '' then
  sDiag := sDiag + #13#10;
sDiag := sDiag + UTF8Encode( Line)
end;

procedure DiagLogFmt( const Line: string; const Args: array of const);
begin
if isDiagOn then
  DiagLog( Format( Line, Args))
end;

procedure DiagBoolean( const Name: string; Value: boolean);
const BoolStrs: array[boolean] of string =('False','True');
begin
DiagLogFmt( '%s = %s', [Name, BoolStrs[ Value]])
end;

procedure DiagString( const Name, Value: string);
begin
DiagLogFmt( '%s = <%s>', [Name, Value])
end;

procedure DiagInteger( const Name: string; Value: integer);
begin
DiagLogFmt( '%s = %d', [Name, Value])
end;

function Diag: PAnsiChar; stdcall;
begin
result   := nil;
if not isDiagOn then exit;
if (sDiag <> '') and doWorkAround_Inno5_4_0u_ReturnAnsiString_Bug then
  sDiag := sDiag + 'X'; // Add a dummy character.
DiagHold := sDiag;
sDiag    := '';
result   := PAnsiChar( DiagHold)
end;


procedure SetDiagLogOn( Value: boolean);
begin
if (not Value) and isDiagOn and (sDiag <> '') then
  sDiag := '';
isDiagOn := Value
end;


end.
