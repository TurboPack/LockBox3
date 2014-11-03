unit umfmUInt64_to_Int64_Converter;
// This file is not part of lockbox. It is merely a design-support tool,
//  and is shown here for the curious.              - SBD.
interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ActnList, ImgList, ExtDlgs;

type
  TmfmUInt64_to_Int64_Converter = class(TForm)
    memoIntro: TMemo;
    edtUInt64Value: TLabeledEdit;
    memoOutput: TMemo;
    actlstMain: TActionList;
    btnCoerceOne: TButton;
    actCoerceOne: TAction;
    btnClearOutput: TButton;
    actClear: TAction;
    edtFileName: TButtonedEdit;
    imglstActionGylphs16x16: TImageList;
    dlgOpenUnit: TOpenTextFileDialog;
    actCoerceFile: TAction;
    btnCoerceFile: TButton;
    procedure FormCreate(Sender: TObject);
    procedure actCoerceOneExecute(Sender: TObject);
    procedure actClearUpdate(Sender: TObject);
    procedure actClearExecute(Sender: TObject);
    procedure actCoerceFileUpdate(Sender: TObject);
    procedure actCoerceFileExecute(Sender: TObject);

  private
    procedure edtFileName_OnRightButtonClick( Sender: TObject);

  public
    procedure ClearOutput;
    procedure Put( const Line: string);
    procedure PutFmt( const Fmt: string; const Args: array of const);
  end;

var
  mfmUInt64_to_Int64_Converter: TmfmUInt64_to_Int64_Converter;

implementation
















uses Character;
{$R *.dfm}

function Coerce_UInt64_to_Int64( V_UInt64: UInt64; var isNeg: boolean): Int64;
// Coerces a UInt64 value into its equivalent SInt64 by modulo arithmetic.
// The returned value is the absolute value of the coerced value.
// Eg.  $FFFFFFFFFFFFFFFF  ==> $0000000000000001  & isNeg = True;
// Eg.  $7FFFFFFFFFFFFFFF  ==> $7FFFFFFFFFFFFFFF  & isNeg = False;

const
  HalfRange = $8000000000000000;
var
  t: UInt64;
begin
isNeg  := V_UInt64 >= HalfRange;
if not isNeg then
    result := V_UInt64
  else
    begin
    t := V_UInt64 - HalfRange;
    result := Int64( t);
    result := - result;
    result := result + HalfRange;
    Assert( UInt64( - result) = V_UInt64, 'Failed self-test!')
    end
end;



function Coerce_UInt64Str_to_Int64Str(
  const V_UInt64Str: string;
  var isNeg: boolean;
  var V_Int64Str, ErrMsg: string): boolean;
var
  s: string;
  j: integer;
  V_UInt64: UInt64;
  Code: integer;
  V_SInt64: Int64;

  function Check( Ok: boolean; const ErrMsg1: string): boolean;
  begin
  result := Ok;
  if (not result) and (s <> '') then
    ErrMsg := ErrMsg1
  end;

begin
result := False;
isNeg  := False;
V_Int64Str := '';
ErrMsg := '';

if not Check( Trim( V_UInt64Str) <> '', 'Input empty.') then exit;

// V_UInt64Str should begin with $
Val( V_UInt64Str, V_UInt64, Code);
if not Check( Code = 0, 'Not a valid integer in UInt64 range.') then exit;

V_SInt64 := Coerce_UInt64_to_Int64( V_UInt64, isNeg);
if not isNeg then
    V_Int64Str := Format( '$%.16x', [V_UInt64])
  else
    begin
    V_Int64Str := Format( '-$%.16x', [V_SInt64]);
    if not Check( UInt64( - V_SInt64) = V_UInt64, 'Failed self-test!') then exit
    end;
result := True
end;





procedure TmfmUInt64_to_Int64_Converter.actClearExecute( Sender: TObject);
begin
ClearOutput
end;

procedure TmfmUInt64_to_Int64_Converter.actClearUpdate( Sender: TObject);
begin
(Sender as TAction).Enabled := memoOutput.Lines.Count > 0
end;

function isHexadecimal( ch: Char): boolean;
begin
result := CharInSet( ch, ['0'..'9','A'..'F'])
end;



procedure TmfmUInt64_to_Int64_Converter.actCoerceFileExecute( Sender: TObject);
var
  Lines: TStrings;
  j, i, k, Len: Integer;
  ch: Char;
  Line: string;
  isValid: boolean;
  Processed: string;
  Match: string;
  isNeg: boolean;
  V_Int64Str, ErrMsg: string;
  Substitute: string;

begin
try
Lines := TStringList.Create;
Lines.LoadFromFile( edtFileName.Text);
try
for j := 0 to Lines.Count - 1 do
  begin
  Line := Lines[j];
  Processed := '';
  i := 1;
  Len := Length( Line);
  while i <= (Len - 16) do
    begin
    ch := Line[i];
    isValid := False;
    if (ch = '$') and ((i <= 1) or (Line[i-1] <> '-')) then
      begin
      isValid := True;
      for k := 1 to 16 do
        begin
        ch := Upcase( Line[ i + k ]);
        isValid := isHexadecimal( ch);
        if not isValid then break
        end
      end;
    if isValid then
      begin
      Processed := Processed + Copy( Line, 1, i - 1);
      Match := Copy( Line, i, 17);
      Delete( Line, 1, i + 16);
      Len := Length( Line);
      i := 0;
      if Coerce_UInt64Str_to_Int64Str( Match, isNeg, V_Int64Str, ErrMsg) then
          Substitute := V_Int64Str
        else
          Substitute := Match + '{<error Msg="' + ErrMsg + '">}';
      Processed := Processed + '{<UtoS_Cnvt>}uint64( ' + Substitute + '){</UtoS_Cnvt>}'
      end;
    Inc( i)
    end;
  Lines[j] := Processed + Line;
  Put( Lines[j])
  end;
finally
  Lines.Free
end;
except on e:exception do
  PutFmt( 'ERROR ! %s: %s', [e.ClassName, e.Message])
  end;
end;



procedure TmfmUInt64_to_Int64_Converter.actCoerceFileUpdate( Sender: TObject);
begin
(Sender as TAction).Enabled := edtFileName.Text <> ''
end;

procedure TmfmUInt64_to_Int64_Converter.actCoerceOneExecute(Sender: TObject);
var
  s: string;
  j: integer;
  isNeg: boolean;
  V_Int64Str, ErrMsg: string;

  function Check( Ok: boolean; const ErrMsg: string): boolean;
  begin
  result := Ok;
  if (not result) and (s <> '') then
    PutFmt( 'Error: %s', [ErrMsg])
  end;

begin
s := edtUInt64Value.Text;
PutFmt( 'Coercing %s from UInt64 to Int64', [s]);
for j := Length( s) downto 1 do
  if s[j] = ' ' then
    Delete( s, j, 1);

if Check( s <> '', 'Input empty.') and
   Check( Coerce_UInt64Str_to_Int64Str( '$'+s, isNeg, V_Int64Str, ErrMsg),
                                        ErrMsg) then
  PutFmt( '(signed) Int64 equivalent = %s', [V_Int64Str])
end;


procedure TmfmUInt64_to_Int64_Converter.ClearOutput;
begin
memoOutput.Clear
end;

procedure TmfmUInt64_to_Int64_Converter.edtFileName_OnRightButtonClick(
  Sender: TObject);
begin
dlgOpenUnit.FileName := (Sender as TButtonedEdit).Text;
if dlgOpenUnit.Execute then
  TButtonedEdit( Sender).Text := dlgOpenUnit.FileName
end;

procedure TmfmUInt64_to_Int64_Converter.FormCreate( Sender: TObject);
begin
ClearOutput;
edtFileName.OnRightButtonClick := edtFileName_OnRightButtonClick
end;

procedure TmfmUInt64_to_Int64_Converter.Put( const Line: string);
begin
memoOutput.Lines.Add( Line)
end;

procedure TmfmUInt64_to_Int64_Converter.PutFmt( const Fmt: string;
  const Args: array of const);
begin
Put( Format( Fmt, Args))
end;

end.
