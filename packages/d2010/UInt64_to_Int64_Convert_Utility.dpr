program UInt64_to_Int64_Convert_Utility;

uses
  Forms,
  umfmUInt64_to_Int64_Converter in '..\..\design\forms\d2010\umfmUInt64_to_Int64_Converter.pas' {mfmUInt64_to_Int64_Converter};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TmfmUInt64_to_Int64_Converter, mfmUInt64_to_Int64_Converter);
  Application.Run;
end.
