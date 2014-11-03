program CSharpDemo;

uses
  Forms,
  uCSharpDemo in '..\..\demo\uCSharpDemo.pas' {mfmCSharpDemo},
  uCSharpZeroPadding in '..\..\demo\uCSharpZeroPadding.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TmfmCSharpDemo, mfmCSharpDemo);
  Application.Run;
end.
