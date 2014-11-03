program Lockbox3_Demo;

uses
  Forms,
  uDemoBlockModeCipher in '..\..\demo\uDemoBlockModeCipher.pas',
  umfmLockbox3_Demo in '..\..\demo\D2010+\umfmLockbox3_Demo.pas' {mfmLockbox3_Demo};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'LB3 Demo';
  Application.CreateForm(TmfmLockbox3_Demo, mfmLockbox3_Demo);
  Application.Run;
end.
