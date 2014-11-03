program RSA_Encryption_Demo;

uses
  Forms,
  umfmRSA_Encryption_Demo in '..\..\demo\D2010+\umfmRSA_Encryption_Demo.pas' {mfmRSA_Encryption_Demo};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TmfmRSA_Encryption_Demo, mfmRSA_Encryption_Demo);
  Application.Run;
end.
