program Precompute;

uses
  Forms,
  umfmPrecompute in '..\..\design\umfmPrecompute.pas' {mfmPrecompute};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Precompute';
  Application.CreateForm(TmfmPrecompute, mfmPrecompute);
  Application.Run;
end.
