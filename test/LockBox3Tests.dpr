program LockBox3Tests;
{

  Delphi DUnit-Testprojekt
  -------------------------
  Dieses Projekt enthält das DUnit-Test-Framework und die GUI/Konsolen-Test-Runner.
  Fügen Sie den Bedingungen in den Projektoptionen "CONSOLE_TESTRUNNER" hinzu,
  um den Konsolen-Test-Runner zu verwenden.  Ansonsten wird standardmäßig der
  GUI-Test-Runner verwendet.

}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  DUnitTestRunner,
  uLockBox_CipherTestCases in 'uLockBox_CipherTestCases.pas',
  uLockBox_Hashes in 'uLockBox_Hashes.pas',
  uLockBox_HugeCardinalTestCases in 'uLockBox_HugeCardinalTestCases.pas',
  uLockBox_RSA_TestCases in 'uLockBox_RSA_TestCases.pas',
  uLockBox_Signatory_TestCases in 'uLockBox_Signatory_TestCases.pas',
  uLockBox_TestCases in 'uLockBox_TestCases.pas';

{$R *.RES}

begin
  DUnitTestRunner.RunRegisteredTests;
end.

