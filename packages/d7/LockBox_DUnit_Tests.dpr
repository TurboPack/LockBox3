program LockBox_DUnit_Tests;

uses
  Forms,
  TestFramework in '..\..\..\..\..\DUnit\d2007-\src\TestFramework.pas',
  GUITestRunner in '..\..\..\..\..\DUnit\d2007-\src\GUITestRunner.pas',
  DUnitConsts in '..\..\..\..\..\DUnit\d2007-\src\DUnitConsts.pas',
  uLockBox_TestCases in '..\..\test\uLockBox_TestCases.pas',
  uLockBox_CipherTestCases in '..\..\test\uLockBox_CipherTestCases.pas',
  uLockBox_HugeCardinalTestCases in '..\..\test\uLockBox_HugeCardinalTestCases.pas',
  uLockBox_RSA_TestCases in '..\..\test\uLockBox_RSA_TestCases.pas',
  uLockBox_Signatory_TestCases in '..\..\test\uLockBox_Signatory_TestCases.pas',
  uLockBox_Hashes in '..\..\test\uLockBox_Hashes.pas';

{$R *.res}

begin
Application.Initialize;
GUITestRunner.RunRegisteredTests
end.
