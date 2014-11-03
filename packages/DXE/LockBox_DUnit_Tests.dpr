program LockBox_DUnit_Tests;

uses
  Forms,
  TestFramework in '..\..\..\..\..\dunit\src\TestFramework.pas',
  GUITestRunner in '..\..\..\..\..\dunit\src\GUITestRunner.pas',
  uLockBox_TestCases in '..\..\test\uLockBox_TestCases.pas',
  uLockBox_CipherTestCases in '..\..\test\uLockBox_CipherTestCases.pas',
  uLockBox_HugeCardinalTestCases in '..\..\test\uLockBox_HugeCardinalTestCases.pas',
  uLockBox_RSA_TestCases in '..\..\test\uLockBox_RSA_TestCases.pas',
  uLockBox_Signatory_TestCases in '..\..\test\uLockBox_Signatory_TestCases.pas',
  uLockBox_Hashes in '..\..\test\uLockBox_Hashes.pas',
  DUnitConsts in '..\..\..\..\..\dunit\src\DUnitConsts.pas',
  uTPLb_SVN_Keywords in '..\..\run\utilities\uTPLb_SVN_Keywords.pas';

{$R *.res}

begin
Application.Initialize;
GUITestRunner.RunRegisteredTests
end.
