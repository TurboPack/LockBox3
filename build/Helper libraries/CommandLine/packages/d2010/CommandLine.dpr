library CommandLine;

{ Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters. }

uses
  SysUtils,
  Classes,
  uCommandLine in '..\..\run\uCommandLine.pas',
  uInnoStringSupport in '..\..\run\uInnoStringSupport.pas',
  uDiagnostics in '..\..\run\uDiagnostics.pas';

{$LIBSUFFIX '_d2010'}

exports uCommandLine.Export_PrepareCommand  name 'SBDIH_PrepareCommand';
exports uCommandLine.Export_QuantumExecute  name 'SBDIH_QuantumExecute';
exports uCommandLine.Export_AbortActivity   name 'SBDIH_AbortActivity';

exports uDiagnostics.Diag   name 'Diag';

exports uInnoStringSupport.Convert_Ansi_To_UTF8              name 'SBDIH_ansi_to_utf8';
exports uInnoStringSupport.Convert_UTF8_To_Ansi              name 'SBDIH_utf8_to_ansi';
exports uInnoStringSupport.Convert_UTF16_To_Ansi             name 'SBDIH_utf16_to_ansi';
exports uInnoStringSupport.Convert_Ansi_To_UTF16_Length      name 'SBDIH_ansi_to_utf16_len';
exports uInnoStringSupport.Convert_Ansi_To_UTF16_IndexedChar name 'SBDIH_ansi_to_utf16_char';
exports uInnoStringSupport.Convert_UTF16_To_UTF8             name 'SBDIH_utf16_to_utf8';
exports uInnoStringSupport.Convert_UTF8_To_UTF16_Length      name 'SBDIH_utf8_to_utf16_len';
exports uInnoStringSupport.Convert_UTF8_To_UTF16_IndexedChar name 'SBDIH_utf8_to_utf16_char';
exports uInnoStringSupport.Convert_Oem_To_Ansi               name 'SBDIH_oem_to_ansi';
exports uInnoStringSupport.ReadAndXOR_GlobalOptions          name 'SBDIH_globaloptions';


begin
end.
