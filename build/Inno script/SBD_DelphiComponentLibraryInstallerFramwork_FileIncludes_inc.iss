; This file is an include file for an Inno project. In goes in the files section.
; This include file is part of the SBD Delphi Component Library Installer Framework.
; It is required that HelperPath be defined in the main .iss file.

Source: "{#HelperPath}\CommandLine_d2010.dll"; DestDir: {tmp}; Flags: dontcopy ignoreversion; 
