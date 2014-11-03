; This file is an include file for an Inno project. In goes in the Tasks section.
; This include file is part of the SBD Delphi Component Library Installer Framework.

; Automated Component Library Installation:
Name: Compile; Description: "Compile packages"; GroupDescription: "Automated Component Library Installation:"; Flags: checkablealone
Name: Compile\Install; Description: "Install design-time packages"; Flags: dontinheritcheck
Name: Compile\LibPath; Description: "Add DCU directory to library search path"; Flags: dontinheritcheck
