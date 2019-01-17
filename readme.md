# TurboPack LockBox3

Updated for **10.3 Rio** / VER330 / PKG 260

You can still access [10.2 Tokyo](https://github.com/TurboPack/LockBox3/releases/tag/102Tokyo) and [10.1 Berlin](https://github.com/TurboPack/LockBox3/releases/tag/101Berlin) versions too.

## Table of contents

1.  Introduction
2.  Package names
3.  Installation

## 1. Introduction

LockBox3 is a Delphi library for cryptography.  
It provides support for AES, DES, 3DES, Blowfish, Twofish, SHA, MD5, a variety 
of chaining modes, RSA digital signature and verific...

This is a source-only release of TurboPack LockBox3. It includes
designtime and runtime packages for Delphi and C++Builder and supports 
VCL, FMX, Win32, Win64, macOS, iOS, and Android.

## 2. Package names

TurboPack LockBox3 package names have the following form:

### Delphi
* LockBox3DR.bpl (Delphi Runtime)
* LockBox3DD.bpl (Delphi Designtime)

### C++Builder
* LockBox3CR.bpl (C++Builder Runtime)
* LockBox3CD.bpl (C++Builder Designtime)

## 3. Installation

TurboPack LockBox3 is available via the [GetIt Package Manager](http://docwiki.embarcadero.com/RADStudio/en/Installing_a_Package_Using_GetIt_Package_Manager) where you can quickly and easily install and uninstall it.

To manually install TurboPack LockBox3 into your IDE, take the following
steps:

1. Unzip the release files into a directory (e.g., `d:\lockBox3`).

2. Start RAD Studio.

3. Add the source directory (e.g. `d:\lockBox3\run` and all the 
     subdirectories) to the IDE's library path. For C++Builder, 
     add the hpp subdirectory (e.g., `d:\lockBox3\source\hpp\Win32\Release`) to the 
     IDE's system include path.

4. Open & install the designtime package specific to the IDE being
     used. The IDE should notify you the components have been
     installed.
