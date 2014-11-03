{* ***** BEGIN LICENSE BLOCK *****
Copyright 2010 Sean B. Durkin
This file is part of TurboPower LockBox 3. TurboPower LockBox 3 is free
software being offered under a dual licensing scheme: LGPL3 or MPL1.1.

The contents of this file are subject to the Mozilla Public License (MPL)
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Alternatively, you may redistribute it and/or modify it under the terms of
the GNU Lesser General Public License (LGPL) as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

You should have received a copy of the Lesser GNU General Public License
along with TurboPower LockBox 3.  If not, see <http://www.gnu.org/licenses/>.

TurboPower LockBox is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. In relation to LGPL,
see the GNU Lesser General Public License for more details. In relation to MPL,
see the MPL License for the specific language governing rights and limitations
under the License.

The Initial Developer of the Original Code for TurboPower LockBox version 2
and earlier was TurboPower Software.

 * ***** END LICENSE BLOCK ***** *}

unit uTPLb_Signatory;
interface
uses Classes, uTPLb_BaseNonVisualComponent, uTPLb_Codec, uTPLb_Asymetric;

type
TGenerateKeysPhase = (gkNeutral, gkGenSigningKeys, gkGenCryptoKeys, gkDone);
TVerifyResult = (vPass, vFail, vUserAbort);


{$IF CompilerVersion >= 23.0}
[ComponentPlatformsAttribute( pidWin32 or pidWin64)]
{$ENDIF}
TSignatory = class( TTPLb_BaseNonVisualComponent)
  private
    FCodec: TCodec;

    procedure SetCodec( Value: TCodec);

  protected
    procedure Notification(
      AComponent: TComponent; Operation: TOperation); override;

  public
    FGenPhase: TGenerateKeysPhase;
    FCryptoKeys : TAsymetricKeyPair;
    FSigningKeys: TAsymetricKeyPair;

    constructor Create( AOwner: TComponent); override;
    destructor Destroy; override;
    function  Sign( Document, Signature: TStream): boolean;
    function  Verify( Document, Signature: TStream): TVerifyResult;

    function  GenerateKeys: boolean;
    procedure StoreKeysToStream( Store: TStream; Parts: TKeyStoragePartSet);
    procedure LoadKeysFromStream( Store: TStream; Parts: TKeyStoragePartSet);
    function  Can_SaveKeys( Parts: TKeyStoragePartSet): boolean;
    function  HasParts: TKeyStoragePartSet;

    procedure Burn;

  published
    property Codec: TCodec     read FCodec write SetCodec;
  end;

implementation










uses SysUtils;
{ TSignatory }

procedure TSignatory.Notification(
  AComponent: TComponent; Operation: TOperation);
begin
inherited;
if (Operation = opRemove) and (AComponent = FCodec) then
  SetCodec( nil)
end;



procedure TSignatory.SetCodec( Value: TCodec);
begin
if FCodec = Value then exit;
if assigned( FCodec) then
  FCodec.RemoveFreeNotification( self);
FCodec := Value;
if assigned( FCodec) then
  FCodec.FreeNotification( self)
end;






constructor TSignatory.Create( AOwner: TComponent);
begin
FGenPhase := gkNeutral;
inherited
end;


destructor TSignatory.Destroy;
begin
Burn;
SetCodec( nil);
inherited
end;


procedure TSignatory.Burn;
begin
if assigned( FSigningKeys) then
  FSigningKeys.Burn;
if assigned( FCryptoKeys) then
  FCryptoKeys.Burn;
FreeAndNil( FSigningKeys);  FreeAndNil( FCryptoKeys)
end;


function TSignatory.GenerateKeys: boolean;
begin
FGenPhase := gkGenSigningKeys;
FreeAndNil( FSigningKeys);  FreeAndNil( FCryptoKeys);
FCodec.InitFromGeneratedAsymetricKeyPair;
result := not FCodec.isUserAborted;
if not result then exit;
FSigningKeys := (FCodec.Key as TAsymetricKeyPair).Clone;
FGenPhase := gkGenCryptoKeys;
FCodec.InitFromGeneratedAsymetricKeyPair;
result := not FCodec.isUserAborted;
if not result then exit;
FCryptoKeys  := (FCodec.Key as TAsymetricKeyPair).Clone;
FGenPhase := gkDone
end;


function TSignatory.HasParts: TKeyStoragePartSet;
begin
if assigned( FCryptoKeys) and assigned( FSigningKeys) then
    result := FCryptoKeys.HasParts * FSigningKeys.HasParts
  else
    result := []
end;

function TSignatory.Sign( Document, Signature: TStream): boolean;
var
  Engine: IASymetric_Engine;
  wasAborted: boolean;
begin
wasAborted := False;
Engine := FCodec.Asymetric_Engine;
FCodec.EstimatedWorkLoad := Document.Size;
Engine.Sign( Document, Signature,
  FSigningKeys.FPrivatePart, self,
  FCodec.OnProgress, wasAborted);
FCodec.isUserAborted := wasAborted;
result := not wasAborted
end;


function TSignatory.Verify( Document, Signature: TStream): TVerifyResult;
var
  Engine: IASymetric_Engine;
  wasAborted, Ok: boolean;
begin
wasAborted := False;
Engine := FCodec.Asymetric_Engine;
FCodec.EstimatedWorkLoad := Document.Size;
Ok := Engine.VerifySignature( Document, Signature,
  FSigningKeys.FPublicPart, self,
  FCodec.OnProgress, wasAborted);
FCodec.isUserAborted := wasAborted;
if wasAborted then
    result := vUserAbort
  else if Ok then
    result := vPass
  else
    result := vFail
end;



procedure TSignatory.StoreKeysToStream(
  Store: TStream; Parts: TKeyStoragePartSet);
begin
if assigned( FCryptoKeys) then
  FCryptoKeys.StoreToStream( Store, Parts);
if assigned( FSigningKeys) then
FSigningKeys.StoreToStream( Store, Parts);
end;



procedure TSignatory.LoadKeysFromStream(
  Store: TStream; Parts: TKeyStoragePartSet);
var
  Engine: IASymetric_Engine;
begin
Engine := FCodec.Asymetric_Engine;
if assigned( FCryptoKeys) then
    FCryptoKeys.LoadFromStream( Store, Parts)

  else if assigned( Engine) then
    FCryptoKeys := Engine.CreateFromStream( Store, Parts);

if assigned( FSigningKeys) then
    FSigningKeys.LoadFromStream( Store, Parts)

  else if assigned( Engine) then
    FSigningKeys := Engine.CreateFromStream( Store, Parts);

if assigned( FCryptoKeys) and assigned( FCodec) then
  FCodec.InitFromKey( FCryptoKeys.Clone)
end;



function TSignatory.Can_SaveKeys( Parts: TKeyStoragePartSet): boolean;
begin
result := assigned( FSigningKeys) and  assigned( FCryptoKeys) and
          FCryptoKeys .Can_StoreToStream( Parts) and
          FSigningKeys.Can_StoreToStream( Parts)
end;



end.
