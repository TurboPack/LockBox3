{* ***** BEGIN LICENSE BLOCK *****
Copyright 2009, 2010 Sean B. Durkin
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

unit uTPLb_ComponentAbout;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, OleCtrls, SHDocVw, ComCtrls, jpeg;

const
  WelcomeVideo_URL = 'http://lockbox.seanbdurkin.id.au/online_resources/Intro3_Embedded.html';

type
  TTPLb_fmComponentAbout = class(TForm)
    pnlAbout: TPanel;
    btnClose: TButton;
    pgAboutContent: TPageControl;
    tsMain: TTabSheet;
    tsWelcome: TTabSheet;
    tsProject: TTabSheet;
    tsCopyLeft: TTabSheet;
    tsAuthors: TTabSheet;
    tsSupport: TTabSheet;
    tsAlgorithms: TTabSheet;
    Image1: TImage;
    lblTitle: TLabel;
    Label1: TLabel;
    lblRunTimeVersion: TLabel;
    lblDesignTimeVersion: TLabel;
    Memo1: TMemo;
    browserIntro: TWebBrowser;
    Label4: TLabel;
    Memo2: TMemo;
    lblWebsite: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Memo3: TMemo;
    Label9: TLabel;
    Memo4: TMemo;
    lbxAuthors: TListBox;
    imgAuthorPic: TImage;
    Bevel1: TBevel;
    lblAuthorEmail: TLabel;
    lblAuthorWeb: TLabel;
    Label12: TLabel;
    memoSupport: TMemo;
    memoHelp: TMemo;
    Panel1: TPanel;
    Label13: TLabel;
    lblHashDisplayName: TLabel;
    Label15: TLabel;
    lbxHashFeatures: TListBox;
    lblHashDefinitionURL: TLabel;
    lblHashWikipediaURL: TLabel;
    lblHashBlockSize: TLabel;
    lblHashDigestSize: TLabel;
    pnlCipherDescription: TPanel;
    Label20: TLabel;
    lblCipherDisplayName: TLabel;
    Label22: TLabel;
    lblCipherDefinitionURL: TLabel;
    lblCipherWikipediaURL: TLabel;
    lblCipherBlockSize: TLabel;
    lbxCipherFeatures: TListBox;
    Panel3: TPanel;
    Label27: TLabel;
    lblChainName: TLabel;
    Label29: TLabel;
    lblChainWikipedia: TLabel;
    lbxChainFeatures: TListBox;
    lblLogoAttribution: TLabel;
    lblTpsfaIntro: TStaticText;
    lblTpsfaContact: TStaticText;
    lblWebLabel: TStaticText;
    imgSean: TImage;
    memoExtraCipherDescription: TMemo;
    memoWelcomeInstructions: TMemo;
    procedure lblTpsfaContactClick(Sender: TObject);
    procedure OnURL_Click(Sender: TObject);
    procedure lbxAuthorsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }

  public
    procedure UpdateAbout( SelectedComponent: TComponent);
  end;

var
  TPLb_fmComponentAbout: TTPLb_fmComponentAbout;

implementation













uses uTPLb_InfoUtils, uTPLb_Hash, uTPLb_HashDsc, ShellAPI, uTPLb_StreamCipher,
  uTPLb_Signatory, uTPLb_Codec, TypInfo, uTPLb_BlockCipher, Registry,
  uTPLb_I18n, uTPLb_Decorators
{$IF CompilerVersion >= 21}
  , Rtti
{$IFEND}
  ;
{$R *.dfm}
{
Content Plan
============
Page 1: Main
0. Big Title: TurboPower LockBox 3
2. Product definition statement.
3. Logo or nice graphic for LockBox 3
7. run-time PE version number
8. design-time PE version
4. Brief statement about FOSS: LGPL

Page 2: Welcome message
1. Embedded video by me introducing the component suite.

Page 3: Brought to you by ...
Statement about project organisation and control (tpfsa)
5. tpfsa email
6. sourceforge url

Page 4: CopyLeft
12. Copyright  (mark and statement of application)
10. Statement of copying permission
9.  LGPL 3 logo as clickable link
11. LGPL and GPL full wording

Page 5: Authors
13. About authors. List -
  13.1 Name
  13.2 Photo
  13.3 Email
14 Statement welcoming contributions and adding to the list of authors.

Page 6: Support
15 Statement about official support.
16 Statement about wiki documentation.

Page 7: Algorithms
17 About Selected Hash -
  17.1 DisplayName
  17.2 Feature list
  17.3 Definition URL
  17.4 Wikipedia URL
  17.5 Block size
  17.6 Digest size
18 About Selected Chaining Mode -
  DisplayName
  Feature list
  Definition URL
  Wikipedia URL
19 About Selected Block Cipher -
  DisplayName
  Feature list
  Definition URL
  Wikipedia URL
  Block size

}
{ TTPLb_fmComponentAbout }

procedure TTPLb_fmComponentAbout.lbxAuthorsClick(Sender: TObject);
var
  Idx: integer;
  Author, sAuthorEmail, sAuthorWeb: string;
begin
Idx := lbxAuthors.ItemIndex;
if Idx <> -1 then
    Author := lbxAuthors.Items[ Idx]
  else
    Author := '';
if SameText( Author, 'Sean B. Durkin') then
    Idx := 0
  else if SameText( Author, 'David Barton') then
    Idx := 1
  else if SameText( Author, 'Another contributor') then
    Idx := -1
  else
    Idx := -1;
case Idx of
  -1: begin
      imgAuthorPic.Picture := nil;
      sAuthorEmail := '';
      sAuthorWeb   := ''
      end;
   0: begin
      imgAuthorPic.Picture := imgSean.Picture;
      sAuthorEmail := 'sean@seanbdurkin.id.au';
      sAuthorWeb   := 'http:\\www.seanbdurkin.id.au'
      end;
   1: begin
      imgAuthorPic.Picture := nil;
      sAuthorEmail := 'crypto@cityinthesky.co.uk';
      sAuthorWeb   := ''
      end;
  end;
if sAuthorEmail <> '' then
    lblAuthorEmail.Caption := DS_Email + sAuthorEmail
  else
    lblAuthorEmail.Caption := '';
if sAuthorWeb <> '' then
    lblAuthorWeb.Caption := DS_Web + sAuthorWeb
  else
    lblAuthorWeb.Caption := '';
end;

procedure TTPLb_fmComponentAbout.OnURL_Click( Sender: TObject);
var
  s: string;
begin
if (Sender is TLabel) then
  begin
  s := TLabel( Sender).Caption;
  ShellExecute( Handle, 'open', PChar( s), nil, nil, SW_SHOWNORMAL)
  end
end;



procedure TTPLb_fmComponentAbout.FormCreate(Sender: TObject);
begin
lblAuthorEmail.Caption := '';
lblAuthorWeb.Caption   := '';
browserIntro.Navigate( WelcomeVideo_URL)
end;

procedure TTPLb_fmComponentAbout.lblTpsfaContactClick(Sender: TObject);
begin
//
end;

procedure TTPLb_fmComponentAbout.UpdateAbout( SelectedComponent: TComponent);
var
  LibName: string;
  FileVersion: string;
  HashComp: THash;
  HashDsc: IHashDsc;
  s: string;
  CodecComp: TCodec;
  Chain: IBlockChainingModel;
  StreamCipher: IStreamCipher;
  BlockCipher: IBlockCipher;
  ErrorPos: integer;
  Reg: TRegistry;
  isDebug: boolean;
  Features1: TAlgorithmicFeatureSet;
  ExtraCipherDescription: string;

{$IF compilerversion >= 21}
  ControlObject: IControlObject;
  Controller: TObject;
  LContext: TRttiContext;
  LType: TRttiType;
  Attr: TCustomAttribute;
  DesignDesc: DesignDescription;
{$IFEND}

  procedure PopulateListBoxWithFeatures( const Features: TAlgorithmicFeatureSet; Lbx: TListBox);
  var
    Feat: TAlgorithmicFeature;
  begin
{$IF compilerversion >= 16}
  // Delphi 2005 or above
    for Feat in Features do
      begin

{$ELSE}
  // Delphi 7 or below
    for Feat := Low( TAlgorithmicFeature) to High( TAlgorithmicFeature) do
      begin
      if not (Feat in Features) then continue;
{$IFEND}

      s := GetEnumName( TypeInfo( TAlgorithmicFeature), Ord( Feat));
      Lbx.Items.Add( s)
      end
  end;

  function WikipediaRelativeURL( const Relative: string): string;
  begin
  result := Relative;
  if (Pos( 'http:' , lowercase( result)) <> 1) and
     (Pos( 'https:', lowercase( result)) <> 1) and
     (result <> '') then
       result := 'http://en.wikipedia.org/wiki/' + result
  end;

  procedure Debug( const Line: string);
  begin
  if isDebug and (Line <> '') then
    lbxHashFeatures.Items.Add( Line)
  end;

  procedure DebugFmt( const Fmt: string; const Args: array of const);
  begin
  Debug( Format( Fmt, Args))
  end;

begin
lbxHashFeatures.Clear;
Reg := TRegistry.Create;
try
  Reg.RootKey := HKEY_CURRENT_USER;
  isDebug := Reg.OpenKeyReadOnly( '\SOFTWARE\TurboPower\LockBox\3') and
             Reg.ReadBool( 'Debug')
finally
Reg.Free
end;
ErrorPos := 0;
try
Get_TP_LockBox3_Info( LibName, FileVersion);
lblRunTimeVersion.Caption := Format( DS_RunTimeIs,
  [LibName, FileVersion]);
Inc( ErrorPos);
Get_dclTP_LockBox3_Info( LibName, FileVersion);
lblDesignTimeVersion.Caption := Format( DS_DesignTimeIs,
  [LibName, FileVersion]);
Inc( ErrorPos);

if SelectedComponent is THash  then
    HashComp := THash( SelectedComponent)
  else
    HashComp := nil;

Inc( ErrorPos);
if assigned( HashComp) and assigned( HashComp.CryptoLibrary) and
   (HashComp.HashId <> '') then
    HashDsc := HashComp.CryptoLibrary.HashIntfc( HashComp.HashId)
  else
    HashDsc := nil;

Inc( ErrorPos);
CodecComp := nil;
if SelectedComponent is TCodec  then
    CodecComp := TCodec( SelectedComponent);

Inc( ErrorPos);
if (SelectedComponent is TSignatory) and
    assigned( TSignatory( SelectedComponent).Codec) and
    TSignatory( SelectedComponent).Codec.isAsymetric  then
      CodecComp := TSignatory( SelectedComponent).Codec;
Inc( ErrorPos);

if assigned( HashComp) then
    begin
    lblHashDisplayName.Caption   := HashComp.Hash;
    PopulateListBoxWithFeatures( HashComp.Features, lbxHashFeatures)
    end
  else
    lblHashDisplayName.Caption   := DS_HashNotSelected;

Inc( ErrorPos);
if assigned( HashDsc) then
    begin
    lblHashDefinitionURL.Caption := HashDsc.DefinitionURL;
    lblHashWikipediaURL.Caption  := WikipediaRelativeURL( HashDsc.WikipediaReference);
    lblHashBlockSize.Caption     := Format( DS_BlockSizeEqs, [HashDsc.UpdateSize]);
    lblHashDigestSize.Caption    := Format( DS_DigestSizeEqs, [HashDsc.DigestSize]);
    end
  else
    begin
    lblHashDefinitionURL.Caption := '';
    lblHashWikipediaURL.Caption  := '';
    lblHashBlockSize.Caption     := '';
    lblHashDigestSize.Caption    := '';
    end;


Inc( ErrorPos);
if assigned( CodecComp) and assigned( CodecComp.CryptoLibrary) and
  (CodecComp.ChainModeId <> '') then
    Chain := CodecComp.CryptoLibrary
      .BlockChainingModelIntfc( CodecComp.ChainModeId)
  else
    Chain := nil;


Inc( ErrorPos);
lbxChainFeatures.Items.Clear;
if assigned( Chain) then
    begin
    lblChainName.Caption := Chain.DisplayName;
    PopulateListBoxWithFeatures( Chain.Features, lbxChainFeatures);
    lblChainWikipedia.Caption  := WikipediaRelativeURL( Chain.WikipediaReference)
    end
  else
    begin
    lblChainName.Caption := DS_ChainModeNotSelected;
    lblChainWikipedia.Caption  := ''
    end;


Inc( ErrorPos);
if assigned( CodecComp) then
    lblCipherDisplayName.Caption := CodecComp.Cipher
  else
    lblCipherDisplayName.Caption := DS_CodecNotSelected;

Inc( ErrorPos);
if assigned( CodecComp) and assigned( CodecComp.CryptoLibrary) and
   (CodecComp.StreamCipherId <> '') then
    StreamCipher := CodecComp.CryptoLibrary
      .StreamCipherIntfc( CodecComp.StreamCipherId)
  else
    StreamCipher := nil;

Inc( ErrorPos);
if assigned( StreamCipher) and (afBlockAdapter in StreamCipher.Features) and
   (CodecComp.BlockCipherId <> '') then
    begin
    BlockCipher := CodecComp.CryptoLibrary.BlockCipherIntfc( CodecComp.BlockCipherId);
    if assigned( BlockCipher) then
      StreamCipher := nil
    end
  else
    BlockCipher := nil;

ExtraCipherDescription := '';
memoExtraCipherDescription.Visible := False;
pnlCipherDescription.Height := 209;

{$IF compilerversion >= 21}
if not Supports( BlockCipher, IControlObject, ControlObject) then
  Supports( StreamCipher, IControlObject, ControlObject);
if assigned( ControlObject) then
    Controller := ControlObject.ControlObject
  else
    Controller := nil;
DesignDesc := nil;
if assigned( Controller) then
  begin
  LContext := TRttiContext.Create;
  try
    LType := LContext.GetType( Controller.ClassType);
    for Attr in LType.GetAttributes do
      begin
      if not (Attr is DesignDescription) then continue;
      DesignDesc := DesignDescription( Attr);
      break
      end;
    if assigned( DesignDesc) then
        ExtraCipherDescription := DesignDesc.Description
  finally
    LContext.Free
    end
  end;
{$IFEND}

if ExtraCipherDescription <> '' then
  begin
  memoExtraCipherDescription.Height := 100;
  pnlCipherDescription.Height := pnlCipherDescription.Height +
                              memoExtraCipherDescription.Height;
  memoExtraCipherDescription.Visible := True;
  memoExtraCipherDescription.Lines.Text := ExtraCipherDescription
  end;

Inc( ErrorPos);
lbxCipherFeatures.Items.Clear;
if assigned( StreamCipher) then
    begin
    ErrorPos := 20;
    PopulateListBoxWithFeatures( StreamCipher.Features, lbxCipherFeatures);
    lblCipherDefinitionURL.Caption := StreamCipher.DefinitionURL;
    lblCipherWikipediaURL.Caption  := WikipediaRelativeURL( StreamCipher.WikipediaReference);
    lblCipherBlockSize.Caption := ''
    end
  else if assigned( BlockCipher) then
    begin
    ErrorPos := 30;
    Features1 := BlockCipher.Features;
    Include( Features1, afBlockAdapter);
    PopulateListBoxWithFeatures( Features1, lbxCipherFeatures);
    lblCipherDefinitionURL.Caption := BlockCipher.DefinitionURL;
    lblCipherWikipediaURL.Caption  := WikipediaRelativeURL( BlockCipher.WikipediaReference);
    lblCipherBlockSize.Caption := Format( DS_BlockSizeEqs, [BlockCipher.BlockSize])
    end
  else
    begin
    ErrorPos := 40;
    lblCipherDefinitionURL.Caption := '';
    lblCipherWikipediaURL.Caption  := '';
    lblCipherBlockSize.Caption := ''
    end
except on E: Exception do
  begin
  DebugFmt( 'ERROR! (%d)', [ErrorPos]);
  DebugFmt( 'Class = %s', [E.ClassName]);
  DebugFmt( 'Error = %s', [E.Message])
  end;
end
end;

end.
