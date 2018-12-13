// CodeGear C++Builder
// Copyright (c) 1995, 2016 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FMX.uTPLb_ComponentAbout.pas' rev: 32.00 (Windows)

#ifndef Fmx_Utplb_componentaboutHPP
#define Fmx_Utplb_componentaboutHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>
#include <System.Types.hpp>
#include <System.UITypes.hpp>
#include <System.Classes.hpp>
#include <System.Variants.hpp>
#include <System.IniFiles.hpp>
#include <Data.DB.hpp>
#include <FMX.Types.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Dialogs.hpp>
#include <FMX.Objects.hpp>
#include <FMX.Menus.hpp>
#include <FMX.Grid.hpp>
#include <FMX.ExtCtrls.hpp>
#include <FMX.ListBox.hpp>
#include <FMX.TreeView.hpp>
#include <FMX.Memo.hpp>
#include <FMX.TabControl.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.Edit.hpp>
#include <FMX.Platform.hpp>
#include <Fmx.Bind.DBEngExt.hpp>
#include <Fmx.Bind.Editors.hpp>
#include <Fmx.Bind.DBLinks.hpp>
#include <Fmx.Bind.Navigator.hpp>
#include <Data.Bind.EngExt.hpp>
#include <Data.Bind.Components.hpp>
#include <Data.Bind.DBScope.hpp>
#include <Data.Bind.DBLinks.hpp>
#include <Datasnap.DBClient.hpp>
#include <Winapi.ShellAPI.hpp>
#include <Fmx.Bind.Grid.hpp>
#include <System.Rtti.hpp>
#include <System.Bindings.Outputs.hpp>
#include <Data.Bind.Grid.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Header.hpp>
#include <FMX.Graphics.hpp>
#include <FMX.WebBrowser.hpp>
#include <FMX.ScrollBox.hpp>
#include <FMX.Controls.Presentation.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fmx
{
namespace Utplb_componentabout
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TTPLb_fmComponentAbout;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TTPLb_fmComponentAbout : public Fmx::Forms::TForm
{
	typedef Fmx::Forms::TForm inherited;
	
__published:
	Fmx::Stdctrls::TPanel* pnlAbout;
	Fmx::Stdctrls::TButton* btnClose;
	Fmx::Tabcontrol::TTabControl* pgAboutContent;
	Fmx::Tabcontrol::TTabItem* tsMain;
	Fmx::Tabcontrol::TTabItem* tsWelcome;
	Fmx::Tabcontrol::TTabItem* tsProject;
	Fmx::Tabcontrol::TTabItem* tsCopyLeft;
	Fmx::Tabcontrol::TTabItem* tsAuthors;
	Fmx::Tabcontrol::TTabItem* tsSupport;
	Fmx::Tabcontrol::TTabItem* tsAlgorithms;
	Fmx::Objects::TImage* Image1;
	Fmx::Stdctrls::TLabel* lblTitle;
	Fmx::Stdctrls::TLabel* Label1;
	Fmx::Stdctrls::TLabel* lblRunTimeVersion;
	Fmx::Stdctrls::TLabel* lblDesignTimeVersion;
	Fmx::Memo::TMemo* Memo1;
	Fmx::Webbrowser::TWebBrowser* browserIntro;
	Fmx::Stdctrls::TLabel* Label4;
	Fmx::Memo::TMemo* Memo2;
	Fmx::Stdctrls::TLabel* lblWebsite;
	Fmx::Stdctrls::TLabel* Label7;
	Fmx::Stdctrls::TLabel* Label8;
	Fmx::Memo::TMemo* Memo3;
	Fmx::Stdctrls::TLabel* Label9;
	Fmx::Memo::TMemo* Memo4;
	Fmx::Listbox::TListBox* lbxAuthors;
	Fmx::Objects::TImage* imgAuthorPic;
	Fmx::Stdctrls::TPanel* Bevel1;
	Fmx::Stdctrls::TLabel* lblAuthorEmail;
	Fmx::Stdctrls::TLabel* lblAuthorWeb;
	Fmx::Stdctrls::TLabel* Label12;
	Fmx::Memo::TMemo* memoSupport;
	Fmx::Memo::TMemo* memoHelp;
	Fmx::Stdctrls::TPanel* Panel1;
	Fmx::Stdctrls::TLabel* Label13;
	Fmx::Stdctrls::TLabel* lblHashDisplayName;
	Fmx::Stdctrls::TLabel* Label15;
	Fmx::Listbox::TListBox* lbxHashFeatures;
	Fmx::Stdctrls::TLabel* lblHashDefinitionURL;
	Fmx::Stdctrls::TLabel* lblHashWikipediaURL;
	Fmx::Stdctrls::TLabel* lblHashBlockSize;
	Fmx::Stdctrls::TLabel* lblHashDigestSize;
	Fmx::Stdctrls::TPanel* pnlCipherDescription;
	Fmx::Stdctrls::TLabel* Label20;
	Fmx::Stdctrls::TLabel* lblCipherDisplayName;
	Fmx::Stdctrls::TLabel* Label22;
	Fmx::Stdctrls::TLabel* lblCipherDefinitionURL;
	Fmx::Stdctrls::TLabel* lblCipherWikipediaURL;
	Fmx::Stdctrls::TLabel* lblCipherBlockSize;
	Fmx::Listbox::TListBox* lbxCipherFeatures;
	Fmx::Stdctrls::TPanel* Panel3;
	Fmx::Stdctrls::TLabel* Label27;
	Fmx::Stdctrls::TLabel* lblChainName;
	Fmx::Stdctrls::TLabel* Label29;
	Fmx::Stdctrls::TLabel* lblChainWikipedia;
	Fmx::Listbox::TListBox* lbxChainFeatures;
	Fmx::Stdctrls::TLabel* lblLogoAttribution;
	Fmx::Stdctrls::TLabel* lblTpsfaIntro;
	Fmx::Stdctrls::TLabel* lblTpsfaContact;
	Fmx::Stdctrls::TLabel* lblWebLabel;
	Fmx::Objects::TImage* imgSean;
	Fmx::Memo::TMemo* memoExtraCipherDescription;
	Fmx::Memo::TMemo* memoWelcomeInstructions;
	void __fastcall lblTpsfaContactClick(System::TObject* Sender);
	void __fastcall OnURL_Click(System::TObject* Sender);
	void __fastcall lbxAuthorsClick(System::TObject* Sender);
	void __fastcall FormCreate(System::TObject* Sender);
	
public:
	void __fastcall UpdateAbout(System::Classes::TComponent* SelectedComponent);
public:
	/* TCustomForm.Create */ inline __fastcall virtual TTPLb_fmComponentAbout(System::Classes::TComponent* AOwner) : Fmx::Forms::TForm(AOwner) { }
	/* TCustomForm.CreateNew */ inline __fastcall virtual TTPLb_fmComponentAbout(System::Classes::TComponent* AOwner, NativeInt Dummy) : Fmx::Forms::TForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TTPLb_fmComponentAbout(void) { }
	
};


//-- var, const, procedure ---------------------------------------------------
#define WelcomeVideo_URL L"http://lockbox.seanbdurkin.id.au/online_resources/Intro3_E"\
	L"mbedded.html"
extern DELPHI_PACKAGE TTPLb_fmComponentAbout* TPLb_fmComponentAbout;
}	/* namespace Utplb_componentabout */
}	/* namespace Fmx */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX_UTPLB_COMPONENTABOUT)
using namespace Fmx::Utplb_componentabout;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX)
using namespace Fmx;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Fmx_Utplb_componentaboutHPP
