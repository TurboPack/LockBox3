// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Vcl.uTPLb_ComponentAbout.pas' rev: 30.00 (Windows)

#ifndef Vcl_Utplb_componentaboutHPP
#define Vcl_Utplb_componentaboutHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Winapi.Messages.hpp>
#include <System.SysUtils.hpp>
#include <System.Variants.hpp>
#include <System.Classes.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Dialogs.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.OleCtrls.hpp>
#include <SHDocVw.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.Imaging.jpeg.hpp>

//-- user supplied -----------------------------------------------------------

namespace Vcl
{
namespace Utplb_componentabout
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TTPLb_fmComponentAbout;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TTPLb_fmComponentAbout : public Vcl::Forms::TForm
{
	typedef Vcl::Forms::TForm inherited;
	
__published:
	Vcl::Extctrls::TPanel* pnlAbout;
	Vcl::Stdctrls::TButton* btnClose;
	Vcl::Comctrls::TPageControl* pgAboutContent;
	Vcl::Comctrls::TTabSheet* tsMain;
	Vcl::Comctrls::TTabSheet* tsWelcome;
	Vcl::Comctrls::TTabSheet* tsProject;
	Vcl::Comctrls::TTabSheet* tsCopyLeft;
	Vcl::Comctrls::TTabSheet* tsAuthors;
	Vcl::Comctrls::TTabSheet* tsSupport;
	Vcl::Comctrls::TTabSheet* tsAlgorithms;
	Vcl::Extctrls::TImage* Image1;
	Vcl::Stdctrls::TLabel* lblTitle;
	Vcl::Stdctrls::TLabel* Label1;
	Vcl::Stdctrls::TLabel* lblRunTimeVersion;
	Vcl::Stdctrls::TLabel* lblDesignTimeVersion;
	Vcl::Stdctrls::TMemo* Memo1;
	Shdocvw::TWebBrowser* browserIntro;
	Vcl::Stdctrls::TLabel* Label4;
	Vcl::Stdctrls::TMemo* Memo2;
	Vcl::Stdctrls::TLabel* lblWebsite;
	Vcl::Stdctrls::TLabel* Label7;
	Vcl::Stdctrls::TLabel* Label8;
	Vcl::Stdctrls::TMemo* Memo3;
	Vcl::Stdctrls::TLabel* Label9;
	Vcl::Stdctrls::TMemo* Memo4;
	Vcl::Stdctrls::TListBox* lbxAuthors;
	Vcl::Extctrls::TImage* imgAuthorPic;
	Vcl::Extctrls::TBevel* Bevel1;
	Vcl::Stdctrls::TLabel* lblAuthorEmail;
	Vcl::Stdctrls::TLabel* lblAuthorWeb;
	Vcl::Stdctrls::TLabel* Label12;
	Vcl::Stdctrls::TMemo* memoSupport;
	Vcl::Stdctrls::TMemo* memoHelp;
	Vcl::Extctrls::TPanel* Panel1;
	Vcl::Stdctrls::TLabel* Label13;
	Vcl::Stdctrls::TLabel* lblHashDisplayName;
	Vcl::Stdctrls::TLabel* Label15;
	Vcl::Stdctrls::TListBox* lbxHashFeatures;
	Vcl::Stdctrls::TLabel* lblHashDefinitionURL;
	Vcl::Stdctrls::TLabel* lblHashWikipediaURL;
	Vcl::Stdctrls::TLabel* lblHashBlockSize;
	Vcl::Stdctrls::TLabel* lblHashDigestSize;
	Vcl::Extctrls::TPanel* pnlCipherDescription;
	Vcl::Stdctrls::TLabel* Label20;
	Vcl::Stdctrls::TLabel* lblCipherDisplayName;
	Vcl::Stdctrls::TLabel* Label22;
	Vcl::Stdctrls::TLabel* lblCipherDefinitionURL;
	Vcl::Stdctrls::TLabel* lblCipherWikipediaURL;
	Vcl::Stdctrls::TLabel* lblCipherBlockSize;
	Vcl::Stdctrls::TListBox* lbxCipherFeatures;
	Vcl::Extctrls::TPanel* Panel3;
	Vcl::Stdctrls::TLabel* Label27;
	Vcl::Stdctrls::TLabel* lblChainName;
	Vcl::Stdctrls::TLabel* Label29;
	Vcl::Stdctrls::TLabel* lblChainWikipedia;
	Vcl::Stdctrls::TListBox* lbxChainFeatures;
	Vcl::Stdctrls::TLabel* lblLogoAttribution;
	Vcl::Stdctrls::TStaticText* lblTpsfaIntro;
	Vcl::Stdctrls::TStaticText* lblTpsfaContact;
	Vcl::Stdctrls::TStaticText* lblWebLabel;
	Vcl::Extctrls::TImage* imgSean;
	Vcl::Stdctrls::TMemo* memoExtraCipherDescription;
	Vcl::Stdctrls::TMemo* memoWelcomeInstructions;
	void __fastcall lblTpsfaContactClick(System::TObject* Sender);
	void __fastcall OnURL_Click(System::TObject* Sender);
	void __fastcall lbxAuthorsClick(System::TObject* Sender);
	void __fastcall FormCreate(System::TObject* Sender);
	
public:
	void __fastcall UpdateAbout(System::Classes::TComponent* SelectedComponent);
public:
	/* TCustomForm.Create */ inline __fastcall virtual TTPLb_fmComponentAbout(System::Classes::TComponent* AOwner) : Vcl::Forms::TForm(AOwner) { }
	/* TCustomForm.CreateNew */ inline __fastcall virtual TTPLb_fmComponentAbout(System::Classes::TComponent* AOwner, int Dummy) : Vcl::Forms::TForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TTPLb_fmComponentAbout(void) { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TTPLb_fmComponentAbout(HWND ParentWindow) : Vcl::Forms::TForm(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
#define WelcomeVideo_URL L"http://lockbox.seanbdurkin.id.au/online_resources/Intro3_E"\
	L"mbedded.html"
extern DELPHI_PACKAGE TTPLb_fmComponentAbout* TPLb_fmComponentAbout;
}	/* namespace Utplb_componentabout */
}	/* namespace Vcl */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VCL_UTPLB_COMPONENTABOUT)
using namespace Vcl::Utplb_componentabout;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VCL)
using namespace Vcl;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Vcl_Utplb_componentaboutHPP
