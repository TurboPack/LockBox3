unit uTPLb_SVN_Keywords;

interface

const
  TPLB3Runtime_SVN_Keyword_Date    : string = '$Date: 2014-06-24 12:42:00 +0200 (Di, 24. Jun 2014) $';
  TPLB3Runtime_SVN_Keyword_Revision: string = '$Revision: 267 $';
  TPLB3Runtime_SVN_Keyword_Author  : string = '$Author: romankassebaum $';
  TPLB3Runtime_SVN_Keyword_HeadURL : string = '$HeadURL: https://svn.code.sf.net/p/tplockbox/code/trunc/run/utilities/uTPLb_SVN_Keywords.pas $';
  TPLB3Runtime_SVN_Keyword_Id      : string = '$Id: uTPLb_SVN_Keywords.pas 267 2014-06-24 10:42:00Z romankassebaum $';


function TPLB3Runtime_SVN_Revision: integer;


implementation

{$WARNINGS OFF}
function TPLB3Runtime_SVN_Revision: integer;
var
  s, Pattern: string;
  P, Code: integer;
begin
s := TPLB3Runtime_SVN_Keyword_Revision;
Pattern := '$Revision: ';
P := Pos( Pattern, s);
if P > 0 then
  Delete( s, P, Length( Pattern));
Pattern := ' ';
P := Pos( Pattern, s);
if P > 0 then
  SetLength( s, P-1);
Val( s, result, Code);
if (s = '') or (Code <> 0) then
  result := -1 // Signifying unknown version
end;
{$WARNINGS ON}

{ Here is a scratch pad area for forcing changes to file in SVN.

}
end.
