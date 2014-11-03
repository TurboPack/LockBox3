unit uTPLb_StrUtils;

interface

uses
  SysUtils;

function AnsiBytesOf(const S: string): TBytes;

implementation

function AnsiBytesOf(const S: string): TBytes;
begin
  Result := TEncoding.ANSI.GetBytes(S);
end;

end.
