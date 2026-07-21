program invtest2;
{$ifdef fpc}
 {$mode delphi}
{$endif}
{$apptype console}
uses {$ifdef unix}cthreads,{$endif} SysUtils,kraft;
var s,r:TKraftMatrix3x3;
    ok:boolean;
begin
 FormatSettings.DecimalSeparator:='.';
 FillChar(s,SizeOf(s),0);
 s[0,0]:=1.0;
 s[1,1]:=0.0857;
 s[2,2]:=1.0;
 ok:=Matrix3x3Inverse(r,s);
 WriteLn('inverse ok=',ok,' diag=(',r[0,0]:0:4,',',r[1,1]:0:4,',',r[2,2]:0:4,')');
 Matrix3x3ScalarMul(r,38300.0);
 WriteLn('after scalarmul diag=(',r[0,0]:0:1,',',r[1,1]:0:1,',',r[2,2]:0:1,')');
end.
