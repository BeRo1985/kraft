program invtest;
{$ifdef fpc}
 {$mode delphi}
{$endif}
{$apptype console}
uses {$ifdef unix}cthreads,{$endif} SysUtils,kraft;
var m,r:TKraftMatrix3x3;
    ok:boolean;
begin
 FormatSettings.DecimalSeparator:='.';
 FillChar(m,SizeOf(m),0);
 m[0,0]:=2.611e-5;
 m[1,1]:=2.24e-6;
 m[2,2]:=2.611e-5;
 ok:=Matrix3x3InverseRobust(r,m);
 WriteLn('ok=',ok,' diag=(',r[0,0]:0:2,',',r[1,1]:0:2,',',r[2,2]:0:2,')');
end.
