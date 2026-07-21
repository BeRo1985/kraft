program threadprobe;
{$ifdef fpc}
 {$mode delphi}
{$endif}
{$apptype console}
uses {$ifdef unix}cthreads,{$endif} SysUtils,kraft;
var Physics:TKraft;
begin
 Physics:=TKraft.Create(-1);
 WriteLn('CountThreads(-1)=',Physics.CountThreads);
 Physics.Free;
 Physics:=TKraft.Create(4);
 WriteLn('CountThreads(4)=',Physics.CountThreads);
 Physics.Free;
end.
