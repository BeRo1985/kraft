program ragdollrepro3;

{$ifdef fpc}
 {$mode delphi}
{$endif}
{$apptype console}

uses {$ifdef unix}cthreads,{$endif} SysUtils,Math,kraft;

{$I ragdollcommon.inc}

var StepIndex,Index:longint;
    Physics:TKraft;
    MaxAngVel,MaxLinVel,AngVel,LinVel:TKraftScalar;
    WorstAng,WorstLin:longint;
begin
 FormatSettings.DecimalSeparator:='.';
 Physics:=TKraft.Create(-1);
 Physics.SetFrequency(120.0);
 Physics.VelocityIterations:=8;
 Physics.PositionIterations:=3;
 Physics.Gravity.y:=-9.81;
 Physics.SolverMode:=ksmTGSSoft;
 Physics.TGSJointMode:=ktjmNativeSoft;
 BuildScene(Physics,slFull);
 for StepIndex:=1 to 720 do begin
  Physics.Step(1.0/120.0);
  MaxAngVel:=0.0;
  MaxLinVel:=0.0;
  WorstAng:=0;
  WorstLin:=0;
  for Index:=0 to 10 do begin
   AngVel:=Vector3Length(RagdollBodies[Index].AngularVelocity);
   LinVel:=Vector3Length(RagdollBodies[Index].LinearVelocity);
   if AngVel>MaxAngVel then begin
    MaxAngVel:=AngVel;
    WorstAng:=Index;
   end;
   if LinVel>MaxLinVel then begin
    MaxLinVel:=LinVel;
    WorstLin:=Index;
   end;
  end;
  if ((StepIndex mod 12)=0) or (MaxLinVel>6.0) or (MaxAngVel>20.0) then begin
   WriteLn(StepIndex:5,' t=',(StepIndex/120.0):0:3,
           ' angMax=',MaxAngVel:8:2,' (',BodyNames[WorstAng],')',
           ' linMax=',MaxLinVel:8:2,' (',BodyNames[WorstLin],')',
           ' pelvis=(',RagdollBodies[0].Sweep.c.x:0:2,',',RagdollBodies[0].Sweep.c.y:0:2,',',RagdollBodies[0].Sweep.c.z:0:2,')');
  end;
 end;
end.
