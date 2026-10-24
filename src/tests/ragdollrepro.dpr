program ragdollrepro;

{$ifdef fpc}
 {$mode delphi}
{$endif}
{$apptype console}

uses {$ifdef unix}cthreads,{$endif} SysUtils,Math,kraft;

{$I ragdollcommon.inc}

procedure RunScenario(const aName:string;const aSolverMode:TKraftSolverMode;const aTGSJointMode:TKraftTGSJointMode;const Limits:TScenarioLimits);
const StepCount=3600; // 30 seconds at 120 Hz
var Physics:TKraft;
    StepIndex,Index,WorstIndex:longint;
    MaxAngVel,MaxLinVel,AngVel,LinVel,PeakAngVel,LateMaxAngVel,LateMaxLinVel:TKraftScalar;
    ExplodedAt:longint;
begin
 Physics:=TKraft.Create(-1);
 try
  Physics.SetFrequency(120.0);
  Physics.VelocityIterations:=8;
  Physics.PositionIterations:=3;
  Physics.Gravity.y:=-9.81;
  Physics.SolverMode:=aSolverMode;
  Physics.TGSJointMode:=aTGSJointMode;
  BuildScene(Physics,Limits);
  ExplodedAt:=-1;
  PeakAngVel:=0.0;
  LateMaxAngVel:=0.0;
  LateMaxLinVel:=0.0;
  WorstIndex:=-1;
  for StepIndex:=1 to StepCount do begin
   Physics.Step(1.0/120.0);
   MaxAngVel:=0.0;
   MaxLinVel:=0.0;
   for Index:=0 to length(RagdollBodies)-1 do begin
    AngVel:=Vector3Length(RagdollBodies[Index].AngularVelocity);
    LinVel:=Vector3Length(RagdollBodies[Index].LinearVelocity);
    if IsNaN(AngVel) or IsNaN(LinVel) then begin
     AngVel:=1e30;
    end;
    if AngVel>MaxAngVel then begin
     MaxAngVel:=AngVel;
     WorstIndex:=Index;
    end;
    if LinVel>MaxLinVel then begin
     MaxLinVel:=LinVel;
    end;
   end;
   if MaxAngVel>PeakAngVel then begin
    PeakAngVel:=MaxAngVel;
   end;
   if StepIndex>(StepCount-1200) then begin
    LateMaxAngVel:=Max(LateMaxAngVel,MaxAngVel);
    LateMaxLinVel:=Max(LateMaxLinVel,MaxLinVel);
   end;
   if ((MaxAngVel>100.0) or (MaxLinVel>50.0)) and (ExplodedAt<0) then begin
    ExplodedAt:=StepIndex;
    WriteLn('  EXPLODED at step ',StepIndex,' (t=',(StepIndex/120.0):0:2,'s) worst body ',BodyNames[WorstIndex],' angvel ',MaxAngVel:0:1,' linvel ',MaxLinVel:0:1);
    break;
   end;
   if (StepIndex mod 600)=0 then begin
    WriteLn('  t=',(StepIndex/120.0):0:1,'s maxAngVel=',MaxAngVel:0:2,' maxLinVel=',MaxLinVel:0:2,' worst=',BodyNames[WorstIndex]);
   end;
  end;
  if ExplodedAt<0 then begin
   WriteLn('  STABLE for ',(StepCount/120.0):0:0,'s, peak angvel ',PeakAngVel:0:2,', last-10s max angvel ',LateMaxAngVel:0:3,' linvel ',LateMaxLinVel:0:3);
  end;
 finally
  Physics.Free;
 end;
end;

begin
 FormatSettings.DecimalSeparator:='.';
 WriteLn('=== SI / full limits ===');
 RunScenario('si-full',ksmSequentialImpulse,ktjmAdapter,slFull);
 WriteLn('=== SI / no cone-twist ===');
 RunScenario('si-noconetwist',ksmSequentialImpulse,ktjmAdapter,slNoConeTwist);
 WriteLn('=== SI / no hinge limits ===');
 RunScenario('si-nohingelimits',ksmSequentialImpulse,ktjmAdapter,slNoHingeLimits);
 WriteLn('=== SI / no limits ===');
 RunScenario('si-nolimits',ksmSequentialImpulse,ktjmAdapter,slNoLimits);
 WriteLn('=== TGS adapter / full ===');
 RunScenario('tgsadapter-full',ksmTGSSoft,ktjmAdapter,slFull);
 WriteLn('=== TGS native / full ===');
 RunScenario('tgsnative-full',ksmTGSSoft,ktjmNativeSoft,slFull);
 WriteLn('=== TGS native / no cone-twist ===');
 RunScenario('tgsnative-noconetwist',ksmTGSSoft,ktjmNativeSoft,slNoConeTwist);
end.
