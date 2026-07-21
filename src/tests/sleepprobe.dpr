program sleepprobe;

{$ifdef fpc}
 {$mode delphi}
{$endif}
{$apptype console}

uses {$ifdef unix}cthreads,{$endif} SysUtils,Math,kraft;

// Probe for the K6 stage 5 sleep parity: reproduces the stacktest scene and reports the first step at
// which the whole stack is asleep, per solver parallel mode, so the persistent island sleep pass of the
// global graph mode can be compared against the classic island sleep timing.

procedure RunProbe(const aName:string;const aMode:TKraftSolverParallelMode);
const StepCount=1800;
var Physics:TKraft;
    FloorBody:TKraftRigidBody;
    FloorShape:TKraftShapePlane;
    BoxBody:TKraftRigidBody;
    ShapeBox:TKraftShapeBox;
    Boxes:array[0..9] of TKraftRigidBody;
    StepIndex,BoxIndex,FirstAsleepStep,ReportedSteps,PreAwakeCount:longint;
    AwakeMask,PreAwakeMask:longword;
    AllAsleep,PreFloorAwake:boolean;
begin
 Physics:=TKraft.Create(-1);
 try
  Physics.SetFrequency(120.0);
  Physics.VelocityIterations:=8;
  Physics.PositionIterations:=3;
  Physics.SpeculativeIterations:=8;
  Physics.TimeOfImpactIterations:=20;
  Physics.Gravity.y:=-9.81;
  Physics.SolverMode:=ksmTGSSoft;
  Physics.TGSJointMode:=ktjmNativeSoft;
  Physics.SolverParallelMode:=aMode;

  FloorBody:=TKraftRigidBody.Create(Physics);
  FloorBody.SetRigidBodyType(krbtSTATIC);
  FloorShape:=TKraftShapePlane.Create(Physics,FloorBody,Plane(Vector3Norm(Vector3(0.0,1.0,0.0)),0.0));
  FloorShape.Restitution:=0.4;
  FloorBody.Finish;
  FloorBody.SetWorldTransformation(Matrix4x4Translate(0.0,0.0,0.0));
  FloorBody.CollisionGroups:=[0];

  for BoxIndex:=0 to 9 do begin
   BoxBody:=TKraftRigidBody.Create(Physics);
   BoxBody.SetRigidBodyType(krbtDYNAMIC);
   ShapeBox:=TKraftShapeBox.Create(Physics,BoxBody,Vector3(0.25,0.25,0.25));
   ShapeBox.Restitution:=0.4;
   ShapeBox.Density:=100.0;
   BoxBody.Finish;
   BoxBody.SetWorldTransformation(Matrix4x4Translate(0.0,ShapeBox.Extents.y+(BoxIndex*(ShapeBox.Extents.y*2.0)),0.0));
   BoxBody.CollisionGroups:=[0];
   Boxes[BoxIndex]:=BoxBody;
  end;

  FirstAsleepStep:=-1;
  ReportedSteps:=0;
  for StepIndex:=1 to StepCount do begin
   PreAwakeCount:=Physics.CountAwakeRigidBodies;
   PreAwakeMask:=0;
   for BoxIndex:=0 to 9 do begin
    if krbfAwake in Boxes[BoxIndex].Flags then begin
     PreAwakeMask:=PreAwakeMask or (longword(1) shl BoxIndex);
    end;
   end;
   PreFloorAwake:=krbfAwake in FloorBody.Flags;
   Physics.Step(1.0/120.0);
   AllAsleep:=true;
   AwakeMask:=0;
   for BoxIndex:=0 to 9 do begin
    if krbfAwake in Boxes[BoxIndex].Flags then begin
     AllAsleep:=false;
     AwakeMask:=AwakeMask or (longword(1) shl BoxIndex);
    end;
   end;
   if AllAsleep then begin
    if FirstAsleepStep<0 then begin
     FirstAsleepStep:=StepIndex;
    end;
   end else begin
    FirstAsleepStep:=-1;
   end;
   // Report the steps which still run narrow phase work after the stack fell asleep once
   if (FirstAsleepStep>0) and (StepIndex>FirstAsleepStep) and (Physics.ContactManager.CountRecycledContactPairs>0) and (ReportedSteps<12) then begin
    inc(ReportedSteps);
    WriteLn(aName,': step ',StepIndex,' recycled=',Physics.ContactManager.CountRecycledContactPairs,
            ' preAwakeCount=',PreAwakeCount,' preAwakeMask=',PreAwakeMask,' preFloorAwake=',PreFloorAwake,
            ' postAwakeCount=',Physics.CountAwakeRigidBodies,' postAwakeMask=',AwakeMask);
   end;
  end;

  WriteLn(aName,': first all-asleep step=',FirstAsleepStep);
 finally
  Physics.Free;
 end;
end;

begin
 FormatSettings.DecimalSeparator:='.';
 RunProbe('islands',kspmIslands);
 RunProbe('colored',kspmIslandColored);
 RunProbe('global ',kspmGlobalGraph);
end.
