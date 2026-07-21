program solverbench;

{$ifdef fpc}
 {$mode delphi}
{$endif}
{$apptype console}

uses {$ifdef unix}cthreads,{$endif} SysUtils,Math,kraft;

// Profiling probe for the K6 discussion: one BIG island (a dense box pile on a plane, everything touching,
// sleeping disabled) and a many-small-islands control scene (isolated stacks far apart). Reports the phase
// time breakdown at different thread counts, to show how much solver time there is to win by intra-island
// parallelism and what the current island-granular parallelism does (nothing for one island, a lot for many).

procedure RunBench(const aName:string;const aSolverMode:TKraftSolverMode;const aTGSJointMode:TKraftTGSJointMode;const aCountThreads:longint;const aManyIslands:boolean);
const StepCount=600;
var Physics:TKraft;
    FloorBody,BoxBody:TKraftRigidBody;
    FloorShape:TKraftShapePlane;
    ShapeBox:TKraftShapeBox;
    StepIndex,BoxX,BoxY,BoxZ:longint;
    Spacing:TKraftScalar;
begin
 Physics:=TKraft.Create(aCountThreads);
 try
  Physics.SetFrequency(120.0);
  Physics.VelocityIterations:=8;
  Physics.PositionIterations:=3;
  Physics.SpeculativeIterations:=8;
  Physics.TimeOfImpactIterations:=20;
  Physics.Gravity.y:=-9.81;
  Physics.SolverMode:=aSolverMode;
  Physics.TGSJointMode:=aTGSJointMode;
  if GetEnvironmentVariable('KRAFT_SPM')='colored' then begin
   Physics.SolverParallelMode:=kspmIslandColored;
  end else begin
   if GetEnvironmentVariable('KRAFT_SPM')='global' then begin
    Physics.SolverParallelMode:=kspmGlobalGraph;
   end;
  end;
  if GetEnvironmentVariable('KRAFT_PIPE')='0' then begin
   Physics.SolverStagePipeline:=false;
  end;
  if GetEnvironmentVariable('KRAFT_WIDE')='1' then begin
   Physics.WideContactSolver:=true;
  end;
  if GetEnvironmentVariable('KRAFT_FUSE')='0' then begin
   Physics.SolverFusedColorStages:=false;
  end;

  FloorBody:=TKraftRigidBody.Create(Physics);
  FloorBody.SetRigidBodyType(krbtSTATIC);
  FloorShape:=TKraftShapePlane.Create(Physics,FloorBody,Plane(Vector3Norm(Vector3(0.0,1.0,0.0)),0.0));
  FloorShape.Restitution:=0.1;
  FloorBody.Finish;
  FloorBody.SetWorldTransformation(Matrix4x4Translate(0.0,0.0,0.0));
  FloorBody.CollisionGroups:=[0];

  // 8x8 columns of 4 boxes: dense (one island, everything AABB-connected over the shared plane neighbors)
  // versus spread out (64 separate islands).
  if aManyIslands then begin
   Spacing:=4.0;
  end else begin
   Spacing:=0.4999;
  end;
  for BoxY:=0 to 3 do begin
   for BoxZ:=0 to 7 do begin
    for BoxX:=0 to 7 do begin
     BoxBody:=TKraftRigidBody.Create(Physics);
     BoxBody.SetRigidBodyType(krbtDynamic);
     ShapeBox:=TKraftShapeBox.Create(Physics,BoxBody,Vector3(0.25,0.25,0.25));
     ShapeBox.Restitution:=0.1;
     ShapeBox.Density:=100.0;
     BoxBody.Flags:=BoxBody.Flags-[krbfAllowSleep];
     BoxBody.Finish;
     BoxBody.SetWorldTransformation(Matrix4x4Translate((BoxX-3.5)*Spacing,0.25+(BoxY*0.51),(BoxZ-3.5)*Spacing));
     BoxBody.CollisionGroups:=[0];
    end;
   end;
  end;

  for StepIndex:=1 to StepCount do begin
   Physics.Step(1.0/120.0);
  end;

  WriteLn(aName,': islands=',Physics.CountIslands,
          ' broad=',Physics.BroadPhaseTime,
          ' narrow=',Physics.NarrowPhaseTime,
          ' solver=',Physics.SolverTime,
          ' total=',Physics.TotalTime);
 finally
  Physics.Free;
 end;
end;

begin
 FormatSettings.DecimalSeparator:='.';
 RunBench('NAT 1-island  1T',ksmTGSSoft,ktjmNativeSoft,1,false);
 RunBench('NAT 1-island  8T',ksmTGSSoft,ktjmNativeSoft,8,false);
 RunBench('NAT 64-island 1T',ksmTGSSoft,ktjmNativeSoft,1,true);
 RunBench('NAT 64-island 8T',ksmTGSSoft,ktjmNativeSoft,8,true);
 RunBench('SI  1-island  1T',ksmSequentialImpulse,ktjmAdapter,1,false);
 RunBench('SI  1-island  8T',ksmSequentialImpulse,ktjmAdapter,8,false);
end.
