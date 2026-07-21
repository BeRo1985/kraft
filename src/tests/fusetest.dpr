program fusetest;

{$ifdef fpc}
 {$mode delphi}
{$endif}
{$apptype console}

uses {$ifdef unix}cthreads,{$endif} SysUtils,Math,kraft;

// Stress and bit identity harness for the fused color stages: one big island whose fused publications
// carry BOTH segments (joint blocks and contact blocks) at 32 threads. A dense box pile settles under
// gravity while the boxes of its top layer are chained pairwise with ball socket joints, so every solver
// round publishes mixed joint plus contact stages per color. After a fixed number of steps every body's
// state dumps as raw hex bits; runs with different toggles (KRAFT_WIDE/KRAFT_PIPE) and repeated runs must
// be bit identical to each other.

procedure DumpScalar(const aValue:TKraftScalar);
var CastedValue:longword absolute aValue;
begin
 Write(IntToHex(CastedValue,8));
end;

procedure RunDump;
const StepCount=240;
var Physics:TKraft;
    FloorBody,BoxBody:TKraftRigidBody;
    FloorShape:TKraftShapePlane;
    ShapeBox:TKraftShapeBox;
    StepIndex,BoxX,BoxY,BoxZ:longint;
    Bodies:array[0..255] of TKraftRigidBody;
    CountBodies:longint;
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
  FloorShape.Restitution:=0.3;
  FloorShape.Friction:=0.6;
  FloorBody.Finish;
  FloorBody.SetWorldTransformation(Matrix4x4Translate(0.0,0.0,0.0));
  FloorBody.CollisionGroups:=[0];

  CountBodies:=0;
  for BoxY:=0 to 3 do begin
   for BoxZ:=0 to 7 do begin
    for BoxX:=0 to 7 do begin
     BoxBody:=TKraftRigidBody.Create(Physics);
     BoxBody.SetRigidBodyType(krbtDynamic);
     ShapeBox:=TKraftShapeBox.Create(Physics,BoxBody,Vector3(0.25,0.25,0.25));
     ShapeBox.Restitution:=0.3;
     ShapeBox.Friction:=0.6;
     ShapeBox.Density:=100.0;
     BoxBody.Flags:=BoxBody.Flags-[krbfAllowSleep];
     BoxBody.Finish;
     BoxBody.SetWorldTransformation(Matrix4x4Translate((BoxX-3.5)*0.4999,0.25+(BoxY*0.51),(BoxZ-3.5)*0.4999));
     BoxBody.CollisionGroups:=[0];
     Bodies[CountBodies]:=BoxBody;
     inc(CountBodies);
    end;
   end;
  end;

  // Chain the top layer pairwise along x with ball socket joints, so the big island carries a joint
  // population across many graph colors alongside its contacts.
  for BoxZ:=0 to 7 do begin
   for BoxX:=0 to 6 do begin
    TKraftConstraintJointBallSocket.Create(Physics,
                                           Bodies[(3*64)+(BoxZ*8)+BoxX],
                                           Bodies[(3*64)+(BoxZ*8)+(BoxX+1)],
                                           Vector3(((BoxX-3.0)*0.4999),0.25+(3*0.51),(BoxZ-3.5)*0.4999),
                                           true);
   end;
  end;

  for StepIndex:=1 to StepCount do begin
   Physics.Step(1.0/120.0);
  end;

  for StepIndex:=0 to CountBodies-1 do begin
   BoxBody:=Bodies[StepIndex];
   Write(StepIndex,' ');
   DumpScalar(BoxBody.Sweep.c.x);
   DumpScalar(BoxBody.Sweep.c.y);
   DumpScalar(BoxBody.Sweep.c.z);
   DumpScalar(BoxBody.Sweep.q.x);
   DumpScalar(BoxBody.Sweep.q.y);
   DumpScalar(BoxBody.Sweep.q.z);
   DumpScalar(BoxBody.Sweep.q.w);
   DumpScalar(BoxBody.LinearVelocity.x);
   DumpScalar(BoxBody.LinearVelocity.y);
   DumpScalar(BoxBody.LinearVelocity.z);
   DumpScalar(BoxBody.AngularVelocity.x);
   DumpScalar(BoxBody.AngularVelocity.y);
   DumpScalar(BoxBody.AngularVelocity.z);
   WriteLn;
  end;

 finally
  Physics.Free;
 end;
end;

begin
 FormatSettings.DecimalSeparator:='.';
 RunDump;
end.
