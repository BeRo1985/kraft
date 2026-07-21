program pilebench;

{$ifdef fpc}
 {$mode delphi}
{$endif}
{$apptype console}

uses {$ifdef unix}cthreads,{$endif} SysUtils,Math,kraft;

// Narrow phase cost probe for contact recycling: a settled pile of boxes with sleeping disabled, so the
// narrow phase keeps running over all resting pairs every step. Reports the summed narrow phase time and
// the recycle rate, once with recycling on (default) and once with it off.

procedure RunPile(const aName:string;const aRecycle:boolean);
const StepCount=1200;
      BoxCount=64;
var Physics:TKraft;
    FloorBody,BoxBody:TKraftRigidBody;
    FloorShape:TKraftShapePlane;
    ShapeBox:TKraftShapeBox;
    StepIndex,BoxIndex,LayerIndex,ColumnIndex:longint;
    RecycledSum,PairSum,NarrowTime:int64;
    Bodies:array[0..63] of TKraftRigidBody;
    MinY,MaxY,AvgY:TKraftScalar;
begin
 Physics:=TKraft.Create(1); // Single threaded for stable narrow phase timing comparisons
 try
  Physics.SetFrequency(120.0);
  Physics.VelocityIterations:=8;
  Physics.PositionIterations:=3;
  Physics.SpeculativeIterations:=8;
  Physics.TimeOfImpactIterations:=20;
  Physics.Gravity.y:=-9.81;
  Physics.SolverMode:=ksmTGSSoft;
  Physics.TGSJointMode:=ktjmNativeSoft;
  if not aRecycle then begin
   Physics.ContactRecycleDistance:=0.0;
  end;

  FloorBody:=TKraftRigidBody.Create(Physics);
  FloorBody.SetRigidBodyType(krbtSTATIC);
  FloorShape:=TKraftShapePlane.Create(Physics,FloorBody,Plane(Vector3Norm(Vector3(0.0,1.0,0.0)),0.0));
  FloorShape.Restitution:=0.1;
  FloorBody.Finish;
  FloorBody.SetWorldTransformation(Matrix4x4Translate(0.0,0.0,0.0));
  FloorBody.CollisionGroups:=[0];

  // An 8x8 grid of loose stacks of two boxes each, spaced so the fat AABBs of the neighbors overlap and
  // the pile forms one big awake island with many resting pairs.
  BoxIndex:=0;
  for LayerIndex:=0 to 1 do begin
   for ColumnIndex:=0 to (BoxCount div 2)-1 do begin
    BoxBody:=TKraftRigidBody.Create(Physics);
    BoxBody.SetRigidBodyType(krbtDynamic);
    ShapeBox:=TKraftShapeBox.Create(Physics,BoxBody,Vector3(0.25,0.25,0.25));
    ShapeBox.Restitution:=0.1;
    ShapeBox.Density:=100.0;
    BoxBody.Flags:=BoxBody.Flags-[krbfAllowSleep];
    BoxBody.Finish;
    BoxBody.SetWorldTransformation(Matrix4x4Translate(((ColumnIndex mod 8)-3.5)*0.52,
                                                      0.25+(LayerIndex*0.51),
                                                      ((ColumnIndex div 8)-3.5)*0.52));
    BoxBody.CollisionGroups:=[0];
    Bodies[BoxIndex]:=BoxBody;
    inc(BoxIndex);
   end;
  end;

  RecycledSum:=0;
  PairSum:=0;
  for StepIndex:=1 to StepCount do begin
   Physics.Step(1.0/120.0);
   inc(RecycledSum,Physics.ContactManager.CountRecycledContactPairs);
   inc(PairSum,Physics.ContactManager.CountContactPairs);
  end;
  NarrowTime:=Physics.NarrowPhaseTime;

  MinY:=1e30;
  MaxY:=-1e30;
  AvgY:=0.0;
  for BoxIndex:=0 to BoxCount-1 do begin
   MinY:=Min(MinY,Bodies[BoxIndex].Sweep.c.y);
   MaxY:=Max(MaxY,Bodies[BoxIndex].Sweep.c.y);
   AvgY:=AvgY+Bodies[BoxIndex].Sweep.c.y;
  end;
  AvgY:=AvgY/BoxCount;

  WriteLn(aName,': narrowPhaseTime=',NarrowTime,' recycled=',RecycledSum,'/',PairSum,' pair-steps (',(RecycledSum*100.0)/Max(1,PairSum):5:1,'%) boxY min/avg/max=',MinY:6:3,'/',AvgY:6:3,'/',MaxY:6:3);
 finally
  Physics.Free;
 end;
end;

begin
 FormatSettings.DecimalSeparator:='.';
 RunPile('RECYCLE-ON ',true);
 RunPile('RECYCLE-OFF',false);
end.
