program rolltest;

{$ifdef fpc}
 {$mode delphi}
{$endif}
{$apptype console}

uses {$ifdef unix}cthreads,{$endif} SysUtils,Math,kraft;

// A fast rolling sphere in sustained contact: penetration depth and rolling behaviour probe the substep
// separation recompute under continuous rotation.

procedure RunRoll(const aName:string;const aSolverMode:TKraftSolverMode);
const StepCount=480;
      Radius=0.25;
var Physics:TKraft;
    FloorBody,BallBody:TKraftRigidBody;
    FloorShape:TKraftShapePlane;
    BallShape:TKraftShapeSphere;
    StepIndex:longint;
    Penetration,MaxPenetration,SumPenetration:TKraftScalar;
    SampleCount:longint;
begin
 Physics:=TKraft.Create(-1);
 try
  Physics.SetFrequency(120.0);
  Physics.VelocityIterations:=8;
  Physics.PositionIterations:=3;
  Physics.SpeculativeIterations:=8;
  Physics.TimeOfImpactIterations:=20;
  Physics.Gravity.y:=-9.81;
  Physics.SolverMode:=aSolverMode;

  FloorBody:=TKraftRigidBody.Create(Physics);
  FloorBody.SetRigidBodyType(krbtSTATIC);
  FloorShape:=TKraftShapePlane.Create(Physics,FloorBody,Plane(Vector3Norm(Vector3(0.0,1.0,0.0)),0.0));
  FloorShape.Restitution:=0.0;
  FloorBody.Finish;
  FloorBody.SetWorldTransformation(Matrix4x4Translate(0.0,0.0,0.0));
  FloorBody.CollisionGroups:=[0];

  BallBody:=TKraftRigidBody.Create(Physics);
  BallBody.SetRigidBodyType(krbtDYNAMIC);
  BallShape:=TKraftShapeSphere.Create(Physics,BallBody,Radius);
  BallShape.Restitution:=0.0;
  BallShape.Density:=5.0;
  BallBody.Finish;
  BallBody.SetWorldTransformation(Matrix4x4Translate(0.0,Radius,0.0));
  BallBody.CollisionGroups:=[0];
  BallBody.LinearVelocityDamp:=0.0;
  BallBody.AngularVelocityDamp:=0.0;

  // Consistent fast rolling: v = omega x r with omega about -z gives +x travel.
  BallBody.AngularVelocity:=Vector3(0.0,0.0,-20.0);
  BallBody.LinearVelocity:=Vector3(20.0*Radius,0.0,0.0);

  MaxPenetration:=0.0;
  SumPenetration:=0.0;
  SampleCount:=0;
  for StepIndex:=1 to StepCount do begin
   Physics.Step(1.0/120.0);
   if StepIndex>60 then begin
    Penetration:=Radius-BallBody.Sweep.c.y;
    if Penetration>MaxPenetration then begin
     MaxPenetration:=Penetration;
    end;
    SumPenetration:=SumPenetration+Max(Penetration,0.0);
    inc(SampleCount);
   end;
  end;
  WriteLn(aName,': maxPen=',MaxPenetration*1000.0:7:3,' mm  avgPen=',(SumPenetration/SampleCount)*1000.0:7:3,' mm  endSpin=',Vector3Length(BallBody.AngularVelocity):7:3,' endVelX=',BallBody.LinearVelocity.x:7:3);
 finally
  Physics.Free;
 end;
end;

begin
 FormatSettings.DecimalSeparator:='.';
 RunRoll('TGS',ksmTGSSoft);
 RunRoll('SI ',ksmSequentialImpulse);
end.
