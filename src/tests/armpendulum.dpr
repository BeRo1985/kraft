program armpendulum;

{$ifdef fpc}
 {$mode delphi}
{$endif}
{$apptype console}

uses {$ifdef unix}cthreads,{$endif} SysUtils,Math,kraft;

// A single forearm capsule on a hinge at a static anchor, released from horizontal. A uniform rod pivoted
// near its end released horizontally reaches roughly omega = sqrt(3*g/L) ~ 8.7 rad/s at the bottom, so any
// big shortfall means the solver or the damping is eating the motion.

procedure RunPendulum(const aName:string;const aSolverMode:TKraftSolverMode;const aJointMode:TKraftTGSJointMode;const ZeroDamping:boolean);
const StepCount=480;
var Physics:TKraft;
    AnchorBody,ArmBody:TKraftRigidBody;
    ShapeBox:TKraftShapeBox;
    Shape:TKraftShapeCapsule;
    StepIndex,PeakStep:longint;
    AngVel,Peak:TKraftScalar;
begin
 Physics:=TKraft.Create(-1);
 try
  Physics.SetFrequency(120.0);
  Physics.VelocityIterations:=8;
  Physics.PositionIterations:=3;
  Physics.Gravity.y:=-9.81;
  Physics.SolverMode:=aSolverMode;
  Physics.TGSJointMode:=aJointMode;

  AnchorBody:=TKraftRigidBody.Create(Physics);
  AnchorBody.SetRigidBodyType(krbtSTATIC);
  ShapeBox:=TKraftShapeBox.Create(Physics,AnchorBody,Vector3(0.05,0.05,0.05));
  AnchorBody.Finish;
  AnchorBody.SetWorldTransformation(Matrix4x4Translate(0.0,2.0,0.0));
  AnchorBody.CollisionGroups:=[0];

  ArmBody:=TKraftRigidBody.Create(Physics);
  ArmBody.SetRigidBodyType(krbtDYNAMIC);
  Shape:=TKraftShapeCapsule.Create(Physics,ArmBody,0.045,0.30);
  Shape.Restitution:=0.1;
  Shape.Density:=1.0;
  ArmBody.Finish;
  ArmBody.SetWorldTransformation(Matrix4x4TermMul(Matrix4x4RotateZ(0.5*pi),Matrix4x4Translate(0.20,2.0,0.0)));
  ArmBody.CollisionGroups:=[0];
  if ZeroDamping then begin
   ArmBody.LinearVelocityDamp:=0.0;
   ArmBody.AngularVelocityDamp:=0.0;
  end;

  TKraftConstraintJointHinge.Create(Physics,AnchorBody,ArmBody,Vector3(0.0,2.0,0.0),Vector3(0.0,0.0,1.0),false,false,0.0,0.0,0.0,0.0,false);

  Peak:=0.0;
  PeakStep:=0;
  for StepIndex:=1 to StepCount do begin
   Physics.Step(1.0/120.0);
   AngVel:=Vector3Length(ArmBody.AngularVelocity);
   if AngVel>Peak then begin
    Peak:=AngVel;
    PeakStep:=StepIndex;
   end;
  end;
  WriteLn(aName,': peak angvel ',Peak:6:2,' rad/s at t=',(PeakStep/120.0):0:3,'s  (analytic rod ~8.7)');
 finally
  Physics.Free;
 end;
end;

begin
 FormatSettings.DecimalSeparator:='.';
 RunPendulum('SI          default-damp',ksmSequentialImpulse,ktjmAdapter,false);
 RunPendulum('SI          zero-damp   ',ksmSequentialImpulse,ktjmAdapter,true);
 RunPendulum('TGS adapter default-damp',ksmTGSSoft,ktjmAdapter,false);
 RunPendulum('TGS adapter zero-damp   ',ksmTGSSoft,ktjmAdapter,true);
 RunPendulum('TGS native  default-damp',ksmTGSSoft,ktjmNativeSoft,false);
 RunPendulum('TGS native  zero-damp   ',ksmTGSSoft,ktjmNativeSoft,true);
end.
