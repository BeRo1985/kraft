program armrepro;

{$ifdef fpc}
 {$mode delphi}
{$endif}
{$apptype console}

uses {$ifdef unix}cthreads,{$endif} SysUtils,Math,kraft;

var UpperArm,LowerArm:TKraftRigidBody;

procedure BuildArm(const Physics:TKraft;const WithConeTwist,WithElbowLimits,MirroredRight:boolean);
var TorsoBody:TKraftRigidBody;
    ShapeBox:TKraftShapeBox;
    Shape:TKraftShapeCapsule;
    BallSocketJoint:TKraftConstraintJointBallSocket;
    Side:TKraftScalar;
begin
 if MirroredRight then begin
  Side:=1.0;
 end else begin
  Side:=-1.0;
 end;

 TorsoBody:=TKraftRigidBody.Create(Physics);
 TorsoBody.SetRigidBodyType(krbtSTATIC);
 ShapeBox:=TKraftShapeBox.Create(Physics,TorsoBody,Vector3(0.20,0.22,0.12));
 TorsoBody.Finish;
 TorsoBody.SetWorldTransformation(Matrix4x4Translate(0.0,1.48,0.0));
 TorsoBody.CollisionGroups:=[0];

 UpperArm:=TKraftRigidBody.Create(Physics);
 UpperArm.SetRigidBodyType(krbtDYNAMIC);
 Shape:=TKraftShapeCapsule.Create(Physics,UpperArm,0.05,0.30);
 Shape.Restitution:=0.1;
 Shape.Density:=1.0;
 UpperArm.Finish;
 UpperArm.SetWorldTransformation(Matrix4x4TermMul(Matrix4x4RotateZ(0.5*pi),Matrix4x4Translate(Side*0.36,1.48,0.0)));
 UpperArm.CollisionGroups:=[0];

 LowerArm:=TKraftRigidBody.Create(Physics);
 LowerArm.SetRigidBodyType(krbtDYNAMIC);
 Shape:=TKraftShapeCapsule.Create(Physics,LowerArm,0.045,0.30);
 Shape.Restitution:=0.1;
 Shape.Density:=1.0;
 LowerArm.Finish;
 LowerArm.SetWorldTransformation(Matrix4x4TermMul(Matrix4x4RotateZ(0.5*pi),Matrix4x4Translate(Side*0.66,1.48,0.0)));
 LowerArm.CollisionGroups:=[0];

 BallSocketJoint:=TKraftConstraintJointBallSocket.Create(Physics,TorsoBody,UpperArm,Vector3(Side*0.21,1.48,0.0),false);
 if WithConeTwist then begin
  BallSocketJoint.SetSwingTwistLimits(true,1.3,true,-0.5,0.5,Vector3(Side*1.0,0.0,0.0));
 end;

 if MirroredRight then begin
  TKraftConstraintJointHinge.Create(Physics,UpperArm,LowerArm,Vector3(0.51,1.48,0.0),Vector3(0.0,0.0,1.0),WithElbowLimits,false,0.0,2.4,0.0,0.0,false);
 end else begin
  TKraftConstraintJointHinge.Create(Physics,UpperArm,LowerArm,Vector3(-0.51,1.48,0.0),Vector3(0.0,0.0,1.0),WithElbowLimits,false,-2.4,0.0,0.0,0.0,false);
 end;

end;

procedure RunArm(const aName:string;const aJointMode:TKraftTGSJointMode;const WithConeTwist,WithElbowLimits,MirroredRight:boolean);
const StepCount=1200;
var Physics:TKraft;
    StepIndex:longint;
    UpperAng,LowerAng,Peak:TKraftScalar;
    FirstBad:longint;
begin
 Physics:=TKraft.Create(-1);
 try
  Physics.SetFrequency(120.0);
  Physics.VelocityIterations:=8;
  Physics.PositionIterations:=3;
  Physics.Gravity.y:=-9.81;
  Physics.SolverMode:=ksmTGSSoft;
  Physics.TGSJointMode:=aJointMode;
  BuildArm(Physics,WithConeTwist,WithElbowLimits,MirroredRight);
  Peak:=0.0;
  FirstBad:=-1;
  for StepIndex:=1 to StepCount do begin
   Physics.Step(1.0/120.0);
   UpperAng:=Vector3Length(UpperArm.AngularVelocity);
   LowerAng:=Vector3Length(LowerArm.AngularVelocity);
   if Max(UpperAng,LowerAng)>Peak then begin
    Peak:=Max(UpperAng,LowerAng);
   end;
   if (Max(UpperAng,LowerAng)>20.0) and (FirstBad<0) then begin
    FirstBad:=StepIndex;
   end;
  end;
  Write(aName,': peak angvel ',Peak:0:2);
  if FirstBad>=0 then begin
   WriteLn('  UNSTABLE (first >20 at t=',(FirstBad/120.0):0:3,'s)');
  end else begin
   WriteLn('  stable');
  end;
 finally
  Physics.Free;
 end;
end;

begin
 FormatSettings.DecimalSeparator:='.';
 RunArm('native left  full           ',ktjmNativeSoft,true,true,false);
 RunArm('native left  no-conetwist   ',ktjmNativeSoft,false,true,false);
 RunArm('native left  no-elbowlimits ',ktjmNativeSoft,true,false,false);
 RunArm('native left  bare           ',ktjmNativeSoft,false,false,false);
 RunArm('native right full           ',ktjmNativeSoft,true,true,true);
 RunArm('native right no-conetwist   ',ktjmNativeSoft,false,true,true);
 RunArm('adapter left full           ',ktjmAdapter,true,true,false);
 RunArm('adapter right full          ',ktjmAdapter,true,true,true);
end.
