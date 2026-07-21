program ragdollrepro;

{$ifdef fpc}
 {$mode delphi}
{$endif}
{$apptype console}

uses {$ifdef unix}cthreads,{$endif} SysUtils,Math,kraft;

const BaseY=0.9;
      BodyNames:array[0..10] of string=('Pelvis','Torso','Head','UpArmR','LoArmR','UpArmL','LoArmL','UpLegR','LoLegR','UpLegL','LoLegL');

type TScenarioLimits=(slFull,slNoConeTwist,slNoHingeLimits,slNoLimits);

var RagdollBodies:array[0..10] of TKraftRigidBody;

function CreateBoxBody(const Physics:TKraft;const AExtents:TKraftVector3;const ATransform:TKraftMatrix4x4):TKraftRigidBody;
var Shape:TKraftShapeBox;
begin
 result:=TKraftRigidBody.Create(Physics);
 result.SetRigidBodyType(krbtDYNAMIC);
 Shape:=TKraftShapeBox.Create(Physics,result,AExtents);
 Shape.Restitution:=0.1;
 Shape.Density:=1.0;
 result.Finish;
 result.SetWorldTransformation(ATransform);
 result.CollisionGroups:=[0];
end;

function CreateSphereBody(const Physics:TKraft;const ARadius:TKraftScalar;const ATransform:TKraftMatrix4x4):TKraftRigidBody;
var Shape:TKraftShapeSphere;
begin
 result:=TKraftRigidBody.Create(Physics);
 result.SetRigidBodyType(krbtDYNAMIC);
 Shape:=TKraftShapeSphere.Create(Physics,result,ARadius);
 Shape.Restitution:=0.1;
 Shape.Density:=1.0;
 result.Finish;
 result.SetWorldTransformation(ATransform);
 result.CollisionGroups:=[0];
end;

function CreateCapsuleBody(const Physics:TKraft;const ARadius,AHeight:TKraftScalar;const ATransform:TKraftMatrix4x4):TKraftRigidBody;
var Shape:TKraftShapeCapsule;
begin
 result:=TKraftRigidBody.Create(Physics);
 result.SetRigidBodyType(krbtDYNAMIC);
 Shape:=TKraftShapeCapsule.Create(Physics,result,ARadius,AHeight);
 Shape.Restitution:=0.1;
 Shape.Density:=1.0;
 result.Finish;
 result.SetWorldTransformation(ATransform);
 result.CollisionGroups:=[0];
end;

procedure BuildScene(const Physics:TKraft;const Limits:TScenarioLimits);
var Index:longint;
    FloorBody,RampBody:TKraftRigidBody;
    FloorShape:TKraftShapePlane;
    ShapeBox:TKraftShapeBox;
    BallSocketJoint:TKraftConstraintJointBallSocket;
    HingeLimitsEnabled:boolean;
begin

 FloorBody:=TKraftRigidBody.Create(Physics);
 FloorBody.SetRigidBodyType(krbtSTATIC);
 FloorShape:=TKraftShapePlane.Create(Physics,FloorBody,Plane(Vector3Norm(Vector3(0.0,1.0,0.0)),0.0));
 FloorShape.Restitution:=0.3;
 FloorBody.Finish;
 FloorBody.SetWorldTransformation(Matrix4x4Translate(0.0,0.0,0.0));
 FloorBody.CollisionGroups:=[0];

 RampBody:=TKraftRigidBody.Create(Physics);
 RampBody.SetRigidBodyType(krbtSTATIC);
 ShapeBox:=TKraftShapeBox.Create(Physics,RampBody,Vector3(1.5,0.1,1.5));
 ShapeBox.Restitution:=0.1;
 RampBody.Finish;
 RampBody.SetWorldTransformation(Matrix4x4TermMul(Matrix4x4RotateZ(-0.35),Matrix4x4Translate(3.0,0.55,0.0)));
 RampBody.CollisionGroups:=[0];

 RagdollBodies[0]:=CreateBoxBody(Physics,Vector3(0.18,0.12,0.12),Matrix4x4Translate(0.0,1.00+BaseY,0.0));
 RagdollBodies[1]:=CreateBoxBody(Physics,Vector3(0.20,0.22,0.12),Matrix4x4Translate(0.0,1.35+BaseY,0.0));
 RagdollBodies[2]:=CreateSphereBody(Physics,0.12,Matrix4x4Translate(0.0,1.72+BaseY,0.0));
 RagdollBodies[3]:=CreateCapsuleBody(Physics,0.05,0.30,Matrix4x4TermMul(Matrix4x4RotateZ(0.5*pi),Matrix4x4Translate(0.36,1.48+BaseY,0.0)));
 RagdollBodies[4]:=CreateCapsuleBody(Physics,0.045,0.30,Matrix4x4TermMul(Matrix4x4RotateZ(0.5*pi),Matrix4x4Translate(0.66,1.48+BaseY,0.0)));
 RagdollBodies[5]:=CreateCapsuleBody(Physics,0.05,0.30,Matrix4x4TermMul(Matrix4x4RotateZ(0.5*pi),Matrix4x4Translate(-0.36,1.48+BaseY,0.0)));
 RagdollBodies[6]:=CreateCapsuleBody(Physics,0.045,0.30,Matrix4x4TermMul(Matrix4x4RotateZ(0.5*pi),Matrix4x4Translate(-0.66,1.48+BaseY,0.0)));
 RagdollBodies[7]:=CreateCapsuleBody(Physics,0.07,0.40,Matrix4x4Translate(0.10,0.70+BaseY,0.0));
 RagdollBodies[8]:=CreateCapsuleBody(Physics,0.06,0.40,Matrix4x4Translate(0.10,0.28+BaseY,0.0));
 RagdollBodies[9]:=CreateCapsuleBody(Physics,0.07,0.40,Matrix4x4Translate(-0.10,0.70+BaseY,0.0));
 RagdollBodies[10]:=CreateCapsuleBody(Physics,0.06,0.40,Matrix4x4Translate(-0.10,0.28+BaseY,0.0));

 // Spine, neck, shoulders, hips.
 BallSocketJoint:=TKraftConstraintJointBallSocket.Create(Physics,RagdollBodies[0],RagdollBodies[1],Vector3(0.0,1.15+BaseY,0.0),false);
 if Limits in [slFull,slNoHingeLimits] then begin
  BallSocketJoint.SetSwingTwistLimits(true,0.35,true,-0.25,0.25,Vector3(0.0,1.0,0.0));
 end;
 BallSocketJoint:=TKraftConstraintJointBallSocket.Create(Physics,RagdollBodies[1],RagdollBodies[2],Vector3(0.0,1.60+BaseY,0.0),false);
 if Limits in [slFull,slNoHingeLimits] then begin
  BallSocketJoint.SetSwingTwistLimits(true,0.6,true,-0.8,0.8,Vector3(0.0,1.0,0.0));
 end;
 BallSocketJoint:=TKraftConstraintJointBallSocket.Create(Physics,RagdollBodies[1],RagdollBodies[3],Vector3(0.21,1.48+BaseY,0.0),false);
 if Limits in [slFull,slNoHingeLimits] then begin
  BallSocketJoint.SetSwingTwistLimits(true,1.3,true,-0.5,0.5,Vector3(1.0,0.0,0.0));
 end;
 BallSocketJoint:=TKraftConstraintJointBallSocket.Create(Physics,RagdollBodies[1],RagdollBodies[5],Vector3(-0.21,1.48+BaseY,0.0),false);
 if Limits in [slFull,slNoHingeLimits] then begin
  BallSocketJoint.SetSwingTwistLimits(true,1.3,true,-0.5,0.5,Vector3(-1.0,0.0,0.0));
 end;
 BallSocketJoint:=TKraftConstraintJointBallSocket.Create(Physics,RagdollBodies[0],RagdollBodies[7],Vector3(0.10,0.90+BaseY,0.0),false);
 if Limits in [slFull,slNoHingeLimits] then begin
  BallSocketJoint.SetSwingTwistLimits(true,0.9,true,-0.3,0.3,Vector3(0.0,1.0,0.0));
 end;
 BallSocketJoint:=TKraftConstraintJointBallSocket.Create(Physics,RagdollBodies[0],RagdollBodies[9],Vector3(-0.10,0.90+BaseY,0.0),false);
 if Limits in [slFull,slNoHingeLimits] then begin
  BallSocketJoint.SetSwingTwistLimits(true,0.9,true,-0.3,0.3,Vector3(0.0,1.0,0.0));
 end;

 // Elbows and knees.
 HingeLimitsEnabled:=Limits in [slFull,slNoConeTwist];
 TKraftConstraintJointHinge.Create(Physics,RagdollBodies[3],RagdollBodies[4],Vector3(0.51,1.48+BaseY,0.0),Vector3(0.0,0.0,1.0),HingeLimitsEnabled,false,-0.1,2.4,0.0,0.0,false);
 TKraftConstraintJointHinge.Create(Physics,RagdollBodies[5],RagdollBodies[6],Vector3(-0.51,1.48+BaseY,0.0),Vector3(0.0,0.0,1.0),HingeLimitsEnabled,false,-2.4,0.1,0.0,0.0,false);
 TKraftConstraintJointHinge.Create(Physics,RagdollBodies[7],RagdollBodies[8],Vector3(0.10,0.48+BaseY,0.0),Vector3(1.0,0.0,0.0),HingeLimitsEnabled,false,-0.1,2.4,0.0,0.0,false);
 TKraftConstraintJointHinge.Create(Physics,RagdollBodies[9],RagdollBodies[10],Vector3(-0.10,0.48+BaseY,0.0),Vector3(1.0,0.0,0.0),HingeLimitsEnabled,false,-0.1,2.4,0.0,0.0,false);

 for Index:=0 to length(RagdollBodies)-1 do begin
  RagdollBodies[Index].LinearVelocity:=Vector3(1.5,0.5,0.0);
 end;

end;

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
