program ragdollsettle;

{$ifdef fpc}
 {$mode delphi}
{$endif}
{$apptype console}

uses {$ifdef unix}cthreads,{$endif} SysUtils,Math,kraft;

const BaseY=0.9;
      BodyNames:array[0..10] of string=('Pelvis','Torso','Head','UpArmR','LoArmR','UpArmL','LoArmL','UpLegR','LoLegR','UpLegL','LoLegL');

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

var WithCone:boolean=true;
    WithTwist:boolean=true;
    WithHinge:boolean=true;
    LimitMargin:TKraftScalar=0.0;

procedure BuildScene(const Physics:TKraft);
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
 if WithCone or WithTwist then begin
  BallSocketJoint.SetSwingTwistLimits(WithCone,0.35,WithTwist,-0.25,0.25,Vector3(0.0,1.0,0.0));
 end;
 BallSocketJoint:=TKraftConstraintJointBallSocket.Create(Physics,RagdollBodies[1],RagdollBodies[2],Vector3(0.0,1.60+BaseY,0.0),false);
 if WithCone or WithTwist then begin
  BallSocketJoint.SetSwingTwistLimits(WithCone,0.6,WithTwist,-0.8,0.8,Vector3(0.0,1.0,0.0));
 end;
 BallSocketJoint:=TKraftConstraintJointBallSocket.Create(Physics,RagdollBodies[1],RagdollBodies[3],Vector3(0.21,1.48+BaseY,0.0),false);
 if WithCone or WithTwist then begin
  BallSocketJoint.SetSwingTwistLimits(WithCone,1.3,WithTwist,-0.5,0.5,Vector3(1.0,0.0,0.0));
 end;
 BallSocketJoint:=TKraftConstraintJointBallSocket.Create(Physics,RagdollBodies[1],RagdollBodies[5],Vector3(-0.21,1.48+BaseY,0.0),false);
 if WithCone or WithTwist then begin
  BallSocketJoint.SetSwingTwistLimits(WithCone,1.3,WithTwist,-0.5,0.5,Vector3(-1.0,0.0,0.0));
 end;
 BallSocketJoint:=TKraftConstraintJointBallSocket.Create(Physics,RagdollBodies[0],RagdollBodies[7],Vector3(0.10,0.90+BaseY,0.0),false);
 if WithCone or WithTwist then begin
  BallSocketJoint.SetSwingTwistLimits(WithCone,0.9,WithTwist,-0.3,0.3,Vector3(0.0,1.0,0.0));
 end;
 BallSocketJoint:=TKraftConstraintJointBallSocket.Create(Physics,RagdollBodies[0],RagdollBodies[9],Vector3(-0.10,0.90+BaseY,0.0),false);
 if WithCone or WithTwist then begin
  BallSocketJoint.SetSwingTwistLimits(WithCone,0.9,WithTwist,-0.3,0.3,Vector3(0.0,1.0,0.0));
 end;

 // Elbows and knees.
 HingeLimitsEnabled:=WithHinge;
 TKraftConstraintJointHinge.Create(Physics,RagdollBodies[3],RagdollBodies[4],Vector3(0.51,1.48+BaseY,0.0),Vector3(0.0,0.0,1.0),HingeLimitsEnabled,false,-LimitMargin,2.4,0.0,0.0,false);
 TKraftConstraintJointHinge.Create(Physics,RagdollBodies[5],RagdollBodies[6],Vector3(-0.51,1.48+BaseY,0.0),Vector3(0.0,0.0,1.0),HingeLimitsEnabled,false,-2.4,LimitMargin,0.0,0.0,false);
 TKraftConstraintJointHinge.Create(Physics,RagdollBodies[7],RagdollBodies[8],Vector3(0.10,0.48+BaseY,0.0),Vector3(1.0,0.0,0.0),HingeLimitsEnabled,false,-LimitMargin,2.4,0.0,0.0,false);
 TKraftConstraintJointHinge.Create(Physics,RagdollBodies[9],RagdollBodies[10],Vector3(-0.10,0.48+BaseY,0.0),Vector3(1.0,0.0,0.0),HingeLimitsEnabled,false,-LimitMargin,2.4,0.0,0.0,false);

 for Index:=0 to length(RagdollBodies)-1 do begin
  RagdollBodies[Index].LinearVelocity:=Vector3(1.5,0.5,0.0);
 end;

end;


procedure RunSettle(const aName:string;const aSolverMode:TKraftSolverMode;const aTGSJointMode:TKraftTGSJointMode;const aCone,aTwist,aHinge:boolean;const aMargin:TKraftScalar);
const StepCount=3600;
var Physics:TKraft;
    StepIndex,Index,TwitchSteps,WorstIndex,LastTwitchStep:longint;
    MaxAngVel,AngVel,LateMax:TKraftScalar;
    TwitchBodyCounts:array[0..10] of longint;
    BestBody:longint;
begin
 Physics:=TKraft.Create(-1);
 try
  Physics.SetFrequency(120.0);
  Physics.VelocityIterations:=8;
  Physics.PositionIterations:=3;
  Physics.Gravity.y:=-9.81;
  Physics.SolverMode:=aSolverMode;
  Physics.TGSJointMode:=aTGSJointMode;
  WithCone:=aCone;
  WithTwist:=aTwist;
  WithHinge:=aHinge;
  LimitMargin:=aMargin;
  BuildScene(Physics);
  TwitchSteps:=0;
  LastTwitchStep:=0;
  LateMax:=0.0;
  for Index:=0 to 10 do begin
   TwitchBodyCounts[Index]:=0;
  end;
  for StepIndex:=1 to StepCount do begin
   Physics.Step(1.0/120.0);
   if StepIndex>1200 then begin
    MaxAngVel:=0.0;
    WorstIndex:=0;
    for Index:=0 to 10 do begin
     AngVel:=Vector3Length(RagdollBodies[Index].AngularVelocity);
     if AngVel>MaxAngVel then begin
      MaxAngVel:=AngVel;
      WorstIndex:=Index;
     end;
    end;
    LateMax:=Max(LateMax,MaxAngVel);
    if MaxAngVel>0.1 then begin
     inc(TwitchSteps);
     inc(TwitchBodyCounts[WorstIndex]);
     LastTwitchStep:=StepIndex;
    end;
   end;
  end;
  BestBody:=0;
  for Index:=1 to 10 do begin
   if TwitchBodyCounts[Index]>TwitchBodyCounts[BestBody] then begin
    BestBody:=Index;
   end;
  end;
  WriteLn(aName,': twitchsteps(t>10s)=',TwitchSteps:5,' lastTwitch t=',(LastTwitchStep/120.0):6:2,'s lateMax=',LateMax:7:2,' mostly ',BodyNames[BestBody]);
 finally
  Physics.Free;
 end;
end;

begin
 FormatSettings.DecimalSeparator:='.';
 RunSettle('SI  full  m=0.00',ksmSequentialImpulse,ktjmAdapter,true,true,true,0.0);
 RunSettle('SI  full  m=0.10',ksmSequentialImpulse,ktjmAdapter,true,true,true,0.10);
 RunSettle('SI  hinge m=0.00',ksmSequentialImpulse,ktjmAdapter,false,false,true,0.0);
 RunSettle('TGA full  m=0.00',ksmTGSSoft,ktjmAdapter,true,true,true,0.0);
 RunSettle('TGA full  m=0.10',ksmTGSSoft,ktjmAdapter,true,true,true,0.10);
 RunSettle('NAT full  m=0.00',ksmTGSSoft,ktjmNativeSoft,true,true,true,0.0);
 RunSettle('NAT full  m=0.10',ksmTGSSoft,ktjmNativeSoft,true,true,true,0.10);
end.
