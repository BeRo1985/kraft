unit UnitDemoSceneRagdoll;

{$MODE Delphi}

interface

uses Kraft,UnitDemoScene;

type { TDemoSceneRagdoll }

     TDemoSceneRagdoll=class(TDemoScene)
      public
       RigidBodyFloor:TKraftRigidBody;
       ShapeFloorPlane:TKraftShapePlane;
       OldSolverMode:TKraftSolverMode;
       OldTGSJointMode:TKraftTGSJointMode;
       constructor Create(const AKraftPhysics:TKraft); override;
       destructor Destroy; override;
       procedure Step(const DeltaTime:double); override;
     end;

implementation

uses UnitFormMain;

// Reference ragdoll built entirely from the existing joints: ball sockets with swing/twist limits for the
// spine, neck, shoulders and hips, and hinges with angle limits for the elbows and knees. It spawns in a
// T-pose slightly above the floor with a sideways toss towards a static ramp, so it tumbles and crumples.
constructor TDemoSceneRagdoll.Create(const AKraftPhysics:TKraft);
const BaseY=0.9;
      TossVelocityX=1.5;
      TossVelocityY=0.5;
var Index:longint;
    RagdollBodies:array[0..10] of TKraftRigidBody;
    PelvisBody:TKraftRigidBody;
    TorsoBody:TKraftRigidBody;
    HeadBody:TKraftRigidBody;
    UpperArmRightBody:TKraftRigidBody;
    LowerArmRightBody:TKraftRigidBody;
    UpperArmLeftBody:TKraftRigidBody;
    LowerArmLeftBody:TKraftRigidBody;
    UpperLegRightBody:TKraftRigidBody;
    LowerLegRightBody:TKraftRigidBody;
    UpperLegLeftBody:TKraftRigidBody;
    LowerLegLeftBody:TKraftRigidBody;
    RampBody:TKraftRigidBody;
    ShapeBox:TKraftShapeBox;
    BallSocketJoint:TKraftConstraintJointBallSocket;
 function CreateBoxBody(const AExtents:TKraftVector3;const ATransform:TKraftMatrix4x4):TKraftRigidBody;
 var Shape:TKraftShapeBox;
 begin
  result:=TKraftRigidBody.Create(KraftPhysics);
  result.SetRigidBodyType(krbtDYNAMIC);
  Shape:=TKraftShapeBox.Create(KraftPhysics,result,AExtents);
  Shape.Restitution:=0.1;
  Shape.Density:=1.0;
  result.Finish;
  result.SetWorldTransformation(ATransform);
  result.CollisionGroups:=[0];
 end;
 function CreateSphereBody(const ARadius:TKraftScalar;const ATransform:TKraftMatrix4x4):TKraftRigidBody;
 var Shape:TKraftShapeSphere;
 begin
  result:=TKraftRigidBody.Create(KraftPhysics);
  result.SetRigidBodyType(krbtDYNAMIC);
  Shape:=TKraftShapeSphere.Create(KraftPhysics,result,ARadius);
  Shape.Restitution:=0.1;
  Shape.Density:=1.0;
  result.Finish;
  result.SetWorldTransformation(ATransform);
  result.CollisionGroups:=[0];
 end;
 function CreateCapsuleBody(const ARadius,AHeight:TKraftScalar;const ATransform:TKraftMatrix4x4):TKraftRigidBody;
 var Shape:TKraftShapeCapsule;
 begin
  result:=TKraftRigidBody.Create(KraftPhysics);
  result.SetRigidBodyType(krbtDYNAMIC);
  Shape:=TKraftShapeCapsule.Create(KraftPhysics,result,ARadius,AHeight);
  Shape.Restitution:=0.1;
  Shape.Density:=1.0;
  result.Finish;
  result.SetWorldTransformation(ATransform);
  result.CollisionGroups:=[0];
 end;
begin

 inherited Create(AKraftPhysics);

 OldSolverMode:=KraftPhysics.SolverMode;
 OldTGSJointMode:=KraftPhysics.TGSJointMode;

 KraftPhysics.SolverMode:=ksmTGSSoft;
 KraftPhysics.TGSJointMode:=ktjmNativeSoft;

 RigidBodyFloor:=TKraftRigidBody.Create(KraftPhysics);
 RigidBodyFloor.SetRigidBodyType(krbtSTATIC);
 ShapeFloorPlane:=TKraftShapePlane.Create(KraftPhysics,RigidBodyFloor,Plane(Vector3Norm(Vector3(0.0,1.0,0.0)),0.0));
 ShapeFloorPlane.Restitution:=0.3;
 RigidBodyFloor.Finish;
 RigidBodyFloor.SetWorldTransformation(Matrix4x4Translate(0.0,0.0,0.0));
 RigidBodyFloor.CollisionGroups:=[0];

 // Static ramp for the ragdoll to tumble over.
 RampBody:=TKraftRigidBody.Create(KraftPhysics);
 RampBody.SetRigidBodyType(krbtSTATIC);
 ShapeBox:=TKraftShapeBox.Create(KraftPhysics,RampBody,Vector3(1.5,0.1,1.5));
 ShapeBox.Restitution:=0.1;
 RampBody.Finish;
 RampBody.SetWorldTransformation(Matrix4x4TermMul(Matrix4x4RotateZ(-0.35),Matrix4x4Translate(3.0,0.55,0.0)));
 RampBody.CollisionGroups:=[0];

 // Body parts, all placed in the T-pose which the joints snapshot as their neutral pose. The capsule axis
 // points along local y, so the arms are rotated onto the x axis.
 PelvisBody:=CreateBoxBody(Vector3(0.18,0.12,0.12),Matrix4x4Translate(0.0,1.00+BaseY,0.0));
 TorsoBody:=CreateBoxBody(Vector3(0.20,0.22,0.12),Matrix4x4Translate(0.0,1.35+BaseY,0.0));
 HeadBody:=CreateSphereBody(0.12,Matrix4x4Translate(0.0,1.72+BaseY,0.0));
 UpperArmRightBody:=CreateCapsuleBody(0.05,0.30,Matrix4x4TermMul(Matrix4x4RotateZ(0.5*pi),Matrix4x4Translate(0.36,1.48+BaseY,0.0)));
 LowerArmRightBody:=CreateCapsuleBody(0.045,0.30,Matrix4x4TermMul(Matrix4x4RotateZ(0.5*pi),Matrix4x4Translate(0.66,1.48+BaseY,0.0)));
 UpperArmLeftBody:=CreateCapsuleBody(0.05,0.30,Matrix4x4TermMul(Matrix4x4RotateZ(0.5*pi),Matrix4x4Translate(-0.36,1.48+BaseY,0.0)));
 LowerArmLeftBody:=CreateCapsuleBody(0.045,0.30,Matrix4x4TermMul(Matrix4x4RotateZ(0.5*pi),Matrix4x4Translate(-0.66,1.48+BaseY,0.0)));
 UpperLegRightBody:=CreateCapsuleBody(0.07,0.40,Matrix4x4Translate(0.10,0.70+BaseY,0.0));
 LowerLegRightBody:=CreateCapsuleBody(0.06,0.40,Matrix4x4Translate(0.10,0.28+BaseY,0.0));
 UpperLegLeftBody:=CreateCapsuleBody(0.07,0.40,Matrix4x4Translate(-0.10,0.70+BaseY,0.0));
 LowerLegLeftBody:=CreateCapsuleBody(0.06,0.40,Matrix4x4Translate(-0.10,0.28+BaseY,0.0));

 // Spine: pelvis to torso, tight cone with a little twist.
 BallSocketJoint:=TKraftConstraintJointBallSocket.Create(KraftPhysics,PelvisBody,TorsoBody,Vector3(0.0,1.15+BaseY,0.0),false);
 BallSocketJoint.SetSwingTwistLimits(true,0.35,true,-0.25,0.25,Vector3(0.0,1.0,0.0));

 // Neck: torso to head, wider cone and more twist for looking around.
 BallSocketJoint:=TKraftConstraintJointBallSocket.Create(KraftPhysics,TorsoBody,HeadBody,Vector3(0.0,1.60+BaseY,0.0),false);
 BallSocketJoint.SetSwingTwistLimits(true,0.6,true,-0.8,0.8,Vector3(0.0,1.0,0.0));

 // Shoulders: wide swing cone, moderate twist, twist axis along the arm.
 BallSocketJoint:=TKraftConstraintJointBallSocket.Create(KraftPhysics,TorsoBody,UpperArmRightBody,Vector3(0.21,1.48+BaseY,0.0),false);
 BallSocketJoint.SetSwingTwistLimits(true,1.3,true,-0.5,0.5,Vector3(1.0,0.0,0.0));
 BallSocketJoint:=TKraftConstraintJointBallSocket.Create(KraftPhysics,TorsoBody,UpperArmLeftBody,Vector3(-0.21,1.48+BaseY,0.0),false);
 BallSocketJoint.SetSwingTwistLimits(true,1.3,true,-0.5,0.5,Vector3(-1.0,0.0,0.0));

 // Elbows: hinges which only curl the forearm towards the head. The straight pose gets a small margin, so
 // the joints do not rest permanently ON their limit in the spawn pose.
 TKraftConstraintJointHinge.Create(KraftPhysics,UpperArmRightBody,LowerArmRightBody,Vector3(0.51,1.48+BaseY,0.0),Vector3(0.0,0.0,1.0),true,false,-0.1,2.4,0.0,0.0,false);
 TKraftConstraintJointHinge.Create(KraftPhysics,UpperArmLeftBody,LowerArmLeftBody,Vector3(-0.51,1.48+BaseY,0.0),Vector3(0.0,0.0,1.0),true,false,-2.4,0.1,0.0,0.0,false);

 // Hips: medium swing cone, little twist, twist axis along the leg.
 BallSocketJoint:=TKraftConstraintJointBallSocket.Create(KraftPhysics,PelvisBody,UpperLegRightBody,Vector3(0.10,0.90+BaseY,0.0),false);
 BallSocketJoint.SetSwingTwistLimits(true,0.9,true,-0.3,0.3,Vector3(0.0,1.0,0.0));
 BallSocketJoint:=TKraftConstraintJointBallSocket.Create(KraftPhysics,PelvisBody,UpperLegLeftBody,Vector3(-0.10,0.90+BaseY,0.0),false);
 BallSocketJoint.SetSwingTwistLimits(true,0.9,true,-0.3,0.3,Vector3(0.0,1.0,0.0));

 // Knees: hinges which only bend the lower leg backwards, with the same small straight-pose margin.
 TKraftConstraintJointHinge.Create(KraftPhysics,UpperLegRightBody,LowerLegRightBody,Vector3(0.10,0.48+BaseY,0.0),Vector3(1.0,0.0,0.0),true,false,-0.1,2.4,0.0,0.0,false);
 TKraftConstraintJointHinge.Create(KraftPhysics,UpperLegLeftBody,LowerLegLeftBody,Vector3(-0.10,0.48+BaseY,0.0),Vector3(1.0,0.0,0.0),true,false,-0.1,2.4,0.0,0.0,false);

 // Toss the whole ragdoll towards the ramp with one shared velocity, so the joints start out relaxed.
 RagdollBodies[0]:=PelvisBody;
 RagdollBodies[1]:=TorsoBody;
 RagdollBodies[2]:=HeadBody;
 RagdollBodies[3]:=UpperArmRightBody;
 RagdollBodies[4]:=LowerArmRightBody;
 RagdollBodies[5]:=UpperArmLeftBody;
 RagdollBodies[6]:=LowerArmLeftBody;
 RagdollBodies[7]:=UpperLegRightBody;
 RagdollBodies[8]:=LowerLegRightBody;
 RagdollBodies[9]:=UpperLegLeftBody;
 RagdollBodies[10]:=LowerLegLeftBody;
 for Index:=0 to length(RagdollBodies)-1 do begin
  // Biomechanically plausible damping: without joint friction the limbs are nearly undamped pendulums and
  // keep swinging for half a minute, which reads as endless twitching instead of a body coming to rest.
  RagdollBodies[Index].AngularVelocityDamp:=2.0;
  RagdollBodies[Index].LinearVelocityDamp:=0.2;
  RagdollBodies[Index].LinearVelocity:=Vector3(TossVelocityX,TossVelocityY,0.0);
 end;

end;

destructor TDemoSceneRagdoll.Destroy;
begin
 KraftPhysics.SolverMode:=OldSolverMode;
 KraftPhysics.TGSJointMode:=OldTGSJointMode;
 inherited Destroy;
end;

procedure TDemoSceneRagdoll.Step(const DeltaTime:double);
begin
end;

initialization
 RegisterDemoScene('Ragdoll',TDemoSceneRagdoll);
end.
