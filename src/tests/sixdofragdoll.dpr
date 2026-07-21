program sixdofragdoll;

{$ifdef fpc}
 {$mode delphi}
{$endif}
{$apptype console}

uses {$ifdef unix}cthreads,{$endif} SysUtils,Math,kraft;

// A/B proof for the 6-DOF joint: the reference ragdoll (ball sockets with swing/twist limits + hinges) versus
// the same ragdoll built ONLY from TKraftConstraintJoint6DOF with identical anchors, limits and damping, run
// through the 100-variant perturbation sweep with the engine-sleep metric. Select via parameter:
//   smoke | refsi | refnat | sixsi | sixnat | sixadpsub   (no parameter = everything)

const BaseY=0.9;

var RagdollBodies:array[0..10] of TKraftRigidBody;
    BodyDensity:TKraftScalar=1.0;
    RagdollAngularDamp:TKraftScalar=2.0;
    LimitMargin:TKraftScalar=0.10;
    TossJitterX:TKraftScalar=0.0;
    TossJitterZ:TKraftScalar=0.0;
    Use6DOF:boolean=false;
    NoTwistLimits:boolean=false;
    JointFriction:TKraftScalar=0.0;
    CapsuleRollingResistance:TKraftScalar=0.0;
    JointShoulderR:TKraftConstraint=nil;
    JointElbowR:TKraftConstraint=nil;
    JointHipR:TKraftConstraint=nil;

function CreateBoxBody(const Physics:TKraft;const AExtents:TKraftVector3;const ATransform:TKraftMatrix4x4):TKraftRigidBody;
var Shape:TKraftShapeBox;
begin
 result:=TKraftRigidBody.Create(Physics);
 result.SetRigidBodyType(krbtDYNAMIC);
 Shape:=TKraftShapeBox.Create(Physics,result,AExtents);
 Shape.Restitution:=0.1;
 Shape.Density:=BodyDensity;
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
 Shape.Density:=BodyDensity;
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
 Shape.Density:=BodyDensity;
 Shape.RollingResistance:=CapsuleRollingResistance;
 result.Finish;
 result.SetWorldTransformation(ATransform);
 result.CollisionGroups:=[0];
end;

// A ball-socket-like 6DOF: translation locked, elliptical cone (symmetric here for the A/B comparison) and
// twist limits, matching the reference SetSwingTwistLimits configuration.
function Create6DOFSocket(const Physics:TKraft;const ABodyA,ABodyB:TKraftRigidBody;const AAnchor,ATwistAxis:TKraftVector3;const AConeHalfAngle,ALowerTwist,AUpperTwist:TKraftScalar):TKraftConstraintJoint6DOF;
var Joint:TKraftConstraintJoint6DOF;
begin
 Joint:=TKraftConstraintJoint6DOF.Create(Physics,ABodyA,ABodyB,AAnchor,ATwistAxis);
 Joint.SwingMode:=k6damLimited;
 Joint.ConeHalfAngle:=AConeHalfAngle;
 Joint.TwistMode:=k6damLimited;
 Joint.LowerTwistAngle:=ALowerTwist;
 Joint.UpperTwistAngle:=AUpperTwist;
 if JointFriction>0.0 then begin
  Joint.EnableAngularVelocityDrive:=true;
  Joint.TargetAngularVelocity:=Vector3Origin;
  Joint.MaximalAngularDriveTorque:=JointFriction;
 end;
 result:=Joint;
end;

// A hinge-like 6DOF: translation locked, swing locked, twist limited about the hinge axis.
function Create6DOFHinge(const Physics:TKraft;const ABodyA,ABodyB:TKraftRigidBody;const AAnchor,AAxis:TKraftVector3;const ALowerLimit,AUpperLimit:TKraftScalar):TKraftConstraintJoint6DOF;
var Joint:TKraftConstraintJoint6DOF;
begin
 Joint:=TKraftConstraintJoint6DOF.Create(Physics,ABodyA,ABodyB,AAnchor,AAxis);
 Joint.TwistMode:=k6damLimited;
 Joint.LowerTwistAngle:=ALowerLimit;
 Joint.UpperTwistAngle:=AUpperLimit;
 result:=Joint;
end;

procedure BuildScene(const Physics:TKraft);
var Index:longint;
    FloorBody,RampBody:TKraftRigidBody;
    FloorShape:TKraftShapePlane;
    ShapeBox:TKraftShapeBox;
    BallSocketJoint:TKraftConstraintJointBallSocket;
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

 if Use6DOF then begin

  // Spine, neck, shoulders, hips.
  Create6DOFSocket(Physics,RagdollBodies[0],RagdollBodies[1],Vector3(0.0,1.15+BaseY,0.0),Vector3(0.0,1.0,0.0),0.35,-0.25,0.25);
  Create6DOFSocket(Physics,RagdollBodies[1],RagdollBodies[2],Vector3(0.0,1.60+BaseY,0.0),Vector3(0.0,1.0,0.0),0.6,-0.8,0.8);
  JointShoulderR:=Create6DOFSocket(Physics,RagdollBodies[1],RagdollBodies[3],Vector3(0.21,1.48+BaseY,0.0),Vector3(1.0,0.0,0.0),1.3,-0.5,0.5);
  Create6DOFSocket(Physics,RagdollBodies[1],RagdollBodies[5],Vector3(-0.21,1.48+BaseY,0.0),Vector3(-1.0,0.0,0.0),1.3,-0.5,0.5);
  JointHipR:=Create6DOFSocket(Physics,RagdollBodies[0],RagdollBodies[7],Vector3(0.10,0.90+BaseY,0.0),Vector3(0.0,1.0,0.0),0.9,-0.3,0.3);
  Create6DOFSocket(Physics,RagdollBodies[0],RagdollBodies[9],Vector3(-0.10,0.90+BaseY,0.0),Vector3(0.0,1.0,0.0),0.9,-0.3,0.3);

  // Elbows and knees.
  JointElbowR:=Create6DOFHinge(Physics,RagdollBodies[3],RagdollBodies[4],Vector3(0.51,1.48+BaseY,0.0),Vector3(0.0,0.0,1.0),-LimitMargin,2.4);
  Create6DOFHinge(Physics,RagdollBodies[5],RagdollBodies[6],Vector3(-0.51,1.48+BaseY,0.0),Vector3(0.0,0.0,1.0),-2.4,LimitMargin);
  Create6DOFHinge(Physics,RagdollBodies[7],RagdollBodies[8],Vector3(0.10,0.48+BaseY,0.0),Vector3(1.0,0.0,0.0),-LimitMargin,2.4);
  Create6DOFHinge(Physics,RagdollBodies[9],RagdollBodies[10],Vector3(-0.10,0.48+BaseY,0.0),Vector3(1.0,0.0,0.0),-LimitMargin,2.4);

 end else begin

  // Spine, neck, shoulders, hips.
  BallSocketJoint:=TKraftConstraintJointBallSocket.Create(Physics,RagdollBodies[0],RagdollBodies[1],Vector3(0.0,1.15+BaseY,0.0),false);
  BallSocketJoint.SetSwingTwistLimits(true,0.35,true,-0.25,0.25,Vector3(0.0,1.0,0.0));
  BallSocketJoint.AngularFriction:=JointFriction;
  BallSocketJoint:=TKraftConstraintJointBallSocket.Create(Physics,RagdollBodies[1],RagdollBodies[2],Vector3(0.0,1.60+BaseY,0.0),false);
  BallSocketJoint.SetSwingTwistLimits(true,0.6,true,-0.8,0.8,Vector3(0.0,1.0,0.0));
  BallSocketJoint.AngularFriction:=JointFriction;
  BallSocketJoint:=TKraftConstraintJointBallSocket.Create(Physics,RagdollBodies[1],RagdollBodies[3],Vector3(0.21,1.48+BaseY,0.0),false);
  BallSocketJoint.SetSwingTwistLimits(true,1.3,not NoTwistLimits,-0.5,0.5,Vector3(1.0,0.0,0.0));
  BallSocketJoint.AngularFriction:=JointFriction;
  JointShoulderR:=BallSocketJoint;
  BallSocketJoint:=TKraftConstraintJointBallSocket.Create(Physics,RagdollBodies[1],RagdollBodies[5],Vector3(-0.21,1.48+BaseY,0.0),false);
  BallSocketJoint.SetSwingTwistLimits(true,1.3,not NoTwistLimits,-0.5,0.5,Vector3(-1.0,0.0,0.0));
  BallSocketJoint.AngularFriction:=JointFriction;
  BallSocketJoint:=TKraftConstraintJointBallSocket.Create(Physics,RagdollBodies[0],RagdollBodies[7],Vector3(0.10,0.90+BaseY,0.0),false);
  BallSocketJoint.SetSwingTwistLimits(true,0.9,true,-0.3,0.3,Vector3(0.0,1.0,0.0));
  BallSocketJoint.AngularFriction:=JointFriction;
  JointHipR:=BallSocketJoint;
  BallSocketJoint:=TKraftConstraintJointBallSocket.Create(Physics,RagdollBodies[0],RagdollBodies[9],Vector3(-0.10,0.90+BaseY,0.0),false);
  BallSocketJoint.SetSwingTwistLimits(true,0.9,true,-0.3,0.3,Vector3(0.0,1.0,0.0));
  BallSocketJoint.AngularFriction:=JointFriction;

  // Elbows and knees.
  JointElbowR:=TKraftConstraintJointHinge.Create(Physics,RagdollBodies[3],RagdollBodies[4],Vector3(0.51,1.48+BaseY,0.0),Vector3(0.0,0.0,1.0),true,false,-LimitMargin,2.4,0.0,0.0,false);
  TKraftConstraintJointHinge.Create(Physics,RagdollBodies[5],RagdollBodies[6],Vector3(-0.51,1.48+BaseY,0.0),Vector3(0.0,0.0,1.0),true,false,-2.4,LimitMargin,0.0,0.0,false);
  TKraftConstraintJointHinge.Create(Physics,RagdollBodies[7],RagdollBodies[8],Vector3(0.10,0.48+BaseY,0.0),Vector3(1.0,0.0,0.0),true,false,-LimitMargin,2.4,0.0,0.0,false);
  TKraftConstraintJointHinge.Create(Physics,RagdollBodies[9],RagdollBodies[10],Vector3(-0.10,0.48+BaseY,0.0),Vector3(1.0,0.0,0.0),true,false,-LimitMargin,2.4,0.0,0.0,false);

 end;

 for Index:=0 to length(RagdollBodies)-1 do begin
  RagdollBodies[Index].AngularVelocityDamp:=RagdollAngularDamp;
  RagdollBodies[Index].LinearVelocityDamp:=0.2;
 end;

 for Index:=0 to length(RagdollBodies)-1 do begin
  RagdollBodies[Index].LinearVelocity:=Vector3(1.5+TossJitterX,0.5,TossJitterZ);
 end;

end;

procedure RunSmoke(const aName:string;const aSolverMode:TKraftSolverMode;const aTGSJointMode:TKraftTGSJointMode;const a6DOF:boolean);
const StepCount=3600;
var Physics:TKraft;
    StepIndex,Index,TwitchSteps:longint;
    MaxAngVel,AngVel,LateMax,PeakAngVel:TKraftScalar;
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
  Physics.TGSJointMode:=aTGSJointMode;
 if GetEnvironmentVariable('KRAFT_NORECYCLE')='1' then begin
  Physics.ContactRecycleDistance:=0.0;
 end;
 if GetEnvironmentVariable('KRAFT_SPM')='colored' then begin
  Physics.SolverParallelMode:=kspmIslandColored;
 end else begin
  if GetEnvironmentVariable('KRAFT_SPM')='global' then begin
   Physics.SolverParallelMode:=kspmGlobalGraph;
  end;
 end;
 if GetEnvironmentVariable('KRAFT_WIDE')='1' then begin
  Physics.WideContactSolver:=true;
 end;
 if GetEnvironmentVariable('KRAFT_FUSE')='0' then begin
  Physics.SolverFusedColorStages:=false;
 end;
  Use6DOF:=a6DOF;
  TossJitterX:=0.0;
  TossJitterZ:=0.0;
  BuildScene(Physics);
  TwitchSteps:=0;
  LateMax:=0.0;
  PeakAngVel:=0.0;
  for StepIndex:=1 to StepCount do begin
   Physics.Step(1.0/120.0);
   MaxAngVel:=0.0;
   for Index:=0 to 10 do begin
    AngVel:=Vector3Length(RagdollBodies[Index].AngularVelocity);
    if AngVel>MaxAngVel then begin
     MaxAngVel:=AngVel;
    end;
   end;
   PeakAngVel:=Max(PeakAngVel,MaxAngVel);
   if StepIndex>1200 then begin
    LateMax:=Max(LateMax,MaxAngVel);
    if MaxAngVel>0.1 then begin
     inc(TwitchSteps);
    end;
   end;
  end;
  WriteLn(aName,': peak=',PeakAngVel:7:2,' twitchsteps(t>10s)=',TwitchSteps:5,' lateMax=',LateMax:7:2);
 finally
  Physics.Free;
 end;
end;

procedure RunSweep(const aName:string;const aSolverMode:TKraftSolverMode;const aTGSJointMode:TKraftTGSJointMode;const a6DOF:boolean);
const StepCount=3600;
var Physics:TKraft;
    Variant,StepIndex,Index:longint;
    AngVel,MaxAngVel,MaxTransverse,LateMax:TKraftScalar;
    w,LongAxis:TKraftVector3;
    Exploded,ExplodedTransverse,AllAsleep:boolean;
    CountBad,CountBadTransverse,CountRestless,CountStanding:longint;
begin
 CountBad:=0;
 CountBadTransverse:=0;
 CountRestless:=0;
 CountStanding:=0;
 for Variant:=0 to 99 do begin
  Physics:=TKraft.Create(-1);
  try
   Physics.SetFrequency(120.0);
   Physics.VelocityIterations:=8;
   Physics.PositionIterations:=3;
   Physics.SpeculativeIterations:=8;
   Physics.TimeOfImpactIterations:=20;
   Physics.Gravity.y:=-9.81;
   Physics.SolverMode:=aSolverMode;
   Physics.TGSJointMode:=aTGSJointMode;
  if GetEnvironmentVariable('KRAFT_NORECYCLE')='1' then begin
   Physics.ContactRecycleDistance:=0.0;
  end;
  if GetEnvironmentVariable('KRAFT_SPM')='colored' then begin
   Physics.SolverParallelMode:=kspmIslandColored;
  end else begin
   if GetEnvironmentVariable('KRAFT_SPM')='global' then begin
    Physics.SolverParallelMode:=kspmGlobalGraph;
   end;
  end;
  if GetEnvironmentVariable('KRAFT_WIDE')='1' then begin
   Physics.WideContactSolver:=true;
  end;
  if GetEnvironmentVariable('KRAFT_FUSE')='0' then begin
   Physics.SolverFusedColorStages:=false;
  end;
   Use6DOF:=a6DOF;
   BodyDensity:=1.0;
   TossJitterX:=(Variant mod 20)*0.007;
   TossJitterZ:=((Variant div 20)-2)*0.011+((Variant mod 20)-10)*0.004;
   BuildScene(Physics);
   LateMax:=0.0;
   Exploded:=false;
   ExplodedTransverse:=false;
   for StepIndex:=1 to StepCount do begin
    Physics.Step(1.0/120.0);
    MaxAngVel:=0.0;
    MaxTransverse:=0.0;
    for Index:=0 to 10 do begin
     w:=RagdollBodies[Index].AngularVelocity;
     AngVel:=Vector3Length(w);
     if AngVel>MaxAngVel then begin
      MaxAngVel:=AngVel;
     end;
     // The long-axis spin of the thin capsules is the harmless flash class, so track the transverse part
     // separately (bodies 3..10 are the capsules; boxes and the head keep the full magnitude).
     if Index>=3 then begin
      LongAxis:=Vector3TermQuaternionRotate(Vector3(0.0,1.0,0.0),RagdollBodies[Index].Sweep.q);
      AngVel:=Vector3Length(Vector3Sub(w,Vector3ScalarMul(LongAxis,Vector3Dot(w,LongAxis))));
     end;
     if AngVel>MaxTransverse then begin
      MaxTransverse:=AngVel;
     end;
    end;
    if StepIndex>2400 then begin
     LateMax:=Max(LateMax,MaxAngVel);
    end;
    if MaxAngVel>100.0 then begin
     Exploded:=true;
    end;
    if MaxTransverse>100.0 then begin
     ExplodedTransverse:=true;
    end;
   end;
   AllAsleep:=true;
   for Index:=0 to 10 do begin
    if krbfAwake in RagdollBodies[Index].Flags then begin
     AllAsleep:=false;
     break;
    end;
   end;
   // Liveness: the doll must actually have FALLEN (a rotationally frozen doll standing bent over also
   // sleeps perfectly, which once masked an overstrong rolling resistance).
   if RagdollBodies[1].Sweep.c.y>0.9 then begin
    inc(CountStanding);
   end;
   if Exploded then begin
    inc(CountBad);
    if ExplodedTransverse then begin
     inc(CountBadTransverse);
    end;
   end else if not AllAsleep then begin
    inc(CountRestless);
   end;
  finally
   Physics.Free;
  end;
 end;
 WriteLn(aName,': ',100-CountBad-CountRestless,'/100 asleep, ',CountRestless,' awake, ',CountBad,' explosive (of which ',CountBadTransverse,' truly transverse), ',CountStanding,' NOT fallen');
end;

const BodyNames:array[0..10] of string=('Pelvis','Torso','Head','UpArmR','LoArmR','UpArmL','LoArmL','UpLegR','LoLegR','UpLegL','LoLegL');

// Rerun the sweep and report every exploding variant in detail: which variant, when the blow-up starts,
// which body leads it and how far it goes.
procedure RunHunt(const aName:string;const aSolverMode:TKraftSolverMode;const aTGSJointMode:TKraftTGSJointMode;const a6DOF:boolean);
const StepCount=3600;
var Physics:TKraft;
    Variant,StepIndex,Index,FirstBadStep,FirstBadBody,PeakBody:longint;
    AngVel,MaxAngVel,PeakAngVel:TKraftScalar;
begin
 for Variant:=0 to 99 do begin
  Physics:=TKraft.Create(-1);
  try
   Physics.SetFrequency(120.0);
   Physics.VelocityIterations:=8;
   Physics.PositionIterations:=3;
   Physics.SpeculativeIterations:=8;
   Physics.TimeOfImpactIterations:=20;
   Physics.Gravity.y:=-9.81;
   Physics.SolverMode:=aSolverMode;
   Physics.TGSJointMode:=aTGSJointMode;
  if GetEnvironmentVariable('KRAFT_NORECYCLE')='1' then begin
   Physics.ContactRecycleDistance:=0.0;
  end;
  if GetEnvironmentVariable('KRAFT_SPM')='colored' then begin
   Physics.SolverParallelMode:=kspmIslandColored;
  end else begin
   if GetEnvironmentVariable('KRAFT_SPM')='global' then begin
    Physics.SolverParallelMode:=kspmGlobalGraph;
   end;
  end;
  if GetEnvironmentVariable('KRAFT_WIDE')='1' then begin
   Physics.WideContactSolver:=true;
  end;
  if GetEnvironmentVariable('KRAFT_FUSE')='0' then begin
   Physics.SolverFusedColorStages:=false;
  end;
   Use6DOF:=a6DOF;
   BodyDensity:=1.0;
   TossJitterX:=(Variant mod 20)*0.007;
   TossJitterZ:=((Variant div 20)-2)*0.011+((Variant mod 20)-10)*0.004;
   BuildScene(Physics);
   FirstBadStep:=-1;
   FirstBadBody:=-1;
   PeakBody:=-1;
   PeakAngVel:=0.0;
   for StepIndex:=1 to StepCount do begin
    Physics.Step(1.0/120.0);
    MaxAngVel:=0.0;
    for Index:=0 to 10 do begin
     AngVel:=Vector3Length(RagdollBodies[Index].AngularVelocity);
     if AngVel>MaxAngVel then begin
      MaxAngVel:=AngVel;
      if AngVel>PeakAngVel then begin
       PeakAngVel:=AngVel;
       PeakBody:=Index;
      end;
      if (FirstBadStep<0) and (AngVel>100.0) then begin
       FirstBadStep:=StepIndex;
       FirstBadBody:=Index;
      end;
     end;
    end;
   end;
   if FirstBadStep>=0 then begin
    WriteLn(aName,' variant=',Variant:3,' EXPLOSIVE: first >100 at t=',(FirstBadStep/120.0):7:3,'s Body=',BodyNames[FirstBadBody],' peak=',PeakAngVel:10:1,' (',BodyNames[PeakBody],')');
   end;
  finally
   Physics.Free;
  end;
 end;
 WriteLn(aName,': hunt fertig');
end;

// Rerun the sweep and characterize every variant that never falls asleep: which body blocks the sleep (the
// one furthest above its threshold, measured relative to the linear/angular sleep thresholds), how hard it
// still moves at the end, and how often the island got its sleep timer reset during the last five seconds.
procedure RunHuntAwake(const aName:string;const aSolverMode:TKraftSolverMode;const aTGSJointMode:TKraftTGSJointMode;const a6DOF:boolean);
const StepCount=3600;
var Physics:TKraft;
    Variant,StepIndex,Index,BlockerBody,ResetSteps:longint;
    AngVel,LinVel,Ratio,MaxRatio,EndLin,EndAng:TKraftScalar;
    AllAsleep,OverThreshold:boolean;
begin
 for Variant:=0 to 99 do begin
  Physics:=TKraft.Create(-1);
  try
   Physics.SetFrequency(120.0);
   Physics.VelocityIterations:=8;
   Physics.PositionIterations:=3;
   Physics.SpeculativeIterations:=8;
   Physics.TimeOfImpactIterations:=20;
   Physics.Gravity.y:=-9.81;
   Physics.SolverMode:=aSolverMode;
   Physics.TGSJointMode:=aTGSJointMode;
  if GetEnvironmentVariable('KRAFT_NORECYCLE')='1' then begin
   Physics.ContactRecycleDistance:=0.0;
  end;
  if GetEnvironmentVariable('KRAFT_SPM')='colored' then begin
   Physics.SolverParallelMode:=kspmIslandColored;
  end else begin
   if GetEnvironmentVariable('KRAFT_SPM')='global' then begin
    Physics.SolverParallelMode:=kspmGlobalGraph;
   end;
  end;
  if GetEnvironmentVariable('KRAFT_WIDE')='1' then begin
   Physics.WideContactSolver:=true;
  end;
  if GetEnvironmentVariable('KRAFT_FUSE')='0' then begin
   Physics.SolverFusedColorStages:=false;
  end;
   Use6DOF:=a6DOF;
   BodyDensity:=1.0;
   TossJitterX:=(Variant mod 20)*0.007;
   TossJitterZ:=((Variant div 20)-2)*0.011+((Variant mod 20)-10)*0.004;
   BuildScene(Physics);
   ResetSteps:=0;
   for StepIndex:=1 to StepCount do begin
    Physics.Step(1.0/120.0);
    if StepIndex>3000 then begin
     OverThreshold:=false;
     for Index:=0 to 10 do begin
      if (Vector3Length(RagdollBodies[Index].LinearVelocity)>Physics.LinearVelocityThreshold) or
         (Vector3Length(RagdollBodies[Index].AngularVelocity)>Physics.AngularVelocityThreshold) then begin
       OverThreshold:=true;
       break;
      end;
     end;
     if OverThreshold then begin
      inc(ResetSteps);
     end;
    end;
   end;
   AllAsleep:=true;
   for Index:=0 to 10 do begin
    if krbfAwake in RagdollBodies[Index].Flags then begin
     AllAsleep:=false;
     break;
    end;
   end;
   if not AllAsleep then begin
    BlockerBody:=0;
    MaxRatio:=0.0;
    for Index:=0 to 10 do begin
     LinVel:=Vector3Length(RagdollBodies[Index].LinearVelocity);
     AngVel:=Vector3Length(RagdollBodies[Index].AngularVelocity);
     Ratio:=Max(LinVel/Physics.LinearVelocityThreshold,AngVel/Physics.AngularVelocityThreshold);
     if Ratio>MaxRatio then begin
      MaxRatio:=Ratio;
      BlockerBody:=Index;
     end;
    end;
    EndLin:=Vector3Length(RagdollBodies[BlockerBody].LinearVelocity);
    EndAng:=Vector3Length(RagdollBodies[BlockerBody].AngularVelocity);
    WriteLn(aName,' variant=',Variant:3,' AWAKE: blocker=',BodyNames[BlockerBody],
            ' endLin=',EndLin:8:4,' endAng=',EndAng:8:4,' ratio=',MaxRatio:7:2,
            ' resetSteps(last 5s)=',ResetSteps:4,'/600');
   end;
  finally
   Physics.Free;
  end;
 end;
 WriteLn(aName,': awake-hunt fertig');
end;

// Single-variant awake probe: replays one sweep variant and prints the steady-state tail, decomposing the
// sleep blocker's angular velocity into its long axis versus transverse part, plus the pose of the arm chain
// (is it pinned under the body?), to identify what keeps feeding the residual motion.
function TwistOf(const aBodyA,aBodyB:TKraftRigidBody;const aWorldAxis:TKraftVector3):TKraftScalar;
var RelQ:TKraftQuaternion;
    ProjectedDot:TKraftScalar;
begin
 RelQ:=QuaternionTermNormalize(QuaternionMul(QuaternionConjugate(aBodyA.Sweep.q),aBodyB.Sweep.q));
 if RelQ.w<0.0 then begin
  RelQ.x:=-RelQ.x;
  RelQ.y:=-RelQ.y;
  RelQ.z:=-RelQ.z;
  RelQ.w:=-RelQ.w;
 end;
 ProjectedDot:=(RelQ.x*aWorldAxis.x)+(RelQ.y*aWorldAxis.y)+(RelQ.z*aWorldAxis.z);
 result:=2.0*ArcTan2(ProjectedDot,RelQ.w);
end;

procedure RunProbeAwake(const aName:string;const aVariant:longint;const a6DOF:boolean);
const StepCount=3600;
var Physics:TKraft;
    StepIndex,Index,TopBody:longint;
    Ratio,TopRatio,LinVel,AngVel,LongSpin:TKraftScalar;
    w,LongAxis:TKraftVector3;
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
  Use6DOF:=a6DOF;
  BodyDensity:=1.0;
  TossJitterX:=(aVariant mod 20)*0.007;
  TossJitterZ:=((aVariant div 20)-2)*0.011+((aVariant mod 20)-10)*0.004;
  BuildScene(Physics);
  for StepIndex:=1 to StepCount do begin
   Physics.Step(1.0/120.0);
   if (StepIndex>3000) and ((StepIndex mod 30)=0) then begin
    TopBody:=0;
    TopRatio:=0.0;
    for Index:=0 to 10 do begin
     LinVel:=Vector3Length(RagdollBodies[Index].LinearVelocity);
     AngVel:=Vector3Length(RagdollBodies[Index].AngularVelocity);
     Ratio:=Max(LinVel/Physics.LinearVelocityThreshold,AngVel/Physics.AngularVelocityThreshold);
     if Ratio>TopRatio then begin
      TopRatio:=Ratio;
      TopBody:=Index;
     end;
    end;
    w:=RagdollBodies[TopBody].AngularVelocity;
    LongAxis:=Vector3TermQuaternionRotate(Vector3(0.0,1.0,0.0),RagdollBodies[TopBody].Sweep.q);
    LongSpin:=Vector3Dot(w,LongAxis);
    LongAxis:=Vector3TermQuaternionRotate(Vector3(0.0,1.0,0.0),RagdollBodies[5].Sweep.q);
    WriteLn(aName,' t=',(StepIndex/120.0):7:3,' blocker=',BodyNames[TopBody],' |w|=',Vector3Length(w):8:4,
            ' wLong=',LongSpin:8:4,' |v|=',Vector3Length(RagdollBodies[TopBody].LinearVelocity):8:4,
            ' y=',RagdollBodies[TopBody].Sweep.c.y:7:3,
            ' torsoY=',RagdollBodies[1].Sweep.c.y:7:3,
            ' twistL=',TwistOf(RagdollBodies[1],RagdollBodies[5],LongAxis):8:4);
   end;
  end;
 finally
  Physics.Free;
 end;
end;

// Single-variant deep probe: replays one sweep variant and prints a time series around the blow-up with the
// leading bodies and the externally reconstructed relative joint angles of the usual suspects (shoulder R,
// elbow R, hip R), so the runaway joint and its limit state become visible.
procedure RunProbe(const aName:string;const aVariant:longint;const a6DOF:boolean);
const StepCount=3600;
var Physics:TKraft;
    StepIndex,Index,TopBody:longint;
    AngVel,TopVel:TKraftScalar;
    SetupRelShoulder,SetupRelElbow,SetupRelHip,RelQ,DeltaQ:TKraftQuaternion;
    Verbose:boolean;
 function RelativeOrientation(const aBodyA,aBodyB:TKraftRigidBody):TKraftQuaternion;
 begin
  result:=QuaternionTermNormalize(QuaternionMul(QuaternionConjugate(aBodyA.Sweep.q),aBodyB.Sweep.q));
 end;
 function DeviationAngle(const aSetupRel:TKraftQuaternion;const aBodyA,aBodyB:TKraftRigidBody):TKraftScalar;
 begin
  RelQ:=RelativeOrientation(aBodyA,aBodyB);
  DeltaQ:=QuaternionMul(QuaternionConjugate(aSetupRel),RelQ);
  if DeltaQ.w<0.0 then begin
   DeltaQ.w:=-DeltaQ.w;
  end;
  result:=2.0*ArcCos(Min(DeltaQ.w,1.0));
 end;
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
  Use6DOF:=a6DOF;
  BodyDensity:=1.0;
  TossJitterX:=(aVariant mod 20)*0.007;
  TossJitterZ:=((aVariant div 20)-2)*0.011+((aVariant mod 20)-10)*0.004;
  BuildScene(Physics);
  SetupRelShoulder:=RelativeOrientation(RagdollBodies[1],RagdollBodies[3]);
  SetupRelElbow:=RelativeOrientation(RagdollBodies[3],RagdollBodies[4]);
  SetupRelHip:=RelativeOrientation(RagdollBodies[0],RagdollBodies[7]);
  Verbose:=false;
  for StepIndex:=1 to StepCount do begin
   Physics.Step(1.0/120.0);
   TopVel:=0.0;
   TopBody:=0;
   for Index:=0 to 10 do begin
    AngVel:=Vector3Length(RagdollBodies[Index].AngularVelocity);
    if AngVel>TopVel then begin
     TopVel:=AngVel;
     TopBody:=Index;
    end;
   end;
   if (not Verbose) and (TopVel>15.0) then begin
    Verbose:=true;
    WriteLn(aName,' verbose ab t=',(StepIndex/120.0):7:3,'s');
   end;
   if Verbose or ((StepIndex mod 120)=0) then begin
    // Angular velocity of the leading body decomposed into its own axes: the local y part is the spin about
    // the long axis of the capsules, whose tiny inertia is the flash-transient suspect.
    RelQ:=RagdollBodies[TopBody].Sweep.q;
    DeltaQ.x:=0.0;
    WriteLn(aName,' t=',(StepIndex/120.0):7:3,' top=',BodyNames[TopBody],' w=',TopVel:9:2,
            ' wLongAxis=',abs(Vector3Dot(RagdollBodies[TopBody].AngularVelocity,Vector3TermQuaternionRotate(Vector3(0.0,1.0,0.0),RelQ))):9:2,
            ' shoulderR=',DeviationAngle(SetupRelShoulder,RagdollBodies[1],RagdollBodies[3]):6:3,
            ' elbowR=',DeviationAngle(SetupRelElbow,RagdollBodies[3],RagdollBodies[4]):6:3,
            ' pelvY=',RagdollBodies[0].Sweep.c.y:6:2,' armRY=',RagdollBodies[3].Sweep.c.y:6:2,
            ' shT=',Vector3Length(JointShoulderR.GetReactionTorque(120.0)):9:3);
   end;
   if TopVel>150.0 then begin
    RelQ:=RagdollBodies[TopBody].Sweep.q;
    WriteLn(aName,' ABORT at t=',(StepIndex/120.0):7:3,'s: ',BodyNames[TopBody],' w=',TopVel:9:2,
            ' wLongAxis=',abs(Vector3Dot(RagdollBodies[TopBody].AngularVelocity,Vector3TermQuaternionRotate(Vector3(0.0,1.0,0.0),RelQ))):9:2,
            ' y=',RagdollBodies[TopBody].Sweep.c.y:7:3,
            ' vLin=',Vector3Length(RagdollBodies[TopBody].LinearVelocity):9:2);
    break;
   end;
  end;
 finally
  Physics.Free;
 end;
end;

var Mode:string;
begin
 FormatSettings.DecimalSeparator:='.';
 Mode:=LowerCase(ParamStr(1));
 RagdollAngularDamp:=2.0;
 LimitMargin:=0.10;
 if (Mode='') or (Mode='smoke') then begin
  RunSmoke('SMOKE REF NAT',ksmTGSSoft,ktjmNativeSoft,false);
  RunSmoke('SMOKE 6DF NAT',ksmTGSSoft,ktjmNativeSoft,true);
  RunSmoke('SMOKE REF SI ',ksmSequentialImpulse,ktjmAdapter,false);
  RunSmoke('SMOKE 6DF SI ',ksmSequentialImpulse,ktjmAdapter,true);
 end;
 if (Mode='') or (Mode='refsi') then begin
  RunSweep('SWEEP REF SI    ',ksmSequentialImpulse,ktjmAdapter,false);
 end;
 if (Mode='') or (Mode='refnat') then begin
  RunSweep('SWEEP REF NAT   ',ksmTGSSoft,ktjmNativeSoft,false);
 end;
 if (Mode='') or (Mode='sixsi') then begin
  RunSweep('SWEEP 6DF SI    ',ksmSequentialImpulse,ktjmAdapter,true);
 end;
 if (Mode='') or (Mode='sixnat') then begin
  RunSweep('SWEEP 6DF NAT   ',ksmTGSSoft,ktjmNativeSoft,true);
 end;
 if (Mode='') or (Mode='sixadpsub') then begin
  RunSweep('SWEEP 6DF ADPSUB',ksmTGSSoft,ktjmAdapterSubstep,true);
 end;
 if Mode='refadpsub' then begin
  RunSweep('SWEEP REF ADPSUB',ksmTGSSoft,ktjmAdapterSubstep,false);
 end;
 if Mode='huntref' then begin
  RunHunt('HUNT REF NAT',ksmTGSSoft,ktjmNativeSoft,false);
 end;
 if Mode='huntsix' then begin
  RunHunt('HUNT 6DF NAT',ksmTGSSoft,ktjmNativeSoft,true);
 end;
 if Mode='awakerefnat' then begin
  RunHuntAwake('AWAKE REF NAT',ksmTGSSoft,ktjmNativeSoft,false);
 end;
 if Mode='awakesixnat' then begin
  RunHuntAwake('AWAKE 6DF NAT',ksmTGSSoft,ktjmNativeSoft,true);
 end;
 if Mode='awakerefsi' then begin
  RunHuntAwake('AWAKE REF SI ',ksmSequentialImpulse,ktjmAdapter,false);
 end;
 if Mode='awakeprobe85' then begin
  RunProbeAwake('AWKPROBE REF v85',85,false);
 end;
 if Mode='rrtestsix' then begin
  JointFriction:=0.005;
  CapsuleRollingResistance:=0.1;
  RunSweep('SWEEP 6DF NAT FRICT RR=0.10',ksmTGSSoft,ktjmNativeSoft,true);
 end;
 if Mode='rrtest' then begin
  JointFriction:=0.005;
  RunSweep('SWEEP REF NAT FRICT RR=0   ',ksmTGSSoft,ktjmNativeSoft,false);
  CapsuleRollingResistance:=0.1;
  RunSweep('SWEEP REF NAT FRICT RR=0.10',ksmTGSSoft,ktjmNativeSoft,false);
  CapsuleRollingResistance:=0.2;
  RunSweep('SWEEP REF NAT FRICT RR=0.20',ksmTGSSoft,ktjmNativeSoft,false);
 end;
 if Mode='probefrict73' then begin
  JointFriction:=0.005;
  RunProbe('PROBE FRICT v73',73,false);
 end;
 if Mode='huntreffriction' then begin
  JointFriction:=0.005;
  RunHunt('HUNT REF NAT FRICT',ksmTGSSoft,ktjmNativeSoft,false);
 end;
 if Mode='refnatfriction' then begin
  JointFriction:=0.005;
  RunSweep('SWEEP REF NAT FRICT',ksmTGSSoft,ktjmNativeSoft,false);
 end;
 if Mode='sixnatfriction' then begin
  JointFriction:=0.005;
  RunSweep('SWEEP 6DF NAT FRICT',ksmTGSSoft,ktjmNativeSoft,true);
 end;
 if Mode='awakeprobe85notwist' then begin
  NoTwistLimits:=true;
  RunProbeAwake('AWKPROBE NOTWIST v85',85,false);
 end;
 if Mode='proberef1' then begin
  RunProbe('PROBE REF v1 ',1,false);
 end;
 if Mode='proberef23' then begin
  RunProbe('PROBE REF v23',23,false);
 end;
 if Mode='probesix21' then begin
  RunProbe('PROBE 6DF v21',21,true);
 end;
end.
