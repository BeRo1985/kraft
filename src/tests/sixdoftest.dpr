program sixdoftest;

{$ifdef fpc}
 {$mode delphi}
{$endif}
{$apptype console}

uses {$ifdef unix}cthreads,{$endif} SysUtils,Math,kraft;

// Behaviour battery for TKraftConstraintJoint6DOF stage 1 across all solver modes:
//  weld      - default (all locked) must hold a box on a 0.5 m lever rigidly
//  free      - swing+twist free must behave like a pure point-to-point pendulum (liveness ~8.7 rad/s peak)
//  cone      - swing limited to 0.5 rad must catch the falling arm inside the cone
//  twist     - twist limited to +/-0.3 rad must catch a 5 rad/s spin, swing lock keeps the axis aligned
//  spinfree  - twist free under swing lock must neither brake the spin nor let the axis wander

var FailureCount:longint=0;

procedure Check(const aName,aMetric:string;const aValue:TKraftScalar;const aOK:boolean);
begin
 if aOK then begin
  WriteLn(aName,' ',aMetric,'=',aValue:8:4,'  PASS');
 end else begin
  WriteLn(aName,' ',aMetric,'=',aValue:8:4,'  FAIL');
  inc(FailureCount);
 end;
end;

function CreatePhysics(const aSolverMode:TKraftSolverMode;const aJointMode:TKraftTGSJointMode):TKraft;
begin
 result:=TKraft.Create(-1);
 result.SetFrequency(120.0);
 result.VelocityIterations:=8;
 result.PositionIterations:=3;
 result.Gravity.y:=-9.81;
 result.SolverMode:=aSolverMode;
 result.TGSJointMode:=aJointMode;
end;

function MakeStaticAnchor(const aPhysics:TKraft;const aPosition:TKraftVector3):TKraftRigidBody;
begin
 result:=TKraftRigidBody.Create(aPhysics);
 result.SetRigidBodyType(krbtSTATIC);
 TKraftShapeBox.Create(aPhysics,result,Vector3(0.05,0.05,0.05));
 result.Finish;
 result.SetWorldTransformation(Matrix4x4Translate(aPosition.x,aPosition.y,aPosition.z));
 result.CollisionGroups:=[0];
end;

// Angle of the deviation of a body-carried axis from its setup direction (the swing of that axis).
function AxisDeviation(const aBody:TKraftRigidBody;const aSetupOrientation:TKraftQuaternion;const aWorldAxis:TKraftVector3):TKraftScalar;
var RelQ:TKraftQuaternion;
    AxisNow:TKraftVector3;
    CosAngle:TKraftScalar;
begin
 RelQ:=QuaternionMul(aBody.Sweep.q,QuaternionConjugate(aSetupOrientation));
 AxisNow:=Vector3TermQuaternionRotate(aWorldAxis,RelQ);
 CosAngle:=Min(Max(Vector3Dot(AxisNow,aWorldAxis),-1.0),1.0);
 result:=ArcCos(CosAngle);
end;

// Signed rotation of the body about a world axis since setup (valid while the swing off that axis is small).
function TwistAbout(const aBody:TKraftRigidBody;const aSetupOrientation:TKraftQuaternion;const aWorldAxis:TKraftVector3):TKraftScalar;
var RelQ:TKraftQuaternion;
    ProjectedDot:TKraftScalar;
begin
 RelQ:=QuaternionMul(aBody.Sweep.q,QuaternionConjugate(aSetupOrientation));
 if RelQ.w<0.0 then begin
  RelQ.x:=-RelQ.x;
  RelQ.y:=-RelQ.y;
  RelQ.z:=-RelQ.z;
  RelQ.w:=-RelQ.w;
 end;
 ProjectedDot:=(RelQ.x*aWorldAxis.x)+(RelQ.y*aWorldAxis.y)+(RelQ.z*aWorldAxis.z);
 result:=2.0*ArcTan2(ProjectedDot,RelQ.w);
end;

procedure RunWeld(const aName:string;const aSolverMode:TKraftSolverMode;const aJointMode:TKraftTGSJointMode);
const StepCount=480;
var Physics:TKraft;
    AnchorBody,BoxBody:TKraftRigidBody;
    SetupOrientation:TKraftQuaternion;
    StepIndex:longint;
    PositionDrift,OrientationDrift,QuatDot:TKraftScalar;
begin
 Physics:=CreatePhysics(aSolverMode,aJointMode);
 try
  AnchorBody:=MakeStaticAnchor(Physics,Vector3(0.0,2.0,0.0));

  BoxBody:=TKraftRigidBody.Create(Physics);
  BoxBody.SetRigidBodyType(krbtDYNAMIC);
  TKraftShapeBox.Create(Physics,BoxBody,Vector3(0.1,0.1,0.1));
  BoxBody.Finish;
  BoxBody.SetWorldTransformation(Matrix4x4Translate(0.5,2.0,0.0));
  BoxBody.CollisionGroups:=[0];
  SetupOrientation:=BoxBody.Sweep.q;

  // All degrees of freedom stay at the locked default.
  TKraftConstraintJoint6DOF.Create(Physics,AnchorBody,BoxBody,Vector3(0.0,2.0,0.0),Vector3(0.0,0.0,1.0));

  for StepIndex:=1 to StepCount do begin
   Physics.Step(1.0/120.0);
  end;

  PositionDrift:=Vector3Length(Vector3Sub(BoxBody.Sweep.c,Vector3(0.5,2.0,0.0)));
  QuatDot:=abs((BoxBody.Sweep.q.x*SetupOrientation.x)+(BoxBody.Sweep.q.y*SetupOrientation.y)+(BoxBody.Sweep.q.z*SetupOrientation.z)+(BoxBody.Sweep.q.w*SetupOrientation.w));
  OrientationDrift:=2.0*ArcCos(Min(QuatDot,1.0));
  Check(aName+' weld    ','posDrift',PositionDrift,(PositionDrift<0.05) and (OrientationDrift<0.06) and (Vector3Length(BoxBody.AngularVelocity)<0.5));
 finally
  Physics.Free;
 end;
end;

procedure RunFreePendulum(const aName:string;const aSolverMode:TKraftSolverMode;const aJointMode:TKraftTGSJointMode);
const StepCount=480;
var Physics:TKraft;
    AnchorBody,ArmBody:TKraftRigidBody;
    Shape:TKraftShapeCapsule;
    Joint:TKraftConstraintJoint6DOF;
    StepIndex:longint;
    AngVel,Peak:TKraftScalar;
begin
 Physics:=CreatePhysics(aSolverMode,aJointMode);
 try
  AnchorBody:=MakeStaticAnchor(Physics,Vector3(0.0,2.0,0.0));

  ArmBody:=TKraftRigidBody.Create(Physics);
  ArmBody.SetRigidBodyType(krbtDYNAMIC);
  Shape:=TKraftShapeCapsule.Create(Physics,ArmBody,0.045,0.30);
  Shape.Density:=1.0;
  ArmBody.Finish;
  ArmBody.SetWorldTransformation(Matrix4x4TermMul(Matrix4x4RotateZ(0.5*pi),Matrix4x4Translate(0.20,2.0,0.0)));
  ArmBody.CollisionGroups:=[0];
  ArmBody.LinearVelocityDamp:=0.0;
  ArmBody.AngularVelocityDamp:=0.0;

  // Swing and twist free: a pure point-to-point pendulum.
  Joint:=TKraftConstraintJoint6DOF.Create(Physics,AnchorBody,ArmBody,Vector3(0.0,2.0,0.0),Vector3(0.0,0.0,1.0));
  Joint.SwingMode:=k6damFree;
  Joint.TwistMode:=k6damFree;

  Peak:=0.0;
  for StepIndex:=1 to StepCount do begin
   Physics.Step(1.0/120.0);
   AngVel:=Vector3Length(ArmBody.AngularVelocity);
   if AngVel>Peak then begin
    Peak:=AngVel;
   end;
  end;
  Check(aName+' free    ','peakAngVel',Peak,(Peak>7.0) and (Peak<10.0));
 finally
  Physics.Free;
 end;
end;

procedure RunConeLimit(const aName:string;const aSolverMode:TKraftSolverMode;const aJointMode:TKraftTGSJointMode);
const StepCount=480;
var Physics:TKraft;
    AnchorBody,ArmBody:TKraftRigidBody;
    Shape:TKraftShapeCapsule;
    Joint:TKraftConstraintJoint6DOF;
    SetupOrientation:TKraftQuaternion;
    StepIndex:longint;
    Swing,MaxSwing,AngVel,Peak:TKraftScalar;
begin
 Physics:=CreatePhysics(aSolverMode,aJointMode);
 try
  AnchorBody:=MakeStaticAnchor(Physics,Vector3(0.0,2.0,0.0));

  ArmBody:=TKraftRigidBody.Create(Physics);
  ArmBody.SetRigidBodyType(krbtDYNAMIC);
  Shape:=TKraftShapeCapsule.Create(Physics,ArmBody,0.045,0.30);
  Shape.Density:=1.0;
  ArmBody.Finish;
  ArmBody.SetWorldTransformation(Matrix4x4TermMul(Matrix4x4RotateZ(0.5*pi),Matrix4x4Translate(0.20,2.0,0.0)));
  ArmBody.CollisionGroups:=[0];
  SetupOrientation:=ArmBody.Sweep.q;

  // Twist axis along the arm; the falling arm is pure swing, caught by the 0.5 rad cone.
  Joint:=TKraftConstraintJoint6DOF.Create(Physics,AnchorBody,ArmBody,Vector3(0.0,2.0,0.0),Vector3(1.0,0.0,0.0));
  Joint.SwingMode:=k6damLimited;
  Joint.ConeHalfAngle:=0.5;

  MaxSwing:=0.0;
  Peak:=0.0;
  for StepIndex:=1 to StepCount do begin
   Physics.Step(1.0/120.0);
   Swing:=AxisDeviation(ArmBody,SetupOrientation,Vector3(1.0,0.0,0.0));
   if Swing>MaxSwing then begin
    MaxSwing:=Swing;
   end;
   AngVel:=Vector3Length(ArmBody.AngularVelocity);
   if AngVel>Peak then begin
    Peak:=AngVel;
   end;
  end;
  Check(aName+' cone    ','maxSwing',MaxSwing,(MaxSwing<0.65) and (MaxSwing>0.4) and (Peak>1.0));
 finally
  Physics.Free;
 end;
end;

procedure RunTwistLimit(const aName:string;const aSolverMode:TKraftSolverMode;const aJointMode:TKraftTGSJointMode);
const StepCount=480;
var Physics:TKraft;
    AnchorBody,ArmBody:TKraftRigidBody;
    Shape:TKraftShapeCapsule;
    Joint:TKraftConstraintJoint6DOF;
    SetupOrientation:TKraftQuaternion;
    StepIndex:longint;
    Twist,MaxTwist:TKraftScalar;
begin
 Physics:=CreatePhysics(aSolverMode,aJointMode);
 try
  AnchorBody:=MakeStaticAnchor(Physics,Vector3(0.0,2.0,0.0));

  ArmBody:=TKraftRigidBody.Create(Physics);
  ArmBody.SetRigidBodyType(krbtDYNAMIC);
  Shape:=TKraftShapeCapsule.Create(Physics,ArmBody,0.045,0.30);
  Shape.Density:=1.0;
  ArmBody.Finish;
  ArmBody.SetWorldTransformation(Matrix4x4Translate(0.0,1.80,0.0));
  ArmBody.CollisionGroups:=[0];
  SetupOrientation:=ArmBody.Sweep.q;

  // Hanging arm, twist axis vertical; a 5 rad/s spin must be caught at the +/-0.3 rad twist limits.
  Joint:=TKraftConstraintJoint6DOF.Create(Physics,AnchorBody,ArmBody,Vector3(0.0,2.0,0.0),Vector3(0.0,1.0,0.0));
  Joint.TwistMode:=k6damLimited;
  Joint.LowerTwistAngle:=-0.3;
  Joint.UpperTwistAngle:=0.3;

  ArmBody.AngularVelocity:=Vector3(0.0,5.0,0.0);

  MaxTwist:=0.0;
  for StepIndex:=1 to StepCount do begin
   Physics.Step(1.0/120.0);
   Twist:=abs(TwistAbout(ArmBody,SetupOrientation,Vector3(0.0,1.0,0.0)));
   if Twist>MaxTwist then begin
    MaxTwist:=Twist;
   end;
  end;
  Check(aName+' twist   ','maxTwist',MaxTwist,(MaxTwist<0.45) and (MaxTwist>0.25));
 finally
  Physics.Free;
 end;
end;

procedure RunSpinFree(const aName:string;const aSolverMode:TKraftSolverMode;const aJointMode:TKraftTGSJointMode);
const StepCount=480;
var Physics:TKraft;
    AnchorBody,ArmBody:TKraftRigidBody;
    Shape:TKraftShapeCapsule;
    Joint:TKraftConstraintJoint6DOF;
    SetupOrientation:TKraftQuaternion;
    StepIndex:longint;
    EndSpin,AxisDev,MaxAxisDev:TKraftScalar;
begin
 Physics:=CreatePhysics(aSolverMode,aJointMode);
 try
  AnchorBody:=MakeStaticAnchor(Physics,Vector3(0.0,2.0,0.0));

  ArmBody:=TKraftRigidBody.Create(Physics);
  ArmBody.SetRigidBodyType(krbtDYNAMIC);
  Shape:=TKraftShapeCapsule.Create(Physics,ArmBody,0.045,0.30);
  Shape.Density:=1.0;
  ArmBody.Finish;
  ArmBody.SetWorldTransformation(Matrix4x4Translate(0.0,1.80,0.0));
  ArmBody.CollisionGroups:=[0];
  ArmBody.LinearVelocityDamp:=0.0;
  ArmBody.AngularVelocityDamp:=0.0;
  SetupOrientation:=ArmBody.Sweep.q;

  // Free twist under the swing lock: the spin about the vertical twist axis must survive, the axis must not wander.
  Joint:=TKraftConstraintJoint6DOF.Create(Physics,AnchorBody,ArmBody,Vector3(0.0,2.0,0.0),Vector3(0.0,1.0,0.0));
  Joint.TwistMode:=k6damFree;

  ArmBody.AngularVelocity:=Vector3(0.0,6.0,0.0);

  MaxAxisDev:=0.0;
  for StepIndex:=1 to StepCount do begin
   Physics.Step(1.0/120.0);
   AxisDev:=AxisDeviation(ArmBody,SetupOrientation,Vector3(0.0,1.0,0.0));
   if AxisDev>MaxAxisDev then begin
    MaxAxisDev:=AxisDev;
   end;
  end;
  EndSpin:=ArmBody.AngularVelocity.y;
  Check(aName+' spinfree','endSpin',EndSpin,(EndSpin>4.0) and (MaxAxisDev<0.06));
 finally
  Physics.Free;
 end;
end;

procedure RunPrismaticLimited(const aName:string;const aSolverMode:TKraftSolverMode;const aJointMode:TKraftTGSJointMode);
const StepCount=480;
var Physics:TKraft;
    AnchorBody,BoxBody:TKraftRigidBody;
    Joint:TKraftConstraintJoint6DOF;
    StepIndex:longint;
    EndY,Lateral:TKraftScalar;
begin
 Physics:=CreatePhysics(aSolverMode,aJointMode);
 try
  AnchorBody:=MakeStaticAnchor(Physics,Vector3(0.0,2.0,0.0));

  BoxBody:=TKraftRigidBody.Create(Physics);
  BoxBody.SetRigidBodyType(krbtDYNAMIC);
  TKraftShapeBox.Create(Physics,BoxBody,Vector3(0.1,0.1,0.1));
  BoxBody.Finish;
  BoxBody.SetWorldTransformation(Matrix4x4Translate(0.0,2.0,0.0));
  BoxBody.CollisionGroups:=[0];

  // Vertical twist axis; axis 2 (= the twist axis) slides within +/-0.2 m, everything else stays locked, so
  // the box must settle hanging at the lower stop.
  Joint:=TKraftConstraintJoint6DOF.Create(Physics,AnchorBody,BoxBody,Vector3(0.0,2.0,0.0),Vector3(0.0,1.0,0.0));
  Joint.SetLinearAxisMode(2,k6damLimited,-0.2,0.2);

  for StepIndex:=1 to StepCount do begin
   Physics.Step(1.0/120.0);
  end;

  EndY:=BoxBody.Sweep.c.y;
  Lateral:=sqrt(sqr(BoxBody.Sweep.c.x)+sqr(BoxBody.Sweep.c.z));
  Check(aName+' prislim ','endY',EndY,(abs(EndY-1.8)<0.05) and (Lateral<0.02));
 finally
  Physics.Free;
 end;
end;

procedure RunPrismaticFree(const aName:string;const aSolverMode:TKraftSolverMode;const aJointMode:TKraftTGSJointMode);
const StepCount=60;
var Physics:TKraft;
    AnchorBody,BoxBody:TKraftRigidBody;
    Joint:TKraftConstraintJoint6DOF;
    StepIndex:longint;
    EndY,Lateral:TKraftScalar;
begin
 Physics:=CreatePhysics(aSolverMode,aJointMode);
 try
  AnchorBody:=MakeStaticAnchor(Physics,Vector3(0.0,2.0,0.0));

  BoxBody:=TKraftRigidBody.Create(Physics);
  BoxBody.SetRigidBodyType(krbtDYNAMIC);
  TKraftShapeBox.Create(Physics,BoxBody,Vector3(0.1,0.1,0.1));
  BoxBody.Finish;
  BoxBody.SetWorldTransformation(Matrix4x4Translate(0.0,2.0,0.0));
  BoxBody.CollisionGroups:=[0];

  // Axis 2 free: the box must fall freely along the vertical twist axis while the locked axes keep it on rail.
  Joint:=TKraftConstraintJoint6DOF.Create(Physics,AnchorBody,BoxBody,Vector3(0.0,2.0,0.0),Vector3(0.0,1.0,0.0));
  Joint.SetLinearAxisMode(2,k6damFree);

  for StepIndex:=1 to StepCount do begin
   Physics.Step(1.0/120.0);
  end;

  EndY:=BoxBody.Sweep.c.y;
  Lateral:=sqrt(sqr(BoxBody.Sweep.c.x)+sqr(BoxBody.Sweep.c.z));
  Check(aName+' prisfree','endY',EndY,(EndY<1.3) and (Lateral<0.05));
 finally
  Physics.Free;
 end;
end;

procedure RunEllipse(const aName:string;const aSolverMode:TKraftSolverMode;const aJointMode:TKraftTGSJointMode;
                     const aSwingReferenceAxis:TKraftVector3;const aSuffix:string;const aExpectedCatchAngle:TKraftScalar);
const StepCount=480;
var Physics:TKraft;
    AnchorBody,ArmBody:TKraftRigidBody;
    Shape:TKraftShapeCapsule;
    Joint:TKraftConstraintJoint6DOF;
    SetupOrientation:TKraftQuaternion;
    StepIndex:longint;
    Swing,MaxSwing,AngVel,Peak:TKraftScalar;
begin
 Physics:=CreatePhysics(aSolverMode,aJointMode);
 try
  AnchorBody:=MakeStaticAnchor(Physics,Vector3(0.0,2.0,0.0));

  ArmBody:=TKraftRigidBody.Create(Physics);
  ArmBody.SetRigidBodyType(krbtDYNAMIC);
  Shape:=TKraftShapeCapsule.Create(Physics,ArmBody,0.045,0.30);
  Shape.Density:=1.0;
  ArmBody.Finish;
  ArmBody.SetWorldTransformation(Matrix4x4TermMul(Matrix4x4RotateZ(0.5*pi),Matrix4x4Translate(0.20,2.0,0.0)));
  ArmBody.CollisionGroups:=[0];
  SetupOrientation:=ArmBody.Sweep.q;

  // Elliptical cone with a narrow 0.2 rad half angle about frame x and a wide 0.6 rad one about frame y.
  // The falling arm always swings about the world z axis; the swing reference axis aims the frame x axis, so
  // it selects which of the two half angles catches the fall.
  Joint:=TKraftConstraintJoint6DOF.Create(Physics,AnchorBody,ArmBody,Vector3(0.0,2.0,0.0),Vector3(1.0,0.0,0.0),aSwingReferenceAxis);
  Joint.SwingMode:=k6damLimited;
  Joint.ConeHalfAngleX:=0.2;
  Joint.ConeHalfAngleY:=0.6;

  MaxSwing:=0.0;
  Peak:=0.0;
  for StepIndex:=1 to StepCount do begin
   Physics.Step(1.0/120.0);
   Swing:=AxisDeviation(ArmBody,SetupOrientation,Vector3(1.0,0.0,0.0));
   if Swing>MaxSwing then begin
    MaxSwing:=Swing;
   end;
   AngVel:=Vector3Length(ArmBody.AngularVelocity);
   if AngVel>Peak then begin
    Peak:=AngVel;
   end;
  end;
  Check(aName+' '+aSuffix,'maxSwing',MaxSwing,(MaxSwing<(aExpectedCatchAngle+0.12)) and (MaxSwing>(aExpectedCatchAngle-0.08)) and (Peak>0.5));
 finally
  Physics.Free;
 end;
end;

procedure RunSlerpHold(const aName:string;const aSolverMode:TKraftSolverMode;const aJointMode:TKraftTGSJointMode);
const StepCount=480;
var Physics:TKraft;
    AnchorBody,ArmBody:TKraftRigidBody;
    Shape:TKraftShapeCapsule;
    Joint:TKraftConstraintJoint6DOF;
    SetupOrientation:TKraftQuaternion;
    StepIndex:longint;
    QuatDot,OrientationDrift:TKraftScalar;
begin
 Physics:=CreatePhysics(aSolverMode,aJointMode);
 try
  AnchorBody:=MakeStaticAnchor(Physics,Vector3(0.0,2.0,0.0));

  ArmBody:=TKraftRigidBody.Create(Physics);
  ArmBody.SetRigidBodyType(krbtDYNAMIC);
  Shape:=TKraftShapeCapsule.Create(Physics,ArmBody,0.045,0.30);
  Shape.Density:=1.0;
  ArmBody.Finish;
  ArmBody.SetWorldTransformation(Matrix4x4TermMul(Matrix4x4RotateZ(0.5*pi),Matrix4x4Translate(0.20,2.0,0.0)));
  ArmBody.CollisionGroups:=[0];
  SetupOrientation:=ArmBody.Sweep.q;

  // Rotation completely free; only the slerp drive (target = the setup pose) holds the horizontal arm up
  // against gravity, where without it the arm falls (see the free scenario).
  Joint:=TKraftConstraintJoint6DOF.Create(Physics,AnchorBody,ArmBody,Vector3(0.0,2.0,0.0),Vector3(1.0,0.0,0.0));
  Joint.SwingMode:=k6damFree;
  Joint.TwistMode:=k6damFree;
  Joint.EnableSlerpDrive:=true;
  Joint.SlerpDriveFrequencyHz:=10.0;
  Joint.SlerpDriveDampingRatio:=1.0;

  for StepIndex:=1 to StepCount do begin
   Physics.Step(1.0/120.0);
  end;

  QuatDot:=abs((ArmBody.Sweep.q.x*SetupOrientation.x)+(ArmBody.Sweep.q.y*SetupOrientation.y)+(ArmBody.Sweep.q.z*SetupOrientation.z)+(ArmBody.Sweep.q.w*SetupOrientation.w));
  OrientationDrift:=2.0*ArcCos(Min(QuatDot,1.0));
  Check(aName+' slerphold','sag',OrientationDrift,OrientationDrift<0.2);
 finally
  Physics.Free;
 end;
end;

procedure RunSlerpTarget(const aName:string;const aSolverMode:TKraftSolverMode;const aJointMode:TKraftTGSJointMode);
const StepCount=480;
var Physics:TKraft;
    AnchorBody,ArmBody:TKraftRigidBody;
    Shape:TKraftShapeCapsule;
    Joint:TKraftConstraintJoint6DOF;
    SetupOrientation:TKraftQuaternion;
    StepIndex:longint;
    EndAngle:TKraftScalar;
begin
 Physics:=CreatePhysics(aSolverMode,aJointMode);
 try
  AnchorBody:=MakeStaticAnchor(Physics,Vector3(0.0,2.0,0.0));

  ArmBody:=TKraftRigidBody.Create(Physics);
  ArmBody.SetRigidBodyType(krbtDYNAMIC);
  Shape:=TKraftShapeCapsule.Create(Physics,ArmBody,0.045,0.30);
  Shape.Density:=1.0;
  ArmBody.Finish;
  ArmBody.SetWorldTransformation(Matrix4x4Translate(0.0,1.80,0.0));
  ArmBody.CollisionGroups:=[0];
  SetupOrientation:=ArmBody.Sweep.q;

  // Hanging arm with world-aligned frame; the drive must swing it to +90 degrees about z and hold it there,
  // which also validates the sign chain of the drive error.
  Joint:=TKraftConstraintJoint6DOF.Create(Physics,AnchorBody,ArmBody,Vector3(0.0,2.0,0.0),Vector3(0.0,0.0,1.0),Vector3(1.0,0.0,0.0));
  Joint.SwingMode:=k6damFree;
  Joint.TwistMode:=k6damFree;
  Joint.EnableSlerpDrive:=true;
  Joint.SlerpDriveFrequencyHz:=10.0;
  Joint.SlerpDriveDampingRatio:=1.0;
  Joint.SlerpDriveTarget:=QuaternionFromAxisAngle(Vector3(0.0,0.0,1.0),0.5*pi);

  for StepIndex:=1 to StepCount do begin
   Physics.Step(1.0/120.0);
  end;

  EndAngle:=TwistAbout(ArmBody,SetupOrientation,Vector3(0.0,0.0,1.0));
  Check(aName+' slerptgt ','endAngle',EndAngle,abs(EndAngle-(0.5*pi))<0.25);
 finally
  Physics.Free;
 end;
end;

procedure RunFriction(const aName:string;const aSolverMode:TKraftSolverMode;const aJointMode:TKraftTGSJointMode);
const StepCount=480;
      StartSpin=6.0;
var Physics:TKraft;
    AnchorBody,ArmBody:TKraftRigidBody;
    Shape:TKraftShapeCapsule;
    Joint:TKraftConstraintJoint6DOF;
    StepIndex:longint;
    InertiaY,FrictionTorque,EndSpin:TKraftScalar;
begin
 Physics:=CreatePhysics(aSolverMode,aJointMode);
 try
  AnchorBody:=MakeStaticAnchor(Physics,Vector3(0.0,2.0,0.0));

  ArmBody:=TKraftRigidBody.Create(Physics);
  ArmBody.SetRigidBodyType(krbtDYNAMIC);
  Shape:=TKraftShapeCapsule.Create(Physics,ArmBody,0.045,0.30);
  Shape.Density:=1.0;
  ArmBody.Finish;
  ArmBody.SetWorldTransformation(Matrix4x4Translate(0.0,1.80,0.0));
  ArmBody.CollisionGroups:=[0];
  ArmBody.LinearVelocityDamp:=0.0;
  ArmBody.AngularVelocityDamp:=0.0;

  // Joint friction = angular velocity drive with target zero and a torque budget calibrated so a constant
  // braking torque halves the spin over the run; ending near half proves the torque clamp really clamps.
  InertiaY:=ArmBody.BodyInertiaTensor[1,1];
  FrictionTorque:=(0.5*InertiaY*StartSpin)/(StepCount/120.0);
  Joint:=TKraftConstraintJoint6DOF.Create(Physics,AnchorBody,ArmBody,Vector3(0.0,2.0,0.0),Vector3(0.0,1.0,0.0));
  Joint.TwistMode:=k6damFree;
  Joint.EnableAngularVelocityDrive:=true;
  Joint.TargetAngularVelocity:=Vector3Origin;
  Joint.MaximalAngularDriveTorque:=FrictionTorque;

  ArmBody.AngularVelocity:=Vector3(0.0,StartSpin,0.0);

  for StepIndex:=1 to StepCount do begin
   Physics.Step(1.0/120.0);
  end;

  EndSpin:=ArmBody.AngularVelocity.y;
  Check(aName+' friction ','endSpin',EndSpin,(EndSpin>(0.3*StartSpin)) and (EndSpin<(0.7*StartSpin)));
 finally
  Physics.Free;
 end;
end;

procedure RunLinearDrive(const aName:string;const aSolverMode:TKraftSolverMode;const aJointMode:TKraftTGSJointMode);
const StepCount=120;
var Physics:TKraft;
    AnchorBody,BoxBody:TKraftRigidBody;
    Joint:TKraftConstraintJoint6DOF;
    StepIndex:longint;
    EndVelocityY:TKraftScalar;
begin
 Physics:=CreatePhysics(aSolverMode,aJointMode);
 try
  AnchorBody:=MakeStaticAnchor(Physics,Vector3(0.0,2.0,0.0));

  BoxBody:=TKraftRigidBody.Create(Physics);
  BoxBody.SetRigidBodyType(krbtDYNAMIC);
  TKraftShapeBox.Create(Physics,BoxBody,Vector3(0.1,0.1,0.1));
  BoxBody.Finish;
  BoxBody.SetWorldTransformation(Matrix4x4Translate(0.0,2.0,0.0));
  BoxBody.CollisionGroups:=[0];
  BoxBody.LinearVelocityDamp:=0.0;
  BoxBody.AngularVelocityDamp:=0.0;

  // Free vertical rail; the linear velocity drive must push the box upward at 1 m/s against gravity.
  Joint:=TKraftConstraintJoint6DOF.Create(Physics,AnchorBody,BoxBody,Vector3(0.0,2.0,0.0),Vector3(0.0,1.0,0.0));
  Joint.SetLinearAxisMode(2,k6damFree);
  Joint.EnableLinearVelocityDrive:=true;
  Joint.TargetLinearVelocity:=Vector3(0.0,0.0,1.0);

  for StepIndex:=1 to StepCount do begin
   Physics.Step(1.0/120.0);
  end;

  EndVelocityY:=BoxBody.LinearVelocity.y;
  Check(aName+' lindrive ','endVelY',EndVelocityY,(abs(EndVelocityY-1.0)<0.15) and (BoxBody.Sweep.c.y>2.5));
 finally
  Physics.Free;
 end;
end;

procedure RunSoftCone(const aName:string;const aSolverMode:TKraftSolverMode;const aJointMode:TKraftTGSJointMode);
const StepCount=480;
var Physics:TKraft;
    AnchorBody,ArmBody:TKraftRigidBody;
    Shape:TKraftShapeCapsule;
    Joint:TKraftConstraintJoint6DOF;
    SetupOrientation:TKraftQuaternion;
    StepIndex:longint;
    Swing,MaxSwing:TKraftScalar;
begin
 Physics:=CreatePhysics(aSolverMode,aJointMode);
 try
  AnchorBody:=MakeStaticAnchor(Physics,Vector3(0.0,2.0,0.0));

  ArmBody:=TKraftRigidBody.Create(Physics);
  ArmBody.SetRigidBodyType(krbtDYNAMIC);
  Shape:=TKraftShapeCapsule.Create(Physics,ArmBody,0.045,0.30);
  Shape.Density:=1.0;
  ArmBody.Finish;
  ArmBody.SetWorldTransformation(Matrix4x4TermMul(Matrix4x4RotateZ(0.5*pi),Matrix4x4Translate(0.20,2.0,0.0)));
  ArmBody.CollisionGroups:=[0];
  SetupOrientation:=ArmBody.Sweep.q;

  // Same setup as the hard cone scenario (which stops dead at 0.5000), but as a soft limit: the falling arm
  // must visibly push past the 0.5 rad boundary against the limit spring and still be caught.
  Joint:=TKraftConstraintJoint6DOF.Create(Physics,AnchorBody,ArmBody,Vector3(0.0,2.0,0.0),Vector3(1.0,0.0,0.0));
  Joint.SwingMode:=k6damLimited;
  Joint.ConeHalfAngle:=0.5;
  Joint.SwingLimitFrequencyHz:=6.0;
  Joint.SwingLimitDampingRatio:=0.7;

  MaxSwing:=0.0;
  for StepIndex:=1 to StepCount do begin
   Physics.Step(1.0/120.0);
   Swing:=AxisDeviation(ArmBody,SetupOrientation,Vector3(1.0,0.0,0.0));
   if Swing>MaxSwing then begin
    MaxSwing:=Swing;
   end;
  end;
  Check(aName+' softcone ','maxSwing',MaxSwing,(MaxSwing>0.55) and (MaxSwing<1.2));
 finally
  Physics.Free;
 end;
end;

procedure RunSoftTwist(const aName:string;const aSolverMode:TKraftSolverMode;const aJointMode:TKraftTGSJointMode);
const StepCount=480;
var Physics:TKraft;
    AnchorBody,ArmBody:TKraftRigidBody;
    Shape:TKraftShapeCapsule;
    Joint:TKraftConstraintJoint6DOF;
    SetupOrientation:TKraftQuaternion;
    StepIndex:longint;
    Twist,MaxTwist:TKraftScalar;
begin
 Physics:=CreatePhysics(aSolverMode,aJointMode);
 try
  AnchorBody:=MakeStaticAnchor(Physics,Vector3(0.0,2.0,0.0));

  ArmBody:=TKraftRigidBody.Create(Physics);
  ArmBody.SetRigidBodyType(krbtDYNAMIC);
  Shape:=TKraftShapeCapsule.Create(Physics,ArmBody,0.045,0.30);
  Shape.Density:=1.0;
  ArmBody.Finish;
  ArmBody.SetWorldTransformation(Matrix4x4Translate(0.0,1.80,0.0));
  ArmBody.CollisionGroups:=[0];
  SetupOrientation:=ArmBody.Sweep.q;

  // Same setup as the hard twist scenario (which stops dead at 0.3000), but as a soft limit: the 5 rad/s
  // spin must visibly wind past the 0.3 rad boundary against the limit spring and still be caught.
  Joint:=TKraftConstraintJoint6DOF.Create(Physics,AnchorBody,ArmBody,Vector3(0.0,2.0,0.0),Vector3(0.0,1.0,0.0));
  Joint.TwistMode:=k6damLimited;
  Joint.LowerTwistAngle:=-0.3;
  Joint.UpperTwistAngle:=0.3;
  Joint.TwistLimitFrequencyHz:=3.0;
  Joint.TwistLimitDampingRatio:=0.7;

  ArmBody.AngularVelocity:=Vector3(0.0,5.0,0.0);

  MaxTwist:=0.0;
  for StepIndex:=1 to StepCount do begin
   Physics.Step(1.0/120.0);
   Twist:=abs(TwistAbout(ArmBody,SetupOrientation,Vector3(0.0,1.0,0.0)));
   if Twist>MaxTwist then begin
    MaxTwist:=Twist;
   end;
  end;
  Check(aName+' softtwist','maxTwist',MaxTwist,(MaxTwist>0.35) and (MaxTwist<1.5));
 finally
  Physics.Free;
 end;
end;

procedure RunSoftLinear(const aName:string;const aSolverMode:TKraftSolverMode;const aJointMode:TKraftTGSJointMode);
const StepCount=480;
var Physics:TKraft;
    AnchorBody,BoxBody:TKraftRigidBody;
    Joint:TKraftConstraintJoint6DOF;
    StepIndex:longint;
    EndY:TKraftScalar;
begin
 Physics:=CreatePhysics(aSolverMode,aJointMode);
 try
  AnchorBody:=MakeStaticAnchor(Physics,Vector3(0.0,2.0,0.0));

  BoxBody:=TKraftRigidBody.Create(Physics);
  BoxBody.SetRigidBodyType(krbtDYNAMIC);
  TKraftShapeBox.Create(Physics,BoxBody,Vector3(0.1,0.1,0.1));
  BoxBody.Finish;
  BoxBody.SetWorldTransformation(Matrix4x4Translate(0.0,2.0,0.0));
  BoxBody.CollisionGroups:=[0];

  // Same rail as the hard prismatic scenario (which rests exactly at 1.8000), but with a 2 Hz soft lower
  // limit: gravity presses the box past the stop by roughly g/omega^2 ~ 0.06 m into the limit spring.
  Joint:=TKraftConstraintJoint6DOF.Create(Physics,AnchorBody,BoxBody,Vector3(0.0,2.0,0.0),Vector3(0.0,1.0,0.0));
  Joint.SetLinearAxisMode(2,k6damLimited,-0.2,0.2,2.0,1.0);

  for StepIndex:=1 to StepCount do begin
   Physics.Step(1.0/120.0);
  end;

  EndY:=BoxBody.Sweep.c.y;
  Check(aName+' softlin  ','endY',EndY,(EndY<1.79) and (EndY>1.65));
 finally
  Physics.Free;
 end;
end;

procedure RunLinearPositionDrive(const aName:string;const aSolverMode:TKraftSolverMode;const aJointMode:TKraftTGSJointMode);
const StepCount=480;
var Physics:TKraft;
    AnchorBody,BoxBody:TKraftRigidBody;
    Joint:TKraftConstraintJoint6DOF;
    StepIndex:longint;
    EndY,EndVelocityY:TKraftScalar;
begin
 Physics:=CreatePhysics(aSolverMode,aJointMode);
 try
  AnchorBody:=MakeStaticAnchor(Physics,Vector3(0.0,2.0,0.0));

  BoxBody:=TKraftRigidBody.Create(Physics);
  BoxBody.SetRigidBodyType(krbtDYNAMIC);
  TKraftShapeBox.Create(Physics,BoxBody,Vector3(0.1,0.1,0.1));
  BoxBody.Finish;
  BoxBody.SetWorldTransformation(Matrix4x4Translate(0.0,2.0,0.0));
  BoxBody.CollisionGroups:=[0];

  // Free vertical rail; the linear position drive must let the box settle hovering at 0.5 m below the
  // anchor (minus the static spring sag of roughly g/omega^2 ~ 0.016 m), held purely by the drive spring.
  Joint:=TKraftConstraintJoint6DOF.Create(Physics,AnchorBody,BoxBody,Vector3(0.0,2.0,0.0),Vector3(0.0,1.0,0.0));
  Joint.SetLinearAxisMode(2,k6damFree);
  Joint.EnableLinearPositionDrive:=true;
  Joint.LinearPositionDriveTarget:=Vector3(0.0,0.0,-0.5);
  Joint.LinearPositionDriveFrequencyHz:=4.0;
  Joint.LinearPositionDriveDampingRatio:=1.0;

  for StepIndex:=1 to StepCount do begin
   Physics.Step(1.0/120.0);
  end;

  EndY:=BoxBody.Sweep.c.y;
  EndVelocityY:=BoxBody.LinearVelocity.y;
  Check(aName+' linposdrv','endY',EndY,(abs(EndY-1.4845)<0.05) and (abs(EndVelocityY)<0.1));
 finally
  Physics.Free;
 end;
end;

procedure RunAll(const aName:string;const aSolverMode:TKraftSolverMode;const aJointMode:TKraftTGSJointMode);
begin
 RunWeld(aName,aSolverMode,aJointMode);
 RunFreePendulum(aName,aSolverMode,aJointMode);
 RunConeLimit(aName,aSolverMode,aJointMode);
 RunTwistLimit(aName,aSolverMode,aJointMode);
 RunSpinFree(aName,aSolverMode,aJointMode);
 RunPrismaticLimited(aName,aSolverMode,aJointMode);
 RunPrismaticFree(aName,aSolverMode,aJointMode);
 RunEllipse(aName,aSolverMode,aJointMode,Vector3(0.0,0.0,1.0),'ellnarrow',0.2);
 RunEllipse(aName,aSolverMode,aJointMode,Vector3(0.0,1.0,0.0),'ellwide  ',0.6);
 RunSlerpHold(aName,aSolverMode,aJointMode);
 RunSlerpTarget(aName,aSolverMode,aJointMode);
 RunFriction(aName,aSolverMode,aJointMode);
 RunLinearDrive(aName,aSolverMode,aJointMode);
 RunSoftCone(aName,aSolverMode,aJointMode);
 RunSoftTwist(aName,aSolverMode,aJointMode);
 RunSoftLinear(aName,aSolverMode,aJointMode);
 RunLinearPositionDrive(aName,aSolverMode,aJointMode);
end;

begin
 FormatSettings.DecimalSeparator:='.';
 RunAll('SI        ',ksmSequentialImpulse,ktjmAdapter);
 RunAll('TGS-ADP   ',ksmTGSSoft,ktjmAdapter);
 RunAll('TGS-ADPSUB',ksmTGSSoft,ktjmAdapterSubstep);
 RunAll('TGS-NAT   ',ksmTGSSoft,ktjmNativeSoft);
 WriteLn;
 if FailureCount=0 then begin
  WriteLn('ALL PASS');
 end else begin
  WriteLn('FAILURES: ',FailureCount);
 end;
end.
