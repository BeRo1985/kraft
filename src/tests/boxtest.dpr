program boxtest;
{$ifdef fpc}
 {$mode delphi}
{$endif}
{$apptype console}
uses {$ifdef unix}cthreads,{$endif} SysUtils,Math,kraft;

procedure RunOne(const aName:string;const aSolverMode:TKraftSolverMode;const aExtents:TKraftVector3;const aTilted:boolean);
var Physics:TKraft;
    FloorBody,Body:TKraftRigidBody;
    FloorShape:TKraftShapePlane;
    Shape:TKraftShapeBox;
    StepIndex:longint;
    AngVel,LinVel,LateMaxAng,LateMaxLin,PeakAng:TKraftScalar;
begin
 Physics:=TKraft.Create(-1);
 try
  Physics.SetFrequency(120.0);
  Physics.VelocityIterations:=8;
  Physics.PositionIterations:=3;
  Physics.Gravity.y:=-9.81;
  Physics.SolverMode:=aSolverMode;
  Physics.TGSJointMode:=ktjmNativeSoft;
  FloorBody:=TKraftRigidBody.Create(Physics);
  FloorBody.SetRigidBodyType(krbtSTATIC);
  FloorShape:=TKraftShapePlane.Create(Physics,FloorBody,Plane(Vector3Norm(Vector3(0.0,1.0,0.0)),0.0));
  FloorShape.Restitution:=0.3;
  FloorBody.Finish;
  FloorBody.CollisionGroups:=[0];
  Body:=TKraftRigidBody.Create(Physics);
  Body.SetRigidBodyType(krbtDYNAMIC);
  Shape:=TKraftShapeBox.Create(Physics,Body,aExtents);
  Shape.Restitution:=0.1;
  Shape.Density:=1.0;
  Body.Finish;
  if aTilted then begin
   Body.SetWorldTransformation(Matrix4x4TermMul(Matrix4x4RotateZ(0.2),Matrix4x4Translate(0.0,aExtents.y*3.0,0.0)));
  end else begin
   Body.SetWorldTransformation(Matrix4x4Translate(0.0,aExtents.y*3.0,0.0));
  end;
  Body.CollisionGroups:=[0];
  LateMaxAng:=0.0;
  LateMaxLin:=0.0;
  PeakAng:=0.0;
  for StepIndex:=1 to 1200 do begin
   Physics.Step(1.0/120.0);
   AngVel:=Vector3Length(Body.AngularVelocity);
   LinVel:=Vector3Length(Body.LinearVelocity);
   PeakAng:=Max(PeakAng,AngVel);
   if StepIndex>600 then begin
    LateMaxAng:=Max(LateMaxAng,AngVel);
    LateMaxLin:=Max(LateMaxLin,LinVel);
   end;
  end;
  WriteLn(aName,': peak=',PeakAng:6:2,' late maxAng=',LateMaxAng:8:3,' maxLin=',LateMaxLin:8:3);
 finally
  Physics.Free;
 end;
end;

begin
 FormatSettings.DecimalSeparator:='.';
 RunOne('SI  small box (pelvis) flat   ',ksmSequentialImpulse,Vector3(0.18,0.12,0.12),false);
 RunOne('SI  small box (pelvis) tilted ',ksmSequentialImpulse,Vector3(0.18,0.12,0.12),true);
 RunOne('SI  large box 1x1x1 tilted    ',ksmSequentialImpulse,Vector3(1.0,1.0,1.0),true);
 RunOne('TGS small box (pelvis) tilted ',ksmTGSSoft,Vector3(0.18,0.12,0.12),true);
 RunOne('TGS large box 1x1x1 tilted    ',ksmTGSSoft,Vector3(1.0,1.0,1.0),true);
end.
