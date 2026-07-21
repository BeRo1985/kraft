program capsuletest;
{$ifdef fpc}
 {$mode delphi}
{$endif}
{$apptype console}
uses {$ifdef unix}cthreads,{$endif} SysUtils,Math,kraft;

procedure RunOne(const aName:string;const aSolverMode:TKraftSolverMode;const aRadius,aHeight,aRestitution:TKraftScalar;const aRotated:boolean);
var Physics:TKraft;
    FloorBody,Body:TKraftRigidBody;
    FloorShape:TKraftShapePlane;
    Shape:TKraftShapeCapsule;
    StepIndex:longint;
    AngVel,LinVel,LateMaxAng,LateMaxLin:TKraftScalar;
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
  Shape:=TKraftShapeCapsule.Create(Physics,Body,aRadius,aHeight);
  Shape.Restitution:=aRestitution;
  Shape.Density:=1.0;
  Body.Finish;
  if aRotated then begin
   Body.SetWorldTransformation(Matrix4x4TermMul(Matrix4x4RotateZ(0.5*pi),Matrix4x4Translate(0.0,aRadius*4.0,0.0)));
  end else begin
   Body.SetWorldTransformation(Matrix4x4Translate(0.0,(aHeight*0.5)+(aRadius*2.0),0.0));
  end;
  Body.CollisionGroups:=[0];
  LateMaxAng:=0.0;
  LateMaxLin:=0.0;
  for StepIndex:=1 to 1200 do begin
   Physics.Step(1.0/120.0);
   if StepIndex>600 then begin
    AngVel:=Vector3Length(Body.AngularVelocity);
    LinVel:=Vector3Length(Body.LinearVelocity);
    LateMaxAng:=Max(LateMaxAng,AngVel);
    LateMaxLin:=Max(LateMaxLin,LinVel);
   end;
  end;
  WriteLn(aName,': late(5-10s) maxAng=',LateMaxAng:8:3,' maxLin=',LateMaxLin:8:3);
 finally
  Physics.Free;
 end;
end;

begin
 FormatSettings.DecimalSeparator:='.';
 RunOne('SI  small lying    r=0.045 rest=0.1',ksmSequentialImpulse,0.045,0.30,0.1,true);
 RunOne('SI  small standing r=0.045 rest=0.1',ksmSequentialImpulse,0.045,0.30,0.1,false);
 RunOne('SI  small lying    r=0.045 rest=0.0',ksmSequentialImpulse,0.045,0.30,0.0,true);
 RunOne('SI  large lying    r=0.5   rest=0.1',ksmSequentialImpulse,0.5,3.0,0.1,true);
 RunOne('TGS small lying    r=0.045 rest=0.1',ksmTGSSoft,0.045,0.30,0.1,true);
 RunOne('TGS small lying    r=0.045 rest=0.0',ksmTGSSoft,0.045,0.30,0.0,true);
 RunOne('TGS large lying    r=0.5   rest=0.1',ksmTGSSoft,0.5,3.0,0.1,true);
end.
