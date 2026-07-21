program tiptest;

{$ifdef fpc}
 {$mode delphi}
{$endif}
{$apptype console}

uses {$ifdef unix}cthreads,{$endif} SysUtils,Math,kraft;

// Measures how deep the corners of a fast-spinning flat box sink into the floor plane while it slams down
// and tips over. The rotational part of the contact separation change within a step is exactly what the
// delta-rotation term in the substep separation formula captures, so this is the before/after gauge for it.

procedure RunTip(const aName:string;const aSolverMode:TKraftSolverMode;const aSpin:TKraftScalar);
const StepCount=360;
      ExtentX=0.25;
      ExtentY=0.05;
      ExtentZ=0.25;
var Physics:TKraft;
    FloorBody,BoxBody:TKraftRigidBody;
    FloorShape:TKraftShapePlane;
    BoxShape:TKraftShapeBox;
    StepIndex,CornerIndex:longint;
    Transform:TKraftMatrix4x4;
    Corner:TKraftVector3;
    CornerY,MinCornerY,MaxPenetration,LatePenetration:TKraftScalar;
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
  FloorShape.Restitution:=0.1;
  FloorBody.Finish;
  FloorBody.SetWorldTransformation(Matrix4x4Translate(0.0,0.0,0.0));
  FloorBody.CollisionGroups:=[0];

  BoxBody:=TKraftRigidBody.Create(Physics);
  BoxBody.SetRigidBodyType(krbtDYNAMIC);
  BoxShape:=TKraftShapeBox.Create(Physics,BoxBody,Vector3(ExtentX,ExtentY,ExtentZ));
  BoxShape.Restitution:=0.1;
  BoxShape.Density:=1.0;
  BoxBody.Finish;
  BoxBody.SetWorldTransformation(Matrix4x4Translate(0.0,0.3,0.0));
  BoxBody.CollisionGroups:=[0];
  BoxBody.AngularVelocity:=Vector3(aSpin,0.0,0.0);

  MaxPenetration:=0.0;
  LatePenetration:=0.0;
  for StepIndex:=1 to StepCount do begin
   Physics.Step(1.0/120.0);
   Transform:=BoxBody.WorldTransform;
   MinCornerY:=MAX_SCALAR;
   for CornerIndex:=0 to 7 do begin
    Corner:=Vector3TermMatrixMul(Vector3(ExtentX*(((CornerIndex and 1) shl 1)-1),
                                         ExtentY*((((CornerIndex shr 1) and 1) shl 1)-1),
                                         ExtentZ*((((CornerIndex shr 2) and 1) shl 1)-1)),Transform);
    CornerY:=Corner.y;
    if CornerY<MinCornerY then begin
     MinCornerY:=CornerY;
    end;
   end;
   if (-MinCornerY)>MaxPenetration then begin
    MaxPenetration:=-MinCornerY;
   end;
   if (StepIndex>240) and ((-MinCornerY)>LatePenetration) then begin
    LatePenetration:=-MinCornerY;
   end;
  end;
  WriteLn(aName,' spin=',aSpin:5:1,': maxPen=',MaxPenetration*1000.0:8:3,' mm  latePen=',LatePenetration*1000.0:8:3,' mm  endAngVel=',Vector3Length(BoxBody.AngularVelocity):6:3);
 finally
  Physics.Free;
 end;
end;

begin
 FormatSettings.DecimalSeparator:='.';
 RunTip('TGS',ksmTGSSoft,0.0);
 RunTip('TGS',ksmTGSSoft,12.0);
 RunTip('TGS',ksmTGSSoft,25.0);
 RunTip('SI ',ksmSequentialImpulse,0.0);
 RunTip('SI ',ksmSequentialImpulse,12.0);
 RunTip('SI ',ksmSequentialImpulse,25.0);
end.
