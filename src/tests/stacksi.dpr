program stacksi;

{$ifdef fpc}
 {$mode delphi}
{$endif}
{$apptype console}

uses {$ifdef unix}cthreads,{$endif} SysUtils,Math,kraft;

// Reproduces the sandbox box stacking scene exactly (ten 0.5 m boxes, density 100, restitution 0.4) and
// watches whether the tower stays up, in both solver modes. Also dumps the box inertia against the analytic
// value, since wrong inertia tensors (the known FPC code generator trap in the body setup path) would topple
// stacks in every solver mode at once.

procedure RunStack(const aName:string);
const StepCount=1800;
var Physics:TKraft;
    FloorBody:TKraftRigidBody;
    FloorShape:TKraftShapePlane;
    BoxBody:TKraftRigidBody;
    ShapeBox:TKraftShapeBox;
    Boxes:array[0..9] of TKraftRigidBody;
    StepIndex,BoxIndex,StandingCount:longint;
    MaxDrift,Drift,TopY:TKraftScalar;
    AllAsleep:boolean;
begin
 Physics:=TKraft.Create(-1);
 try
  Physics.SetFrequency(120.0);
  Physics.VelocityIterations:=8;
  Physics.PositionIterations:=3;
  Physics.SpeculativeIterations:=8;
  Physics.TimeOfImpactIterations:=20;
  Physics.Gravity.y:=-9.81;

  FloorBody:=TKraftRigidBody.Create(Physics);
  FloorBody.SetRigidBodyType(krbtSTATIC);
  FloorShape:=TKraftShapePlane.Create(Physics,FloorBody,Plane(Vector3Norm(Vector3(0.0,1.0,0.0)),0.0));
  FloorShape.Restitution:=0.4;
  FloorBody.Finish;
  FloorBody.SetWorldTransformation(Matrix4x4Translate(0.0,0.0,0.0));
  FloorBody.CollisionGroups:=[0];

  for BoxIndex:=0 to 9 do begin
   BoxBody:=TKraftRigidBody.Create(Physics);
   BoxBody.SetRigidBodyType(krbtDYNAMIC);
   ShapeBox:=TKraftShapeBox.Create(Physics,BoxBody,Vector3(0.25,0.25,0.25));
   ShapeBox.Restitution:=0.4;
   ShapeBox.Density:=100.0;
   BoxBody.Finish;
   BoxBody.SetWorldTransformation(Matrix4x4Translate(0.0,ShapeBox.Extents.y+(BoxIndex*(ShapeBox.Extents.y*2.0)),0.0));
   BoxBody.CollisionGroups:=[0];
   Boxes[BoxIndex]:=BoxBody;
  end;

  // Analytic check for the bottom box: m = 0.5^3 * 100 = 12.5 kg, I = m*(0.5^2+0.5^2)/12 = 0.5208,
  // so the inverse inertia diagonal should be about 1.92.
  WriteLn(aName,' box mass=',Boxes[0].Mass:8:4,' bodyInvInertia diag=(',
          Boxes[0].BodyInverseInertiaTensor[0,0]:8:4,',',
          Boxes[0].BodyInverseInertiaTensor[1,1]:8:4,',',
          Boxes[0].BodyInverseInertiaTensor[2,2]:8:4,')');

  MaxDrift:=0.0;
  for StepIndex:=1 to StepCount do begin
   Physics.Step(1.0/120.0);
   Drift:=sqrt(sqr(Boxes[9].Sweep.c.x)+sqr(Boxes[9].Sweep.c.z));
   if Drift>MaxDrift then begin
    MaxDrift:=Drift;
   end;
  end;

  StandingCount:=0;
  for BoxIndex:=0 to 9 do begin
   if abs(Boxes[BoxIndex].Sweep.c.y-(0.25+(BoxIndex*0.5)))<0.1 then begin
    inc(StandingCount);
   end;
  end;
  TopY:=Boxes[9].Sweep.c.y;
  AllAsleep:=true;
  for BoxIndex:=0 to 9 do begin
   if krbfAwake in Boxes[BoxIndex].Flags then begin
    AllAsleep:=false;
    break;
   end;
  end;
  WriteLn(aName,': standing=',StandingCount,'/10 topY=',TopY:7:3,' maxTopDrift=',MaxDrift:7:3,' allAsleep=',AllAsleep);
 finally
  Physics.Free;
 end;
end;

begin
 FormatSettings.DecimalSeparator:='.';
 RunStack('SI     ');
end.
