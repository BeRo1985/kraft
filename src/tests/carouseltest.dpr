program carouseltest;
{$ifdef fpc}
 {$mode delphi}
{$endif}
{$apptype console}
uses {$ifdef unix}cthreads,{$endif} SysUtils,Math,kraft;
var Physics:TKraft;
    BaseBody,TopBody:TKraftRigidBody;
    Hull:TKraftConvexHull;
    Shape:TKraftShapeConvexHull;
    Index,StepIndex:longint;
    v:TKraftScalar;
begin
 FormatSettings.DecimalSeparator:='.';
 Physics:=TKraft.Create(-1);
 Physics.SetFrequency(120.0);
 Physics.Gravity.y:=-9.81;
 BaseBody:=TKraftRigidBody.Create(Physics);
 BaseBody.SetRigidBodyType(krbtSTATIC);
 Hull:=TKraftConvexHull.Create(Physics);
 for Index:=0 to 15 do begin
  v:=(Index/16)*(pi*2.0);
  Hull.AddVertex(Vector3(sin(v)*1.0,-0.25,cos(v)*1.0));
  Hull.AddVertex(Vector3(sin(v)*1.0,0.25,cos(v)*1.0));
 end;
 Hull.Build; Hull.Finish;
 Shape:=TKraftShapeConvexHull.Create(Physics,BaseBody,Hull);
 BaseBody.Finish;
 BaseBody.SetWorldTransformation(Matrix4x4Translate(0.0,0.25,0.0));
 BaseBody.CollisionGroups:=[0];
 TopBody:=TKraftRigidBody.Create(Physics);
 TopBody.SetRigidBodyType(krbtDYNAMIC);
 Hull:=TKraftConvexHull.Create(Physics);
 for Index:=0 to 15 do begin
  v:=(Index/16)*(pi*2.0);
  Hull.AddVertex(Vector3(sin(v)*2.0,-0.125,cos(v)*2.0));
  Hull.AddVertex(Vector3(sin(v)*2.0,0.125,cos(v)*2.0));
 end;
 Hull.Build; Hull.Finish;
 Shape:=TKraftShapeConvexHull.Create(Physics,TopBody,Hull);
 Shape.Density:=1.0;
 Shape.Finish;
 TopBody.ForcedMass:=100.0;
 TopBody.Finish;
 TopBody.SetWorldTransformation(Matrix4x4Translate(0.0,0.625,0.0));
 TopBody.CollisionGroups:=[0];
 TKraftConstraintJointHinge.Create(Physics,BaseBody,TopBody,Vector3(0.0,0.5,0.0),Vector3(0.0,1.0,0.0));
 TopBody.AddBodyTorque(Vector3(0.0,3000.0,0.0));
 for StepIndex:=1 to 360 do begin
  Physics.Step(1.0/120.0);
  if (StepIndex mod 120)=0 then begin
   WriteLn('t=',(StepIndex/120.0):0:1,'s  wY=',TopBody.AngularVelocity.y:0:3,'  y=',TopBody.Sweep.c.y:0:3);
  end;
 end;
 Physics.Free;
end.
