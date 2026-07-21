program hulltest;
{$ifdef fpc}
 {$mode delphi}
{$endif}
{$apptype console}
uses {$ifdef unix}cthreads,{$endif} SysUtils,kraft;
var Physics:TKraft;
    Body:TKraftRigidBody;
    Hull:TKraftConvexHull;
    Shape:TKraftShapeConvexHull;
    Index,i,j:longint;
    v:TKraftScalar;
    M,RInv,Product:TKraftMatrix3x3;
    ok:boolean;
begin
 FormatSettings.DecimalSeparator:='.';
 Physics:=TKraft.Create(-1);
 Body:=TKraftRigidBody.Create(Physics);
 Body.SetRigidBodyType(krbtDYNAMIC);
 Hull:=TKraftConvexHull.Create(Physics);
 for Index:=0 to 15 do begin
  v:=(Index/16)*(pi*2.0);
  Hull.AddVertex(Vector3(sin(v)*2.0,-0.125,cos(v)*2.0));
  Hull.AddVertex(Vector3(sin(v)*2.0,0.125,cos(v)*2.0));
 end;
 Hull.Build;
 Hull.Finish;
 Shape:=TKraftShapeConvexHull.Create(Physics,Body,Hull);
 Shape.Density:=1.0;
 Shape.Finish;
 Body.ForcedMass:=100.0;
 Body.Finish;
 WriteLn('mass=',Body.Mass:0:3);
 M:=Body.BodyInertiaTensor;
 WriteLn('inertia:');
 for i:=0 to 2 do begin
  WriteLn('  ',M[i,0]:12:6,' ',M[i,1]:12:6,' ',M[i,2]:12:6);
 end;
 RInv:=Body.BodyInverseInertiaTensor;
 WriteLn('robust inverse:');
 for i:=0 to 2 do begin
  WriteLn('  ',RInv[i,0]:12:6,' ',RInv[i,1]:12:6,' ',RInv[i,2]:12:6);
 end;
 Product:=Matrix3x3TermMul(M,RInv);
 WriteLn('M*Minv (should be identity):');
 for i:=0 to 2 do begin
  WriteLn('  ',Product[i,0]:12:6,' ',Product[i,1]:12:6,' ',Product[i,2]:12:6);
 end;
 Physics.Free;
end.
