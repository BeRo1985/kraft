program seattest;
{$ifdef fpc}
 {$mode delphi}
{$endif}
{$apptype console}
uses {$ifdef unix}cthreads,{$endif} SysUtils,kraft;
var Physics:TKraft;
    Body:TKraftRigidBody;
    ShapeBox:TKraftShapeBox;
    M:TKraftMatrix3x3;
    i:longint;
    Det:TKraftScalar;
begin
 FormatSettings.DecimalSeparator:='.';
 Physics:=TKraft.Create(-1);
 Body:=TKraftRigidBody.Create(Physics);
 Body.SetRigidBodyType(krbtDYNAMIC);
 ShapeBox:=TKraftShapeBox.Create(Physics,Body,Vector3(0.25,0.0625,0.25));
 ShapeBox.Finish;
 ShapeBox:=TKraftShapeBox.Create(Physics,Body,Vector3(0.0625,0.25,0.25));
 ShapeBox.LocalTransform:=Matrix4x4Translate(0.0625-0.25,0.0625+0.25,0.0);
 ShapeBox.Finish;
 ShapeBox:=TKraftShapeBox.Create(Physics,Body,Vector3(0.25,0.0625,0.03125));
 ShapeBox.LocalTransform:=Matrix4x4Translate(0.0,0.0625+0.0625,0.03125-0.25);
 ShapeBox.Finish;
 ShapeBox:=TKraftShapeBox.Create(Physics,Body,Vector3(0.25,0.0625,0.03125));
 ShapeBox.LocalTransform:=Matrix4x4Translate(0.0,0.0625+0.0625,-(0.03125-0.25));
 ShapeBox.Finish;
 Body.ForcedMass:=5.0;
 Body.Finish;
 M:=Body.BodyInertiaTensor;
 WriteLn('Seat: mass=',Body.Mass:0:4);
 for i:=0 to 2 do begin
  WriteLn('  I: ',M[i,0]:12:8,' ',M[i,1]:12:8,' ',M[i,2]:12:8);
 end;
 Det:=((M[0,0]*((M[1,1]*M[2,2])-(M[2,1]*M[1,2])))-(M[0,1]*((M[1,0]*M[2,2])-(M[2,0]*M[1,2]))))+(M[0,2]*((M[1,0]*M[2,1])-(M[2,0]*M[1,1])));
 WriteLn('  det=',Det,'  (altes Epsilon 1e-5: Fallback=',Det<1e-5,')');
 M:=Body.BodyInverseInertiaTensor;
 for i:=0 to 2 do begin
  WriteLn('  Iinv: ',M[i,0]:12:6,' ',M[i,1]:12:6,' ',M[i,2]:12:6);
 end;
 Physics.Free;
end.
