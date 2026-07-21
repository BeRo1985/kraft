program masscheck;

{$ifdef fpc}
 {$mode delphi}
{$endif}
{$apptype console}

uses {$ifdef unix}cthreads,{$endif} SysUtils,Math,kraft;

// Cross-check: the same 0.5 m cube once as TKraftShapeBox and once as a convex hull built from its eight
// corners. The hull path integrates the geometry, so both must agree with the analytic values
// (m = 12.5 kg, inverse inertia diagonal = 1.92 at density 100).

var Physics:TKraft;
    BoxBody,HullBody:TKraftRigidBody;
    ShapeBox:TKraftShapeBox;
    ShapeHull:TKraftShapeConvexHull;
    Hull:TKraftConvexHull;
    CornerIndex:longint;
begin
 FormatSettings.DecimalSeparator:='.';
 Physics:=TKraft.Create(-1);
 try
  BoxBody:=TKraftRigidBody.Create(Physics);
  BoxBody.SetRigidBodyType(krbtDYNAMIC);
  ShapeBox:=TKraftShapeBox.Create(Physics,BoxBody,Vector3(0.25,0.25,0.25));
  ShapeBox.Density:=100.0;
  BoxBody.Finish;

  HullBody:=TKraftRigidBody.Create(Physics);
  HullBody.SetRigidBodyType(krbtDYNAMIC);
  Hull:=TKraftConvexHull.Create(Physics);
  for CornerIndex:=0 to 7 do begin
   Hull.AddVertex(Vector3(0.25*(((CornerIndex and 1) shl 1)-1),
                          0.25*((((CornerIndex shr 1) and 1) shl 1)-1),
                          0.25*((((CornerIndex shr 2) and 1) shl 1)-1)));
  end;
  Hull.Build;
  Hull.Finish;
  ShapeHull:=TKraftShapeConvexHull.Create(Physics,HullBody,Hull);
  ShapeHull.Density:=100.0;
  HullBody.Finish;

  WriteLn('Box  shape: mass=',BoxBody.Mass:9:4,' invInertia diag=(',
          BoxBody.BodyInverseInertiaTensor[0,0]:8:4,',',
          BoxBody.BodyInverseInertiaTensor[1,1]:8:4,',',
          BoxBody.BodyInverseInertiaTensor[2,2]:8:4,')');
  WriteLn('Hull shape: mass=',HullBody.Mass:9:4,' invInertia diag=(',
          HullBody.BodyInverseInertiaTensor[0,0]:8:4,',',
          HullBody.BodyInverseInertiaTensor[1,1]:8:4,',',
          HullBody.BodyInverseInertiaTensor[2,2]:8:4,')');
  WriteLn('Analytic  : mass=  12.5000 invInertia diag=(  1.9200,  1.9200,  1.9200)');
 finally
  Physics.Free;
 end;
end.
