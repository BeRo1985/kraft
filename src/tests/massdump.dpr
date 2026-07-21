program massdump;
{$ifdef fpc}
 {$mode delphi}
{$endif}
{$apptype console}
uses {$ifdef unix}cthreads,{$endif} SysUtils,kraft;
var Physics:TKraft;
    ArmBody,BigBody:TKraftRigidBody;
    Shape:TKraftShapeCapsule;
begin
 FormatSettings.DecimalSeparator:='.';
 Physics:=TKraft.Create(-1);
 ArmBody:=TKraftRigidBody.Create(Physics);
 ArmBody.SetRigidBodyType(krbtDYNAMIC);
 Shape:=TKraftShapeCapsule.Create(Physics,ArmBody,0.045,0.30);
 Shape.Density:=1.0;
 ArmBody.Finish;
 WriteLn('small capsule r=0.045 h=0.30:');
 WriteLn(' mass=',ArmBody.Mass:0:6,' invMass=',ArmBody.InverseMass:0:6);
 WriteLn(' bodyInertia diag=(',ArmBody.BodyInertiaTensor[0,0]:0:8,',',ArmBody.BodyInertiaTensor[1,1]:0:8,',',ArmBody.BodyInertiaTensor[2,2]:0:8,')');
 WriteLn(' bodyInvInertia diag=(',ArmBody.BodyInverseInertiaTensor[0,0]:0:6,',',ArmBody.BodyInverseInertiaTensor[1,1]:0:6,',',ArmBody.BodyInverseInertiaTensor[2,2]:0:6,')');
 BigBody:=TKraftRigidBody.Create(Physics);
 BigBody.SetRigidBodyType(krbtDYNAMIC);
 Shape:=TKraftShapeCapsule.Create(Physics,BigBody,0.5,3.0);
 Shape.Density:=1.0;
 BigBody.Finish;
 WriteLn('big capsule r=0.5 h=3.0:');
 WriteLn(' mass=',BigBody.Mass:0:6,' invMass=',BigBody.InverseMass:0:6);
 WriteLn(' bodyInertia diag=(',BigBody.BodyInertiaTensor[0,0]:0:6,',',BigBody.BodyInertiaTensor[1,1]:0:6,',',BigBody.BodyInertiaTensor[2,2]:0:6,')');
 Physics.Free;
end.
