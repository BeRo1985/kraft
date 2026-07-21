program carouselfull;
{$ifdef fpc}
 {$mode delphi}
{$endif}
{$apptype console}
uses {$ifdef unix}cthreads,{$endif} SysUtils,Math,kraft;

procedure RunOne(const aName:string;const aSolverMode:TKraftSolverMode;const aTGSJointMode:TKraftTGSJointMode;const ChainDamp,SeatDamp,ChainDensity:TKraftScalar);
const CountChainSpheres=4;
var Physics:TKraft;
    BaseBody,TopBody,SeatBody:TKraftRigidBody;
    ChainBodies:array[0..3,0..CountChainSpheres-1] of TKraftRigidBody;
    Hull:TKraftConvexHull;
    ShapeHull:TKraftShapeConvexHull;
    ShapeBox:TKraftShapeBox;
    ShapeSphere:TKraftShapeSphere;
    Index,EdgeIndex,SubIndex,StepIndex:longint;
    v,MaxChainAng,LateMaxChainAng,SeatY:TKraftScalar;
    m:TKraftMatrix4x4;
    vFrom,vTo:TKraftVector3;
    Exploded:boolean;
begin
 Physics:=TKraft.Create(-1);
 try
  Physics.SetFrequency(120.0);
  Physics.VelocityIterations:=8;
  Physics.PositionIterations:=3;
  Physics.Gravity.y:=-9.81;
  Physics.SolverMode:=aSolverMode;
  Physics.TGSJointMode:=aTGSJointMode;
  if GetEnvironmentVariable('KRAFT_NORECYCLE')='1' then begin
   Physics.ContactRecycleDistance:=0.0;
  end;
  if GetEnvironmentVariable('KRAFT_SPM')='colored' then begin
   Physics.SolverParallelMode:=kspmIslandColored;
  end else begin
   if GetEnvironmentVariable('KRAFT_SPM')='global' then begin
    Physics.SolverParallelMode:=kspmGlobalGraph;
   end;
  end;
  if GetEnvironmentVariable('KRAFT_WIDE')='1' then begin
   Physics.WideContactSolver:=true;
  end;
  if GetEnvironmentVariable('KRAFT_FUSE')='0' then begin
   Physics.SolverFusedColorStages:=false;
  end;

  BaseBody:=TKraftRigidBody.Create(Physics);
  BaseBody.SetRigidBodyType(krbtSTATIC);
  Hull:=TKraftConvexHull.Create(Physics);
  for Index:=0 to 15 do begin
   v:=(Index/16)*(pi*2.0);
   Hull.AddVertex(Vector3(sin(v)*1.0,-0.25,cos(v)*1.0));
   Hull.AddVertex(Vector3(sin(v)*1.0,0.25,cos(v)*1.0));
  end;
  Hull.Build; Hull.Finish;
  ShapeHull:=TKraftShapeConvexHull.Create(Physics,BaseBody,Hull);
  BaseBody.Finish;
  BaseBody.SetWorldTransformation(Matrix4x4Translate(0.0,0.25,0.0));
  BaseBody.CollisionGroups:=[0];

  TopBody:=TKraftRigidBody.Create(Physics);
  TopBody.SetRigidBodyType(krbtDYNAMIC);
  Hull:=TKraftConvexHull.Create(Physics);
  for Index:=0 to 15 do begin
   v:=(Index/16)*(pi*2.0);
   Hull.AddVertex(Vector3(sin(v)*3.0,-0.125,cos(v)*3.0));
   Hull.AddVertex(Vector3(sin(v)*3.0,0.125,cos(v)*3.0));
  end;
  Hull.Build; Hull.Finish;
  ShapeHull:=TKraftShapeConvexHull.Create(Physics,TopBody,Hull);
  ShapeHull.Density:=1.0;
  ShapeHull.Finish;
  TopBody.ForcedMass:=100.0;
  TopBody.Finish;
  TopBody.SetWorldTransformation(Matrix4x4Translate(0.0,0.625,0.0));
  TopBody.CollisionGroups:=[0];
  TKraftConstraintJointHinge.Create(Physics,BaseBody,TopBody,Vector3(0.0,0.5,0.0),Vector3(0.0,1.0,0.0));

  SeatBody:=TKraftRigidBody.Create(Physics);
  SeatBody.SetRigidBodyType(krbtDYNAMIC);
  ShapeBox:=TKraftShapeBox.Create(Physics,SeatBody,Vector3(0.25,0.0625,0.25));
  ShapeBox.Finish;
  ShapeBox:=TKraftShapeBox.Create(Physics,SeatBody,Vector3(0.0625,0.25,0.25));
  ShapeBox.LocalTransform:=Matrix4x4Translate(0.0625-0.25,0.0625+0.25,0.0);
  ShapeBox.Finish;
  SeatBody.ForcedMass:=5.0;
  SeatBody.Finish;
  SeatBody.AngularVelocityDamp:=SeatDamp;
  m:=Matrix4x4Translate(0.0,0.5+0.25+0.5,3.0-1.0);
  SeatBody.SetWorldTransformation(m);

  for EdgeIndex:=0 to 3 do begin
   case EdgeIndex of
    0:begin vFrom:=Vector3TermMatrixMul(Vector3(0.25,2.0,0.25),m); vTo:=Vector3TermMatrixMul(Vector3(0.25,0.5,0.25),m); end;
    1:begin vFrom:=Vector3TermMatrixMul(Vector3(-0.25,2.0,0.25),m); vTo:=Vector3TermMatrixMul(Vector3(-0.25,0.5,0.25),m); end;
    2:begin vFrom:=Vector3TermMatrixMul(Vector3(-0.25,2.0,-0.25),m); vTo:=Vector3TermMatrixMul(Vector3(-0.25,0.5,-0.25),m); end;
    else begin vFrom:=Vector3TermMatrixMul(Vector3(0.25,2.0,-0.25),m); vTo:=Vector3TermMatrixMul(Vector3(0.25,0.5,-0.25),m); end;
   end;
   for SubIndex:=0 to CountChainSpheres-1 do begin
    ChainBodies[EdgeIndex,SubIndex]:=TKraftRigidBody.Create(Physics);
    ChainBodies[EdgeIndex,SubIndex].SetRigidBodyType(krbtDYNAMIC);
    ShapeSphere:=TKraftShapeSphere.Create(Physics,ChainBodies[EdgeIndex,SubIndex],0.1);
    ShapeSphere.Restitution:=0.3;
    ShapeSphere.Density:=ChainDensity;
    ChainBodies[EdgeIndex,SubIndex].Finish;
    ChainBodies[EdgeIndex,SubIndex].AngularVelocityDamp:=ChainDamp;
    ChainBodies[EdgeIndex,SubIndex].SetWorldTransformation(Matrix4x4Translate(Vector3Lerp(vTo,vFrom,SubIndex/CountChainSpheres)));
    ChainBodies[EdgeIndex,SubIndex].CollisionGroups:=[0];
   end;
   for SubIndex:=1 to CountChainSpheres-1 do begin
    TKraftConstraintJointBallSocket.Create(Physics,ChainBodies[EdgeIndex,SubIndex-1],ChainBodies[EdgeIndex,SubIndex],Vector3Lerp(PKraftVector3(pointer(@ChainBodies[EdgeIndex,SubIndex].WorldTransform[3,0]))^,PKraftVector3(pointer(@ChainBodies[EdgeIndex,SubIndex-1].WorldTransform[3,0]))^,0.5),true);
   end;
   TKraftConstraintJointBallSocket.Create(Physics,TopBody,ChainBodies[EdgeIndex,CountChainSpheres-1],vFrom,true);
   TKraftConstraintJointBallSocket.Create(Physics,SeatBody,ChainBodies[EdgeIndex,0],vTo,false);
  end;
  SeatBody.CollisionGroups:=[0];

  LateMaxChainAng:=0.0;
  Exploded:=false;
  for StepIndex:=1 to 3600 do begin
   TopBody.SetBodyTorque(Vector3(0.0,1.25,0.0),kfmAcceleration);
   Physics.Step(1.0/120.0);
   MaxChainAng:=0.0;
   for EdgeIndex:=0 to 3 do begin
    for SubIndex:=0 to CountChainSpheres-1 do begin
     MaxChainAng:=Max(MaxChainAng,Vector3Length(ChainBodies[EdgeIndex,SubIndex].AngularVelocity));
    end;
   end;
   if MaxChainAng>150.0 then begin
    Exploded:=true;
   end;
   if StepIndex>2400 then begin
    LateMaxChainAng:=Max(LateMaxChainAng,MaxChainAng);
   end;
  end;
  SeatY:=SeatBody.Sweep.c.y;
  Write(aName,': late maxChainAng=',LateMaxChainAng:7:2,' seatY=',SeatY:6:3,' TellerOmega=',TopBody.AngularVelocity.y:6:2);
  if Exploded then begin
   WriteLn('  EXPLODIERT');
  end else begin
   WriteLn;
  end;
 finally
  Physics.Free;
 end;
end;

begin
 FormatSettings.DecimalSeparator:='.';
 RunOne('SI          d=100 ',ksmSequentialImpulse,ktjmAdapter,10.0,1.0,100.0);
 RunOne('TGA-alt     d=100 ',ksmTGSSoft,ktjmAdapter,10.0,1.0,100.0);
 RunOne('TGA-Substep d=100 ',ksmTGSSoft,ktjmAdapterSubstep,10.0,1.0,100.0);
 RunOne('NAT         d=100 ',ksmTGSSoft,ktjmNativeSoft,10.0,1.0,100.0);
end.
