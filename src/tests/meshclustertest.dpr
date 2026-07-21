program meshclustertest;

{$ifdef fpc}
 {$mode delphi}
{$endif}
{$apptype console}

uses {$ifdef unix}cthreads,{$endif} SysUtils,Math,kraft;

// Validation harness for the clustered mesh manifold mode: dynamic box/sphere/capsule bodies on a flat but
// finely tessellated static triangle mesh. Compares per-triangle versus clustered mode per solver: resting
// height (no sinking, no hovering), sleep liveness, touching pair and contact counts (clustering should
// concentrate the contacts into few representative pairs), a slide-across-the-seams test (vertical kick =
// snagging indicator) and a wake-after-sleep test. Run twice per configuration for a determinism check.

type TScenarioResult=record
      RestY:array[0..2] of TKraftScalar;
      AllAsleep:boolean;
      TouchingPairs:longint;
      TotalContacts:longint;
      RepresentativePairs:longint;
      SlideDistance:TKraftScalar;
      SlideMaxVerticalVelocity:TKraftScalar;
      WakeSettleY:TKraftScalar;
     end;

function BuildMeshBody(const aPhysics:TKraft;const aWavy:boolean):TKraftRigidBody;
const GridSize=16;
      CellSize=0.5;
var Mesh:TKraftMesh;
    ShapeMesh:TKraftShapeMesh;
    IndexX,IndexZ:longint;
    VertexIndices:array[0..GridSize,0..GridSize] of longint;
begin
 result:=TKraftRigidBody.Create(aPhysics);
 result.SetRigidBodyType(krbtSTATIC);
 Mesh:=TKraftMesh.Create(aPhysics);
 for IndexZ:=0 to GridSize do begin
  for IndexX:=0 to GridSize do begin
   if aWavy then begin
    VertexIndices[IndexX,IndexZ]:=Mesh.AddVertex(Vector3((IndexX-(GridSize*0.5))*CellSize,
                                                         0.15*sin(IndexX*0.8)*cos(IndexZ*0.7),
                                                         (IndexZ-(GridSize*0.5))*CellSize));
   end else begin
    VertexIndices[IndexX,IndexZ]:=Mesh.AddVertex(Vector3((IndexX-(GridSize*0.5))*CellSize,0.0,(IndexZ-(GridSize*0.5))*CellSize));
   end;
  end;
 end;
 for IndexZ:=0 to GridSize-1 do begin
  for IndexX:=0 to GridSize-1 do begin
   Mesh.AddTriangle(VertexIndices[IndexX,IndexZ],VertexIndices[IndexX,IndexZ+1],VertexIndices[IndexX+1,IndexZ]);
   Mesh.AddTriangle(VertexIndices[IndexX+1,IndexZ],VertexIndices[IndexX,IndexZ+1],VertexIndices[IndexX+1,IndexZ+1]);
  end;
 end;
 Mesh.Finish;
 ShapeMesh:=TKraftShapeMesh.Create(aPhysics,result,Mesh);
 ShapeMesh.Friction:=0.5;
 ShapeMesh.Restitution:=0.0;
 ShapeMesh.Finish;
 result.Finish;
 result.SetWorldTransformation(Matrix4x4Translate(0.0,0.0,0.0));
 result.CollisionGroups:=[0];
end;

procedure CountMeshContacts(const aPhysics:TKraft;out aTouchingPairs,aTotalContacts,aRepresentativePairs:longint);
var ContactPair:PKraftContactPair;
begin
 aTouchingPairs:=0;
 aTotalContacts:=0;
 aRepresentativePairs:=0;
 ContactPair:=aPhysics.ContactManager.ContactPairFirst;
 while assigned(ContactPair) do begin
  if assigned(ContactPair^.MeshContactPair) and (ContactPair^.Manifold.CountContacts>0) then begin
   inc(aTouchingPairs);
   inc(aTotalContacts,ContactPair^.Manifold.CountContacts);
   if kcfMeshClusterRepresentative in ContactPair^.Flags then begin
    inc(aRepresentativePairs);
   end;
  end;
  ContactPair:=ContactPair^.Next;
 end;
end;

procedure RunScenario(const aSolverMode:TKraftSolverMode;const aTGSJointMode:TKraftTGSJointMode;const aMeshManifoldMode:TKraftMeshManifoldMode;const aInternalEdgeHandlingMode:TKraftInternalEdgeHandlingMode;out aResult:TScenarioResult);
var Physics:TKraft;
    Bodies:array[0..2] of TKraftRigidBody;
    SlideBody:TKraftRigidBody;
    ShapeBox:TKraftShapeBox;
    ShapeSphere:TKraftShapeSphere;
    ShapeCapsule:TKraftShapeCapsule;
    StepIndex,BodyIndex:longint;
    VerticalVelocity:TKraftScalar;
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
  Physics.TGSJointMode:=aTGSJointMode;
  Physics.MeshManifoldMode:=aMeshManifoldMode;
  Physics.InternalEdgeHandlingMode:=aInternalEdgeHandlingMode;

  BuildMeshBody(Physics,false);

  // Resting box, straddling triangle seams on purpose (0.4 half extents over 0.5 cells)
  Bodies[0]:=TKraftRigidBody.Create(Physics);
  Bodies[0].SetRigidBodyType(krbtDynamic);
  ShapeBox:=TKraftShapeBox.Create(Physics,Bodies[0],Vector3(0.4,0.25,0.4));
  ShapeBox.Friction:=0.4;
  ShapeBox.Restitution:=0.0;
  ShapeBox.Density:=100.0;
  Bodies[0].Finish;
  Bodies[0].SetWorldTransformation(Matrix4x4Translate(-2.0,0.3,-2.0));
  Bodies[0].CollisionGroups:=[0];

  // Resting sphere
  Bodies[1]:=TKraftRigidBody.Create(Physics);
  Bodies[1].SetRigidBodyType(krbtDynamic);
  ShapeSphere:=TKraftShapeSphere.Create(Physics,Bodies[1],0.3);
  ShapeSphere.Friction:=0.4;
  ShapeSphere.Restitution:=0.0;
  ShapeSphere.Density:=100.0;
  Bodies[1].Finish;
  Bodies[1].SetWorldTransformation(Matrix4x4Translate(2.0,0.35,-2.0));
  Bodies[1].CollisionGroups:=[0];

  // Resting capsule, lying sideways across seams
  Bodies[2]:=TKraftRigidBody.Create(Physics);
  Bodies[2].SetRigidBodyType(krbtDynamic);
  ShapeCapsule:=TKraftShapeCapsule.Create(Physics,Bodies[2],0.2,1.2);
  ShapeCapsule.Friction:=0.4;
  ShapeCapsule.Restitution:=0.0;
  ShapeCapsule.Density:=100.0;
  Bodies[2].Finish;
  Bodies[2].SetWorldTransformation(Matrix4x4TermMul(Matrix4x4RotateZ(pi*0.5),Matrix4x4Translate(0.0,0.25,2.0)));
  Bodies[2].CollisionGroups:=[0];

  // Settle phase
  for StepIndex:=1 to 600 do begin
   Physics.Step(1.0/120.0);
  end;

  for BodyIndex:=0 to 2 do begin
   aResult.RestY[BodyIndex]:=Bodies[BodyIndex].Sweep.c.y;
  end;
  aResult.AllAsleep:=true;
  for BodyIndex:=0 to 2 do begin
   if krbfAwake in Bodies[BodyIndex].Flags then begin
    aResult.AllAsleep:=false;
    break;
   end;
  end;

  // Contact statistics of the settled state: one extra step so the counters reflect a regular step,
  // after waking one body up (sleeping pairs are not processed by the narrow phase).
  Bodies[0].SetToAwake;
  Physics.Step(1.0/120.0);
  CountMeshContacts(Physics,aResult.TouchingPairs,aResult.TotalContacts,aResult.RepresentativePairs);

  // Slide test: shoot the box across the tessellation seams and watch for vertical kicks (snagging).
  SlideBody:=Bodies[0];
  SlideBody.SetToAwake;
  SlideBody.SetWorldTransformation(Matrix4x4Translate(-3.5,0.251,0.0));
  SlideBody.LinearVelocity:=Vector3(8.0,0.0,0.0);
  SlideBody.AngularVelocity:=Vector3Origin;
  aResult.SlideMaxVerticalVelocity:=0.0;
  for StepIndex:=1 to 240 do begin
   Physics.Step(1.0/120.0);
   VerticalVelocity:=abs(SlideBody.LinearVelocity.y);
   if aResult.SlideMaxVerticalVelocity<VerticalVelocity then begin
    aResult.SlideMaxVerticalVelocity:=VerticalVelocity;
   end;
  end;
  aResult.SlideDistance:=SlideBody.Sweep.c.x-(-3.5);

  // Wake-after-sleep test: let everything settle and sleep, then kick the capsule and let it settle again.
  for StepIndex:=1 to 600 do begin
   Physics.Step(1.0/120.0);
  end;
  Bodies[2].SetToAwake;
  Bodies[2].LinearVelocity:=Vector3(0.0,2.0,0.0);
  for StepIndex:=1 to 600 do begin
   Physics.Step(1.0/120.0);
  end;
  aResult.WakeSettleY:=Bodies[2].Sweep.c.y;

 finally
  Physics.Free;
 end;

end;

procedure RunAndReport(const aName:string;const aSolverMode:TKraftSolverMode;const aTGSJointMode:TKraftTGSJointMode;const aMeshManifoldMode:TKraftMeshManifoldMode;const aInternalEdgeHandlingMode:TKraftInternalEdgeHandlingMode);
var FirstResult,SecondResult:TScenarioResult;
    Deterministic:boolean;
begin
 RunScenario(aSolverMode,aTGSJointMode,aMeshManifoldMode,aInternalEdgeHandlingMode,FirstResult);
 RunScenario(aSolverMode,aTGSJointMode,aMeshManifoldMode,aInternalEdgeHandlingMode,SecondResult);
 Deterministic:=(FirstResult.RestY[0]=SecondResult.RestY[0]) and
                (FirstResult.RestY[1]=SecondResult.RestY[1]) and
                (FirstResult.RestY[2]=SecondResult.RestY[2]) and
                (FirstResult.SlideDistance=SecondResult.SlideDistance) and
                (FirstResult.SlideMaxVerticalVelocity=SecondResult.SlideMaxVerticalVelocity) and
                (FirstResult.WakeSettleY=SecondResult.WakeSettleY);
 WriteLn(aName,': restY box/sphere/capsule=',FirstResult.RestY[0]:7:4,'/',FirstResult.RestY[1]:7:4,'/',FirstResult.RestY[2]:7:4,
         ' asleep=',FirstResult.AllAsleep,
         ' pairs=',FirstResult.TouchingPairs,' contacts=',FirstResult.TotalContacts,' representatives=',FirstResult.RepresentativePairs);
 WriteLn(aName,': slideDist=',FirstResult.SlideDistance:7:3,' slideMaxVy=',FirstResult.SlideMaxVerticalVelocity:7:4,
         ' wakeSettleY=',FirstResult.WakeSettleY:7:4,
         ' deterministic=',Deterministic);
end;

procedure RunBumpy(const aName:string;const aSolverMode:TKraftSolverMode;const aTGSJointMode:TKraftTGSJointMode;const aMeshManifoldMode:TKraftMeshManifoldMode);
var Physics:TKraft;
    Bodies:array[0..5] of TKraftRigidBody;
    ShapeBox:TKraftShapeBox;
    StepIndex,BodyIndex,TouchingPairs,TotalContacts,RepresentativePairs:longint;
    MinY,MaxY,YSum1,YSum2:TKraftScalar;
    AllAsleep,SecondRun:boolean;
begin
 YSum1:=0.0;
 YSum2:=0.0;
 for SecondRun:=false to true do begin
  Physics:=TKraft.Create(-1);
  try
   Physics.SetFrequency(120.0);
   Physics.VelocityIterations:=8;
   Physics.PositionIterations:=3;
   Physics.SpeculativeIterations:=8;
   Physics.TimeOfImpactIterations:=20;
   Physics.Gravity.y:=-9.81;
   Physics.SolverMode:=aSolverMode;
   Physics.TGSJointMode:=aTGSJointMode;
   Physics.MeshManifoldMode:=aMeshManifoldMode;
   BuildMeshBody(Physics,true);
   for BodyIndex:=0 to 5 do begin
    Bodies[BodyIndex]:=TKraftRigidBody.Create(Physics);
    Bodies[BodyIndex].SetRigidBodyType(krbtDynamic);
    ShapeBox:=TKraftShapeBox.Create(Physics,Bodies[BodyIndex],Vector3(0.3,0.3,0.3));
    ShapeBox.Friction:=0.4;
    ShapeBox.Restitution:=0.1;
    ShapeBox.Density:=100.0;
    Bodies[BodyIndex].Finish;
    Bodies[BodyIndex].SetWorldTransformation(Matrix4x4TermMul(Matrix4x4RotateY(BodyIndex*0.5),
                                                              Matrix4x4Translate(((BodyIndex mod 3)-1)*1.5,1.5+((BodyIndex div 3)*0.8),((BodyIndex div 3)-0.5)*1.5)));
    Bodies[BodyIndex].CollisionGroups:=[0];
   end;
   for StepIndex:=1 to 1200 do begin
    Physics.Step(1.0/120.0);
   end;
   MinY:=1e30;
   MaxY:=-1e30;
   AllAsleep:=true;
   for BodyIndex:=0 to 5 do begin
    MinY:=Min(MinY,Bodies[BodyIndex].Sweep.c.y);
    MaxY:=Max(MaxY,Bodies[BodyIndex].Sweep.c.y);
    if krbfAwake in Bodies[BodyIndex].Flags then begin
     AllAsleep:=false;
    end;
    if SecondRun then begin
     YSum2:=YSum2+Bodies[BodyIndex].Sweep.c.y;
    end else begin
     YSum1:=YSum1+Bodies[BodyIndex].Sweep.c.y;
    end;
   end;
   if not SecondRun then begin
    Bodies[0].SetToAwake;
    Physics.Step(1.0/120.0);
    CountMeshContacts(Physics,TouchingPairs,TotalContacts,RepresentativePairs);
    WriteLn(aName,': boxY min/max=',MinY:7:4,'/',MaxY:7:4,' asleep=',AllAsleep,
            ' pairs=',TouchingPairs,' contacts=',TotalContacts,' representatives=',RepresentativePairs);
   end;
  finally
   Physics.Free;
  end;
 end;
 WriteLn(aName,': deterministic=',YSum1=YSum2);
end;

procedure RunMeshBench(const aName:string;const aMeshManifoldMode:TKraftMeshManifoldMode;const aRecycle:boolean);
// Narrow phase cost probe on the mesh: one box resting on the tessellated floor with sleeping disabled, so
// the mesh contact pair keeps running every step. Reports the recycle rate and the narrow phase time, plus
// the contact statistics at the end (the cluster manifold must stay alive while recycling).
var Physics:TKraft;
    BoxBody:TKraftRigidBody;
    ShapeBox:TKraftShapeBox;
    StepIndex,TouchingPairs,TotalContacts,RepresentativePairs:longint;
    RecycledSum,PairSum:int64;
begin
 Physics:=TKraft.Create(-1);
 try
  Physics.SetFrequency(120.0);
  Physics.VelocityIterations:=8;
  Physics.PositionIterations:=3;
  Physics.SpeculativeIterations:=8;
  Physics.TimeOfImpactIterations:=20;
  Physics.Gravity.y:=-9.81;
  Physics.SolverMode:=ksmTGSSoft;
  Physics.TGSJointMode:=ktjmNativeSoft;
  Physics.MeshManifoldMode:=aMeshManifoldMode;
  if not aRecycle then begin
   Physics.ContactRecycleDistance:=0.0;
  end;
  BuildMeshBody(Physics,false);
  BoxBody:=TKraftRigidBody.Create(Physics);
  BoxBody.SetRigidBodyType(krbtDynamic);
  ShapeBox:=TKraftShapeBox.Create(Physics,BoxBody,Vector3(0.4,0.25,0.4));
  ShapeBox.Friction:=0.4;
  ShapeBox.Restitution:=0.0;
  ShapeBox.Density:=100.0;
  BoxBody.Flags:=BoxBody.Flags-[krbfAllowSleep];
  BoxBody.Finish;
  BoxBody.SetWorldTransformation(Matrix4x4Translate(-0.2,0.3,-0.2));
  BoxBody.CollisionGroups:=[0];
  RecycledSum:=0;
  PairSum:=0;
  for StepIndex:=1 to 900 do begin
   Physics.Step(1.0/120.0);
   if StepIndex>300 then begin
    inc(RecycledSum,Physics.ContactManager.CountRecycledContactPairs);
    inc(PairSum,Physics.ContactManager.CountContactPairs);
   end;
  end;
  CountMeshContacts(Physics,TouchingPairs,TotalContacts,RepresentativePairs);
  WriteLn(aName,': narrowPhaseTime=',Physics.NarrowPhaseTime,
          ' recycled=',RecycledSum,'/',PairSum,' pair-steps (',(RecycledSum*100.0)/Max(1,PairSum):5:1,'%)',
          ' boxY=',BoxBody.Sweep.c.y:7:4,
          ' pairs=',TouchingPairs,' contacts=',TotalContacts,' representatives=',RepresentativePairs);
 finally
  Physics.Free;
 end;
end;

begin
 FormatSettings.DecimalSeparator:='.';
 RunAndReport('SI  PerTriangle         ',ksmSequentialImpulse,ktjmAdapter,kmmmPerTriangle,kiehmNone);
 RunAndReport('SI  Clustered           ',ksmSequentialImpulse,ktjmAdapter,kmmmClustered,kiehmNone);
 RunAndReport('NAT PerTriangle         ',ksmTGSSoft,ktjmNativeSoft,kmmmPerTriangle,kiehmNone);
 RunAndReport('NAT Clustered           ',ksmTGSSoft,ktjmNativeSoft,kmmmClustered,kiehmNone);
 RunAndReport('NAT Clustered +Clamping ',ksmTGSSoft,ktjmNativeSoft,kmmmClustered,kiehmClamping);
 RunAndReport('NAT Clustered +Ownership',ksmTGSSoft,ktjmNativeSoft,kmmmClustered,kiehmFeatureOwnership);
 RunBumpy('NAT PerTriangle bumpy   ',ksmTGSSoft,ktjmNativeSoft,kmmmPerTriangle);
 RunBumpy('NAT Clustered   bumpy   ',ksmTGSSoft,ktjmNativeSoft,kmmmClustered);
 RunMeshBench('NAT PerTriangle bench   ',kmmmPerTriangle,true);
 RunMeshBench('NAT Clustered   bench   ',kmmmClustered,true);
 RunMeshBench('NAT Clustered   norecycl',kmmmClustered,false);
end.
