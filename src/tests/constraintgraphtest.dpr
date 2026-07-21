program constraintgraphtest;

{$ifdef fpc}
 {$mode delphi}
{$endif}
{$apptype console}

uses {$ifdef unix}cthreads,{$endif} SysUtils,Math,kraft;

// Invariant harness for the K6 stage 2 persistent constraint graph: after every step the graph must contain
// exactly the touching non-sensor contact pairs and the active unbroken joints with at least one movable
// awake body, with consistent back references, no movable body referenced twice within one color and body
// occupancy bitsets which match the actual residency. Exercised over piles (contact churn), a mesh floor
// (triangle contact pairs), joints (ball sockets and a rope chain), body churn, forced sleep/wake cycles,
// body type switches and joint destruction. Any violation fails loudly.

var TotalChecks:int64=0;

procedure Fail(const aMessage:string);
begin
 WriteLn('FAIL: ',aMessage);
 Halt(1);
end;

procedure CheckGraph(const aPhysics:TKraft;const aWhere:string);
var GraphError:string;
begin
 if not aPhysics.ConstraintGraph.Validate(GraphError) then begin
  Fail(aWhere+': '+GraphError);
 end;
 inc(TotalChecks);
end;

procedure CheckPersistentIslands(const aPhysics:TKraft;const aWhere:string);
// Only meaningful directly after a step: the comparison partner is the per step island build, which becomes
// stale (up to dangling body pointers) as soon as bodies get destroyed between the steps.
var GraphError:TKraftString; // ValidatePersistentIslands wants a var/out RawByteString, which TKraftString aliases
begin
 if not aPhysics.ValidatePersistentIslands(GraphError) then begin
  Fail(aWhere+' (persistent islands): '+GraphError);
 end;
 inc(TotalChecks);
end;

function BuildMeshFloor(const aPhysics:TKraft):TKraftRigidBody;
const GridSize=8;
      CellSize=1.0;
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
   VertexIndices[IndexX,IndexZ]:=Mesh.AddVertex(Vector3((IndexX-(GridSize*0.5))*CellSize,0.0,(IndexZ-(GridSize*0.5))*CellSize));
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
 ShapeMesh.Finish;
 result.Finish;
 result.SetWorldTransformation(Matrix4x4Translate(0.0,0.0,0.0));
 result.CollisionGroups:=[0];
end;

function SpawnBox(const aPhysics:TKraft;const aX,aY,aZ:TKraftScalar):TKraftRigidBody;
var ShapeBox:TKraftShapeBox;
begin
 result:=TKraftRigidBody.Create(aPhysics);
 result.SetRigidBodyType(krbtDynamic);
 ShapeBox:=TKraftShapeBox.Create(aPhysics,result,Vector3(0.25,0.25,0.25));
 ShapeBox.Restitution:=0.1;
 ShapeBox.Density:=100.0;
 result.Finish;
 result.SetWorldTransformation(Matrix4x4Translate(aX,aY,aZ));
 result.CollisionGroups:=[0];
end;

procedure RunScenario(const aName:string;const aSolverMode:TKraftSolverMode;const aTGSJointMode:TKraftTGSJointMode;const aMeshManifoldMode:TKraftMeshManifoldMode);
var Physics:TKraft;
    Boxes:array[0..15] of TKraftRigidBody;
    ChainBodies:array[0..3] of TKraftRigidBody;
    ChainJoints:array[0..3] of TKraftConstraintJointBallSocket;
    GrabJoint:TKraftConstraintJointGrab;
    StepIndex,BoxIndex,ChurnCounter:longint;
begin

 Physics:=TKraft.Create(-1);
 try

  Physics.SetFrequency(120.0);
  Physics.Gravity.y:=-9.81;
  Physics.SolverMode:=aSolverMode;
  Physics.TGSJointMode:=aTGSJointMode;
  Physics.MeshManifoldMode:=aMeshManifoldMode;
  if GetEnvironmentVariable('KRAFT_SPM')='colored' then begin
   Physics.SolverParallelMode:=kspmIslandColored;
  end else begin
   if GetEnvironmentVariable('KRAFT_SPM')='global' then begin
    Physics.SolverParallelMode:=kspmGlobalGraph;
   end;
  end;
  if GetEnvironmentVariable('KRAFT_PIPE')='0' then begin
   Physics.SolverStagePipeline:=false;
  end;
  if GetEnvironmentVariable('KRAFT_WIDE')='1' then begin
   Physics.WideContactSolver:=true;
  end;
  if GetEnvironmentVariable('KRAFT_FUSE')='0' then begin
   Physics.SolverFusedColorStages:=false;
  end;

  BuildMeshFloor(Physics);

  // Dense pile of touching boxes (contact churn while settling)
  for BoxIndex:=0 to 15 do begin
   Boxes[BoxIndex]:=SpawnBox(Physics,((BoxIndex mod 4)-1.5)*0.4999,0.3+((BoxIndex div 4)*0.55),0.0);
  end;

  // Hanging ball socket chain plus a rope joint (joints in the graph, including a static anchor partner)
  for BoxIndex:=0 to 3 do begin
   ChainBodies[BoxIndex]:=SpawnBox(Physics,3.0,2.5-(BoxIndex*0.6),0.0);
  end;
  for BoxIndex:=1 to 3 do begin
   ChainJoints[BoxIndex]:=TKraftConstraintJointBallSocket.Create(Physics,ChainBodies[BoxIndex-1],ChainBodies[BoxIndex],Vector3(3.0,2.5-((BoxIndex-0.5)*0.6),0.0),true);
  end;
  // Single body joint with a world anchor (RigidBodies[1]=nil partner case in the graph)
  GrabJoint:=TKraftConstraintJointGrab.Create(Physics,ChainBodies[0],Vector3(3.0,2.5,0.0));
  if not assigned(GrabJoint) then begin
   Fail('grab joint creation failed');
  end;
  ChainJoints[0]:=nil;

  // The graph is consistent at step boundaries: freshly created joints enter it at the narrow phase of
  // the next step, so the first validation happens after the first step.
  ChurnCounter:=0;
  for StepIndex:=1 to 600 do begin

   Physics.Step(1.0/120.0);
   CheckGraph(Physics,'step '+IntToStr(StepIndex));
   CheckPersistentIslands(Physics,'step '+IntToStr(StepIndex));

   if (StepIndex mod 15)=0 then begin
    BoxIndex:=ChurnCounter mod 16;
    inc(ChurnCounter);
    Boxes[BoxIndex].Free;
    Boxes[BoxIndex]:=SpawnBox(Physics,((BoxIndex mod 4)-1.5)*0.4999,2.5,((ChurnCounter mod 3)-1)*0.3);
    CheckGraph(Physics,'churn step '+IntToStr(StepIndex));
   end;

   if (StepIndex mod 41)=0 then begin
    Boxes[(StepIndex div 41) mod 16].SetToSleep;
    CheckGraph(Physics,'sleep step '+IntToStr(StepIndex));
   end;
   if (StepIndex mod 59)=0 then begin
    Boxes[(StepIndex div 59) mod 16].SetToAwake;
    CheckGraph(Physics,'wake step '+IntToStr(StepIndex));
   end;

   if (StepIndex mod 101)=0 then begin
    BoxIndex:=(StepIndex div 101) mod 16;
    Boxes[BoxIndex].SetRigidBodyType(krbtStatic);
    CheckGraph(Physics,'tostatic step '+IntToStr(StepIndex));
    Boxes[BoxIndex].SetRigidBodyType(krbtDynamic);
    Boxes[BoxIndex].SetToAwake;
    CheckGraph(Physics,'todynamic step '+IntToStr(StepIndex));
   end;

   // Destroy and recreate a chain joint mid-simulation
   if StepIndex=300 then begin
    ChainJoints[2].Free;
    ChainJoints[2]:=nil;
    CheckGraph(Physics,'jointfree step '+IntToStr(StepIndex));
   end;
   if StepIndex=360 then begin
    // Freshly created joints enter the graph at the next step, checked by the per-step validation above
    ChainJoints[2]:=TKraftConstraintJointBallSocket.Create(Physics,ChainBodies[1],ChainBodies[2],Vector3(3.0,2.5-(1.5*0.6),0.0),true);
   end;

  end;

  WriteLn('PASS ',aName,': ',TotalChecks,' graph validations so far');
 finally
  Physics.Free;
 end;

end;

begin
 FormatSettings.DecimalSeparator:='.';
 RunScenario('NAT PerTriangle',ksmTGSSoft,ktjmNativeSoft,kmmmPerTriangle);
 RunScenario('NAT Clustered  ',ksmTGSSoft,ktjmNativeSoft,kmmmClustered);
 RunScenario('SI  PerTriangle',ksmSequentialImpulse,ktjmAdapter,kmmmPerTriangle);
end.
