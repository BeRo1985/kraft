program meshmeshtest;
{$ifdef fpc}
 {$mode delphi}
{$endif}
{$apptype console}
uses {$ifdef unix}cthreads,{$endif} SysUtils,Math,kraft;

// Test harness for the mesh versus mesh campaign: dynamic mesh bodies, the dedicated triangle versus triangle
// narrow phase, CCD for mesh pairs and the query mesh support of PushShape/CollideShape.

type { TSlabSDF }
     // Wide flat rounded slab, outer half extents (8.0,0.5,8.0), top face at y=0.5
     TSlabSDF=class(TKraftSignedDistanceField)
      public
       function GetLocalSignedDistance(const Position:TKraftVector3):TKraftScalar; override;
     end;

var CountPassed,CountFailed:longint;

function SdRoundedBox(const Position,b:TKraftVector3;const r:TKraftScalar):TKraftScalar;
var q:TKraftVector3;
begin
 q:=Vector3Add(Vector3Sub(Vector3Abs(Position),b),Vector3(r,r,r));
 result:=(Vector3Length(Vector3(Max(0.0,q.x),Max(0.0,q.y),Max(0.0,q.z)))+Min(Max(q.x,Max(q.y,q.z)),0.0))-r;
end;

function TSlabSDF.GetLocalSignedDistance(const Position:TKraftVector3):TKraftScalar;
begin
 result:=SdRoundedBox(Position,Vector3(8.0,0.5,8.0),0.25);
end;

procedure Check(const aName:string;const aOK:boolean);
begin
 if aOK then begin
  inc(CountPassed);
  WriteLn(' PASS ',aName);
 end else begin
  inc(CountFailed);
  WriteLn(' FAIL ',aName);
 end;
end;

procedure CheckScalar(const aName:string;const aGot,aWant,aTolerance:TKraftScalar);
begin
 if abs(aGot-aWant)<=aTolerance then begin
  inc(CountPassed);
  WriteLn(' PASS ',aName,' (got ',aGot:9:5,')');
 end else begin
  inc(CountFailed);
  WriteLn(' FAIL ',aName,' (got ',aGot:9:5,', want ',aWant:9:5,')');
 end;
end;

procedure CheckVector(const aName:string;const aGot,aWant:TKraftVector3;const aTolerance:TKraftScalar);
begin
 if Vector3Dist(aGot,aWant)<=aTolerance then begin
  inc(CountPassed);
  WriteLn(' PASS ',aName,' (got ',aGot.x:7:4,',',aGot.y:7:4,',',aGot.z:7:4,')');
 end else begin
  inc(CountFailed);
  WriteLn(' FAIL ',aName,' (got ',aGot.x:7:4,',',aGot.y:7:4,',',aGot.z:7:4,' want ',aWant.x:7:4,',',aWant.y:7:4,',',aWant.z:7:4,')');
 end;
end;

function MakeBoxMesh(const aPhysics:TKraft;const aHalfExtentX,aHalfExtentY,aHalfExtentZ:TKraftScalar):TKraftMesh;
var VertexIndices:array[0..7] of TKraftInt32;
begin
 result:=TKraftMesh.Create(aPhysics);
 VertexIndices[0]:=result.AddVertex(Vector3(-aHalfExtentX,-aHalfExtentY,-aHalfExtentZ),false);
 VertexIndices[1]:=result.AddVertex(Vector3(aHalfExtentX,-aHalfExtentY,-aHalfExtentZ),false);
 VertexIndices[2]:=result.AddVertex(Vector3(aHalfExtentX,aHalfExtentY,-aHalfExtentZ),false);
 VertexIndices[3]:=result.AddVertex(Vector3(-aHalfExtentX,aHalfExtentY,-aHalfExtentZ),false);
 VertexIndices[4]:=result.AddVertex(Vector3(-aHalfExtentX,-aHalfExtentY,aHalfExtentZ),false);
 VertexIndices[5]:=result.AddVertex(Vector3(aHalfExtentX,-aHalfExtentY,aHalfExtentZ),false);
 VertexIndices[6]:=result.AddVertex(Vector3(aHalfExtentX,aHalfExtentY,aHalfExtentZ),false);
 VertexIndices[7]:=result.AddVertex(Vector3(-aHalfExtentX,aHalfExtentY,aHalfExtentZ),false);
 // Outward counter clockwise winding on all six faces
 result.AddTriangle(VertexIndices[4],VertexIndices[5],VertexIndices[6]); // +z
 result.AddTriangle(VertexIndices[4],VertexIndices[6],VertexIndices[7]);
 result.AddTriangle(VertexIndices[1],VertexIndices[0],VertexIndices[3]); // -z
 result.AddTriangle(VertexIndices[1],VertexIndices[3],VertexIndices[2]);
 result.AddTriangle(VertexIndices[5],VertexIndices[1],VertexIndices[2]); // +x
 result.AddTriangle(VertexIndices[5],VertexIndices[2],VertexIndices[6]);
 result.AddTriangle(VertexIndices[0],VertexIndices[4],VertexIndices[7]); // -x
 result.AddTriangle(VertexIndices[0],VertexIndices[7],VertexIndices[3]);
 result.AddTriangle(VertexIndices[3],VertexIndices[7],VertexIndices[6]); // +y
 result.AddTriangle(VertexIndices[3],VertexIndices[6],VertexIndices[2]);
 result.AddTriangle(VertexIndices[0],VertexIndices[1],VertexIndices[5]); // -y
 result.AddTriangle(VertexIndices[0],VertexIndices[5],VertexIndices[4]);
 result.Finish;
end;

function MakeStaticMeshFloor(const aPhysics:TKraft):TKraftShape;
var FloorBody:TKraftRigidBody;
begin
 // Thick box mesh floor, top surface at y=0
 FloorBody:=TKraftRigidBody.Create(aPhysics);
 FloorBody.SetRigidBodyType(krbtStatic);
 result:=TKraftShapeMesh.Create(aPhysics,FloorBody,MakeBoxMesh(aPhysics,10.0,0.5,10.0));
 FloorBody.Finish;
 FloorBody.SetWorldTransformation(Matrix4x4Translate(0.0,-0.5,0.0));
 FloorBody.CollisionGroups:=[0];
end;

function MakeDynamicMeshCube(const aPhysics:TKraft;const aX,aY,aZ:TKraftScalar):TKraftRigidBody;
begin
 result:=TKraftRigidBody.Create(aPhysics);
 result.SetRigidBodyType(krbtDynamic);
 TKraftShapeMesh.Create(aPhysics,result,MakeBoxMesh(aPhysics,0.5,0.5,0.5));
 result.Finish;
 result.SetWorldTransformation(Matrix4x4Translate(aX,aY,aZ));
 result.CollisionGroups:=[0];
end;

procedure StepSeconds(const aPhysics:TKraft;const aSeconds:TKraftScalar);
var StepIndex,CountSteps:TKraftInt32;
begin
 CountSteps:=round(aSeconds*60.0);
 for StepIndex:=1 to CountSteps do begin
  aPhysics.Step(1.0/60.0);
 end;
end;

procedure TestDynamicConvexBoxOnMeshFloor;
var Physics:TKraft;
    BoxBody:TKraftRigidBody;
begin
 WriteLn('=== Regression: dynamic convex box resting on static mesh floor ===');
 Physics:=TKraft.Create(-1);
 try
  MakeStaticMeshFloor(Physics);
  BoxBody:=TKraftRigidBody.Create(Physics);
  BoxBody.SetRigidBodyType(krbtDynamic);
  TKraftShapeBox.Create(Physics,BoxBody,Vector3(0.5,0.5,0.5));
  BoxBody.Finish;
  BoxBody.SetWorldTransformation(Matrix4x4Translate(0.0,2.0,0.0));
  BoxBody.CollisionGroups:=[0];
  StepSeconds(Physics,3.0);
  CheckScalar('convex box rest height',BoxBody.Sweep.c.y,0.5,0.05);
 finally
  Physics.Free;
 end;
end;

procedure TestDynamicMeshCubeOnMeshFloor;
var Physics:TKraft;
    CubeBody:TKraftRigidBody;
begin
 WriteLn('=== Dynamic mesh cube resting on static mesh floor ===');
 Physics:=TKraft.Create(-1);
 try
  MakeStaticMeshFloor(Physics);
  CubeBody:=MakeDynamicMeshCube(Physics,0.0,2.0,0.0);
  StepSeconds(Physics,3.0);
  CheckScalar('mesh cube rest height',CubeBody.Sweep.c.y,0.5,0.05);
  CheckScalar('mesh cube resting velocity',Vector3Length(CubeBody.LinearVelocity),0.0,0.1);
  StepSeconds(Physics,5.0);
  Check('mesh cube fell asleep',not (krbfAwake in CubeBody.Flags));
  CheckScalar('mesh cube rest height after sleep',CubeBody.Sweep.c.y,0.5,0.05);
 finally
  Physics.Free;
 end;
end;

procedure TestTwoDynamicMeshCubesStacked;
var Physics:TKraft;
    LowerBody,UpperBody:TKraftRigidBody;
begin
 WriteLn('=== Two dynamic mesh cubes stacked on static mesh floor ===');
 Physics:=TKraft.Create(-1);
 try
  MakeStaticMeshFloor(Physics);
  LowerBody:=MakeDynamicMeshCube(Physics,0.0,0.55,0.0);
  UpperBody:=MakeDynamicMeshCube(Physics,0.0,1.65,0.0);
  StepSeconds(Physics,4.0);
  CheckScalar('lower mesh cube rest height',LowerBody.Sweep.c.y,0.5,0.06);
  CheckScalar('upper mesh cube rest height',UpperBody.Sweep.c.y,1.5,0.1);
  CheckScalar('upper mesh cube lateral drift x',UpperBody.Sweep.c.x,0.0,0.1);
  CheckScalar('upper mesh cube lateral drift z',UpperBody.Sweep.c.z,0.0,0.1);
 finally
  Physics.Free;
 end;
end;

procedure TestRotatedDynamicMeshCube;
var Physics:TKraft;
    CubeBody:TKraftRigidBody;
begin
 WriteLn('=== Dynamic mesh cube rotated 45 degrees around Y resting on mesh floor ===');
 Physics:=TKraft.Create(-1);
 try
  MakeStaticMeshFloor(Physics);
  CubeBody:=TKraftRigidBody.Create(Physics);
  CubeBody.SetRigidBodyType(krbtDynamic);
  TKraftShapeMesh.Create(Physics,CubeBody,MakeBoxMesh(Physics,0.5,0.5,0.5));
  CubeBody.Finish;
  CubeBody.SetWorldTransformation(Matrix4x4TermMul(Matrix4x4RotateY(pi*0.25),Matrix4x4Translate(0.0,1.5,0.0)));
  CubeBody.CollisionGroups:=[0];
  StepSeconds(Physics,3.0);
  CheckScalar('rotated mesh cube rest height',CubeBody.Sweep.c.y,0.5,0.06);
 finally
  Physics.Free;
 end;
end;

procedure TestEdgeEdgeContact;
var Physics:TKraft;
    QueryBody:TKraftRigidBody;
    QueryShape:TKraftShape;
    Contacts:TKraftShapeCollisionContacts;
    CountContacts:TKraftInt32;
begin
 WriteLn('=== Edge-edge: 45/45 degree tilted query mesh cube over mesh floor edge region ===');
 Physics:=TKraft.Create(-1);
 try
  MakeStaticMeshFloor(Physics);
  QueryBody:=TKraftRigidBody.Create(Physics);
  QueryBody.SetRigidBodyType(krbtStatic);
  QueryShape:=TKraftShapeMesh.Create(Physics,QueryBody,MakeBoxMesh(Physics,0.5,0.5,0.5));
  QueryBody.Finish;
  // Tilted so a cube edge points down, lowest edge point at roughly -0.207 below its center at 0.5
  QueryBody.SetWorldTransformation(Matrix4x4TermMul(Matrix4x4RotateZ(pi*0.25),Matrix4x4Translate(0.0,0.5,0.0)));
  QueryBody.CollisionGroups:=[1];
  Contacts:=nil;
  if Physics.CollideShape(QueryShape,Contacts,CountContacts,[0]) and (CountContacts>0) then begin
   Check('edge contact found',true);
   CheckVector('edge contact normal',Contacts[0].Normal,Vector3(0.0,1.0,0.0),0.1);
   CheckScalar('edge contact depth',Contacts[0].PenetrationDepth,0.207,0.05);
  end else begin
   Check('edge contact found',false);
  end;
 finally
  Physics.Free;
 end;
end;

procedure TestFastMeshCube(const aContinuousMode:TKraftContinuousMode;const aModeName:string);
var Physics:TKraft;
    CubeBody:TKraftRigidBody;
begin
 WriteLn('=== Fast dynamic mesh cube versus mesh floor, continuous mode: ',aModeName,' ===');
 Physics:=TKraft.Create(-1);
 try
  Physics.ContinuousMode:=aContinuousMode;
  MakeStaticMeshFloor(Physics);
  CubeBody:=MakeDynamicMeshCube(Physics,0.0,20.0,0.0);
  CubeBody.LinearVelocity:=Vector3(0.0,-200.0,0.0);
  StepSeconds(Physics,1.0);
  Check('fast mesh cube did not tunnel ('+aModeName+')',CubeBody.Sweep.c.y>-2.0);
 finally
  Physics.Free;
 end;
end;

procedure TestFastConvexBox(const aContinuousMode:TKraftContinuousMode;const aModeName:string);
var Physics:TKraft;
    BoxBody:TKraftRigidBody;
begin
 WriteLn('=== Differential: fast dynamic CONVEX box versus mesh floor, continuous mode: ',aModeName,' ===');
 Physics:=TKraft.Create(-1);
 try
  Physics.ContinuousMode:=aContinuousMode;
  MakeStaticMeshFloor(Physics);
  BoxBody:=TKraftRigidBody.Create(Physics);
  BoxBody.SetRigidBodyType(krbtDynamic);
  TKraftShapeBox.Create(Physics,BoxBody,Vector3(0.5,0.5,0.5));
  BoxBody.Finish;
  BoxBody.SetWorldTransformation(Matrix4x4Translate(0.0,20.0,0.0));
  BoxBody.CollisionGroups:=[0];
  BoxBody.LinearVelocity:=Vector3(0.0,-200.0,0.0);
  StepSeconds(Physics,1.0);
  Check('fast convex box did not tunnel ('+aModeName+')',BoxBody.Sweep.c.y>-2.0);
 finally
  Physics.Free;
 end;
end;

procedure TestPushShapeQueryMeshVersusBoxWorld;
var Physics:TKraft;
    FloorBody,QueryBody:TKraftRigidBody;
    QueryShape:TKraftShape;
    Seperation:TKraftVector3;
begin
 WriteLn('=== PushShape: query mesh cube out of static convex box floor ===');
 Physics:=TKraft.Create(-1);
 try
  FloorBody:=TKraftRigidBody.Create(Physics);
  FloorBody.SetRigidBodyType(krbtStatic);
  TKraftShapeBox.Create(Physics,FloorBody,Vector3(10.0,0.5,10.0));
  FloorBody.Finish;
  FloorBody.SetWorldTransformation(Matrix4x4Translate(0.0,-0.5,0.0));
  FloorBody.CollisionGroups:=[0];
  QueryBody:=TKraftRigidBody.Create(Physics);
  QueryBody.SetRigidBodyType(krbtStatic);
  QueryShape:=TKraftShapeMesh.Create(Physics,QueryBody,MakeBoxMesh(Physics,0.5,0.5,0.5));
  QueryBody.Finish;
  QueryBody.SetWorldTransformation(Matrix4x4Translate(0.0,0.3,0.0));
  QueryBody.CollisionGroups:=[1];
  if Physics.PushShape(QueryShape,Seperation,[0]) then begin
   Check('push happened',true);
   CheckVector('push separation',Seperation,Vector3(0.0,0.2,0.0),0.05);
  end else begin
   Check('push happened',false);
  end;
 finally
  Physics.Free;
 end;
end;

procedure TestPushShapeQueryMeshVersusMeshWorld;
var Physics:TKraft;
    QueryBody:TKraftRigidBody;
    QueryShape:TKraftShape;
    Seperation:TKraftVector3;
begin
 WriteLn('=== PushShape: query mesh cube out of static mesh floor ===');
 Physics:=TKraft.Create(-1);
 try
  MakeStaticMeshFloor(Physics);
  QueryBody:=TKraftRigidBody.Create(Physics);
  QueryBody.SetRigidBodyType(krbtStatic);
  QueryShape:=TKraftShapeMesh.Create(Physics,QueryBody,MakeBoxMesh(Physics,0.5,0.5,0.5));
  QueryBody.Finish;
  QueryBody.SetWorldTransformation(Matrix4x4Translate(0.0,0.3,0.0));
  QueryBody.CollisionGroups:=[1];
  if Physics.PushShape(QueryShape,Seperation,[0]) then begin
   Check('push happened',true);
   CheckVector('push separation',Seperation,Vector3(0.0,0.2,0.0),0.05);
  end else begin
   Check('push happened',false);
  end;
 finally
  Physics.Free;
 end;
end;

procedure TestPushShapeQueryMeshVersusSDFWorld;
var Physics:TKraft;
    FloorBody,QueryBody:TKraftRigidBody;
    QueryShape:TKraftShape;
    SDF:TSlabSDF;
    Seperation:TKraftVector3;
begin
 WriteLn('=== PushShape: query mesh cube out of static SDF slab (top y=0.5) ===');
 Physics:=TKraft.Create(-1);
 try
  FloorBody:=TKraftRigidBody.Create(Physics);
  FloorBody.SetRigidBodyType(krbtStatic);
  SDF:=TSlabSDF.Create(Physics,true);
  SDF.Finish;
  TKraftShapeSignedDistanceField.Create(Physics,FloorBody,SDF);
  FloorBody.Finish;
  FloorBody.SetWorldTransformation(Matrix4x4Translate(0.0,0.0,0.0));
  FloorBody.CollisionGroups:=[0];
  QueryBody:=TKraftRigidBody.Create(Physics);
  QueryBody.SetRigidBodyType(krbtStatic);
  QueryShape:=TKraftShapeMesh.Create(Physics,QueryBody,MakeBoxMesh(Physics,0.5,0.5,0.5));
  QueryBody.Finish;
  QueryBody.SetWorldTransformation(Matrix4x4Translate(0.0,0.8,0.0));
  QueryBody.CollisionGroups:=[1];
  if Physics.PushShape(QueryShape,Seperation,[0]) then begin
   Check('push happened',true);
   CheckVector('push separation',Seperation,Vector3(0.0,0.2,0.0),0.06);
  end else begin
   Check('push happened',false);
  end;
 finally
  Physics.Free;
 end;
end;

procedure TestCollideShapeQueryMeshVersusMeshWorld;
var Physics:TKraft;
    FloorShape,QueryShape:TKraftShape;
    QueryBody:TKraftRigidBody;
    Contacts:TKraftShapeCollisionContacts;
    CountContacts:TKraftInt32;
begin
 WriteLn('=== CollideShape: query mesh cube at y=0.3 over static mesh floor (penetration 0.2) ===');
 Physics:=TKraft.Create(-1);
 try
  FloorShape:=MakeStaticMeshFloor(Physics);
  QueryBody:=TKraftRigidBody.Create(Physics);
  QueryBody.SetRigidBodyType(krbtStatic);
  QueryShape:=TKraftShapeMesh.Create(Physics,QueryBody,MakeBoxMesh(Physics,0.5,0.5,0.5));
  QueryBody.Finish;
  QueryBody.SetWorldTransformation(Matrix4x4Translate(0.0,0.3,0.0));
  QueryBody.CollisionGroups:=[1];
  Contacts:=nil;
  if Physics.CollideShape(QueryShape,Contacts,CountContacts,[0]) and (CountContacts>0) then begin
   Check('contact found',true);
   Check('contact with mesh floor',Contacts[0].ShapeB=FloorShape);
   CheckVector('contact normal',Contacts[0].Normal,Vector3(0.0,1.0,0.0),1e-2);
   CheckScalar('contact depth',Contacts[0].PenetrationDepth,0.2,0.03);
  end else begin
   Check('contact found',false);
  end;
 finally
  Physics.Free;
 end;
end;

procedure TestCollideShapeQueryMeshVersusBoxWorld;
var Physics:TKraft;
    FloorBody,QueryBody:TKraftRigidBody;
    FloorShape,QueryShape:TKraftShape;
    Contacts:TKraftShapeCollisionContacts;
    CountContacts:TKraftInt32;
begin
 WriteLn('=== CollideShape: query mesh cube at y=0.3 over static convex box floor (penetration 0.2) ===');
 Physics:=TKraft.Create(-1);
 try
  FloorBody:=TKraftRigidBody.Create(Physics);
  FloorBody.SetRigidBodyType(krbtStatic);
  FloorShape:=TKraftShapeBox.Create(Physics,FloorBody,Vector3(10.0,0.5,10.0));
  FloorBody.Finish;
  FloorBody.SetWorldTransformation(Matrix4x4Translate(0.0,-0.5,0.0));
  FloorBody.CollisionGroups:=[0];
  QueryBody:=TKraftRigidBody.Create(Physics);
  QueryBody.SetRigidBodyType(krbtStatic);
  QueryShape:=TKraftShapeMesh.Create(Physics,QueryBody,MakeBoxMesh(Physics,0.5,0.5,0.5));
  QueryBody.Finish;
  QueryBody.SetWorldTransformation(Matrix4x4Translate(0.0,0.3,0.0));
  QueryBody.CollisionGroups:=[1];
  Contacts:=nil;
  if Physics.CollideShape(QueryShape,Contacts,CountContacts,[0]) and (CountContacts>0) then begin
   Check('contact found',true);
   Check('contact with box floor',Contacts[0].ShapeB=FloorShape);
   CheckVector('contact normal',Contacts[0].Normal,Vector3(0.0,1.0,0.0),1e-2);
   CheckScalar('contact depth',Contacts[0].PenetrationDepth,0.2,0.03);
  end else begin
   Check('contact found',false);
  end;
 finally
  Physics.Free;
 end;
end;

begin
 FormatSettings.DecimalSeparator:='.';
 SetExceptionMask([exInvalidOp,exDenormalized,exZeroDivide,exOverflow,exUnderflow,exPrecision]);
 CountPassed:=0;
 CountFailed:=0;
 TestDynamicConvexBoxOnMeshFloor;
 TestDynamicMeshCubeOnMeshFloor;
 TestTwoDynamicMeshCubesStacked;
 TestRotatedDynamicMeshCube;
 TestEdgeEdgeContact;
 TestFastMeshCube(kcmSpeculativeContacts,'speculative');
 TestFastMeshCube(kcmMotionClamping,'motion clamping');
 TestFastConvexBox(kcmSpeculativeContacts,'speculative');
 TestFastConvexBox(kcmMotionClamping,'motion clamping');
 TestPushShapeQueryMeshVersusBoxWorld;
 TestPushShapeQueryMeshVersusMeshWorld;
 TestPushShapeQueryMeshVersusSDFWorld;
 TestCollideShapeQueryMeshVersusMeshWorld;
 TestCollideShapeQueryMeshVersusBoxWorld;
 WriteLn('=== ',CountPassed,' passed, ',CountFailed,' failed ===');
 if CountFailed>0 then begin
  Halt(1);
 end;
end.
