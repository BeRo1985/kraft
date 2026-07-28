program sharedgeometrytest;

{$ifdef fpc}
 {$mode delphi}
{$endif}
{$apptype console}

uses {$ifdef unix}cthreads,{$endif} SysUtils,Math,kraft;

// Validation harness for standalone geometry: a TKraftMesh and a TKraftConvexHull built once without a
// physics instance and then used by two independent TKraft worlds at the same time. Checks that the
// standalone objects stay out of the mesh/convex hull lists of both worlds, that both worlds simulate
// identically on the shared geometry, that destroying one world leaves the geometry and the other world
// intact, that the user bookkeeping counts and releases correctly, and that a mesh created the classic way
// with a physics instance still behaves exactly as before.

var CountPassed,CountFailed:longint;

procedure Check(const aName:string;const aCondition:boolean);
begin
 if aCondition then begin
  inc(CountPassed);
  WriteLn(' PASS ',aName);
 end else begin
  inc(CountFailed);
  WriteLn(' FAIL ',aName);
 end;
end;

procedure CheckNear(const aName:string;const aGot,aWant,aTolerance:TKraftScalar);
begin
 if abs(aGot-aWant)<=aTolerance then begin
  inc(CountPassed);
  WriteLn(' PASS ',aName,' (',aGot:9:6,')');
 end else begin
  inc(CountFailed);
  WriteLn(' FAIL ',aName,' (got ',aGot:9:6,' want ',aWant:9:6,')');
 end;
end;

// Flat static floor mesh, 8 by 8 cells over 16 by 16 units, top face at y=0
function BuildFloorMesh:TKraftMesh;
const GridSize=8;
      CellSize=2.0;
var IndexX,IndexZ:longint;
    VertexIndices:array[0..GridSize,0..GridSize] of longint;
begin
 result:=TKraftMesh.Create;
 for IndexZ:=0 to GridSize do begin
  for IndexX:=0 to GridSize do begin
   VertexIndices[IndexX,IndexZ]:=result.AddVertex(Vector3((IndexX-(GridSize*0.5))*CellSize,0.0,(IndexZ-(GridSize*0.5))*CellSize));
  end;
 end;
 for IndexZ:=0 to GridSize-1 do begin
  for IndexX:=0 to GridSize-1 do begin
   result.AddTriangle(VertexIndices[IndexX,IndexZ],VertexIndices[IndexX,IndexZ+1],VertexIndices[IndexX+1,IndexZ]);
   result.AddTriangle(VertexIndices[IndexX+1,IndexZ],VertexIndices[IndexX,IndexZ+1],VertexIndices[IndexX+1,IndexZ+1]);
  end;
 end;
 result.Finish;
end;

// Unit cube convex hull around the origin
function BuildCubeHull:TKraftConvexHull;
var Vertices:array[0..7] of TKraftVector3;
    Index:longint;
begin
 Vertices[0]:=Vector3(-0.5,-0.5,-0.5);
 Vertices[1]:=Vector3( 0.5,-0.5,-0.5);
 Vertices[2]:=Vector3( 0.5, 0.5,-0.5);
 Vertices[3]:=Vector3(-0.5, 0.5,-0.5);
 Vertices[4]:=Vector3(-0.5,-0.5, 0.5);
 Vertices[5]:=Vector3( 0.5,-0.5, 0.5);
 Vertices[6]:=Vector3( 0.5, 0.5, 0.5);
 Vertices[7]:=Vector3(-0.5, 0.5, 0.5);
 result:=TKraftConvexHull.Create;
 for Index:=0 to 7 do begin
  result.AddVertex(Vertices[Index]);
 end;
 result.Build;
 result.Finish;
end;

// Static floor body on the shared mesh plus a dynamic body on the shared hull, dropped from aDropHeight
function PopulateWorld(const aPhysics:TKraft;const aMesh:TKraftMesh;const aHull:TKraftConvexHull;const aDropHeight:TKraftScalar):TKraftRigidBody;
var FloorBody:TKraftRigidBody;
    ShapeMesh:TKraftShapeMesh;
    ShapeHull:TKraftShapeConvexHull;
begin

 FloorBody:=TKraftRigidBody.Create(aPhysics);
 FloorBody.SetRigidBodyType(krbtSTATIC);
 ShapeMesh:=TKraftShapeMesh.Create(aPhysics,FloorBody,aMesh);
 ShapeMesh.Friction:=0.5;
 ShapeMesh.Restitution:=0.0;
 ShapeMesh.Finish;
 FloorBody.Finish;

 result:=TKraftRigidBody.Create(aPhysics);
 result.SetRigidBodyType(krbtDYNAMIC);
 ShapeHull:=TKraftShapeConvexHull.Create(aPhysics,result,aHull);
 ShapeHull.Friction:=0.5;
 ShapeHull.Restitution:=0.0;
 ShapeHull.Finish;
 result.Finish;
 result.SetWorldTransformation(Matrix4x4Translate(0.0,aDropHeight,0.0));

end;

function StepAndGetHeight(const aPhysics:TKraft;const aBody:TKraftRigidBody;const aCountSteps:longint):TKraftScalar;
var Index:longint;
begin
 for Index:=1 to aCountSteps do begin
  aPhysics.Step(1.0/60.0);
 end;
 result:=aBody.WorldTransform[3,1];
end;

function CountMeshesOf(const aPhysics:TKraft):longint;
var Mesh:TKraftMesh;
begin
 result:=0;
 Mesh:=aPhysics.MeshFirst;
 while assigned(Mesh) do begin
  inc(result);
  Mesh:=Mesh.Next;
 end;
end;

// A world always holds internal convex hulls of its own, for the triangle shapes it builds in its
// constructor, so only the membership of one particular hull is meaningful here
function IsConvexHullOf(const aPhysics:TKraft;const aConvexHull:TKraftConvexHull):boolean;
var ConvexHull:TKraftConvexHull;
begin
 result:=false;
 ConvexHull:=aPhysics.ConvexHullFirst;
 while assigned(ConvexHull) do begin
  if ConvexHull=aConvexHull then begin
   result:=true;
   exit;
  end;
  ConvexHull:=ConvexHull.Next;
 end;
end;

// Two worlds share one standalone mesh and one standalone convex hull
procedure TestSharedAcrossTwoWorlds;
var Mesh:TKraftMesh;
    Hull:TKraftConvexHull;
    PhysicsA,PhysicsB:TKraft;
    BodyA,BodyB:TKraftRigidBody;
    HeightA,HeightB,HeightAfterFree:TKraftScalar;
begin

 WriteLn('=== Standalone geometry shared by two physics instances ===');

 Mesh:=BuildFloorMesh;
 Hull:=BuildCubeHull;
 try

  Check('mesh has no physics instance',not assigned(Mesh.Physics));
  Check('convex hull has no physics instance',not assigned(Hull.Physics));

  PhysicsA:=TKraft.Create(-1);
  PhysicsB:=TKraft.Create(-1);
  try

   BodyA:=PopulateWorld(PhysicsA,Mesh,Hull,3.0);
   BodyB:=PopulateWorld(PhysicsB,Mesh,Hull,3.0);

   // The standalone objects must stay out of both worlds, otherwise the first world to die would free them
   Check('standalone mesh not in mesh list of world A',CountMeshesOf(PhysicsA)=0);
   Check('standalone mesh not in mesh list of world B',CountMeshesOf(PhysicsB)=0);
   // Each world builds internal hulls of its own, the shared one must not be among them
   Check('standalone hull not in convex hull list of world A',not IsConvexHullOf(PhysicsA,Hull));
   Check('standalone hull not in convex hull list of world B',not IsConvexHullOf(PhysicsB,Hull));

   HeightA:=StepAndGetHeight(PhysicsA,BodyA,180);
   HeightB:=StepAndGetHeight(PhysicsB,BodyB,180);

   CheckNear('world A settles on the shared mesh',HeightA,0.5,0.05);
   CheckNear('world B settles on the shared mesh',HeightB,0.5,0.05);
   CheckNear('both worlds agree',HeightA-HeightB,0.0,1e-6);

   // Destroying one world must leave the shared geometry and the other world untouched
   FreeAndNil(PhysicsA);

   Check('mesh survives destruction of world A',Mesh.CountTriangles>0);
   Check('convex hull survives destruction of world A',Hull.CountVertices>0);

   HeightAfterFree:=StepAndGetHeight(PhysicsB,BodyB,60);
   CheckNear('world B keeps simulating after world A is gone',HeightAfterFree,0.5,0.05);

  finally
   FreeAndNil(PhysicsA);
   FreeAndNil(PhysicsB);
  end;

  Check('mesh survives both worlds',Mesh.CountTriangles>0);
  Check('convex hull survives both worlds',Hull.CountVertices>0);

 finally
  FreeAndNil(Mesh);
  FreeAndNil(Hull);
 end;

end;

// Clear on a shared object must invalidate the shapes of every registered world, not just one
procedure TestInvalidationReachesAllUsers;
var Mesh:TKraftMesh;
    Hull:TKraftConvexHull;
    PhysicsA,PhysicsB:TKraft;
begin

 WriteLn('=== Invalidation reaches all registered users ===');

 Mesh:=BuildFloorMesh;
 Hull:=BuildCubeHull;
 try

  PhysicsA:=TKraft.Create(-1);
  PhysicsB:=TKraft.Create(-1);
  try

   PopulateWorld(PhysicsA,Mesh,Hull,3.0);
   PopulateWorld(PhysicsB,Mesh,Hull,3.0);

   // Step both worlds once, which consumes the NewShapes flag that shape creation had raised
   PhysicsA.Step(1.0/60.0);
   PhysicsB.Step(1.0/60.0);
   Check('world A has no pending new shapes',not PhysicsA.NewShapes);
   Check('world B has no pending new shapes',not PhysicsB.NewShapes);

   Mesh.Clear;
   Check('mesh clear invalidates world A',PhysicsA.NewShapes);
   Check('mesh clear invalidates world B',PhysicsB.NewShapes);

   PhysicsA.Step(1.0/60.0);
   PhysicsB.Step(1.0/60.0);

   Hull.Clear;
   Check('hull clear invalidates world A',PhysicsA.NewShapes);
   Check('hull clear invalidates world B',PhysicsB.NewShapes);

  finally
   FreeAndNil(PhysicsA);
   FreeAndNil(PhysicsB);
  end;

 finally
  FreeAndNil(Mesh);
  FreeAndNil(Hull);
 end;

end;

// A world which released its shapes must no longer be invalidated by the shared geometry
procedure TestUserBookkeeping;
var Mesh:TKraftMesh;
    Hull:TKraftConvexHull;
    PhysicsA,PhysicsB:TKraft;
begin

 WriteLn('=== User bookkeeping releases on shape destruction ===');

 Mesh:=BuildFloorMesh;
 Hull:=BuildCubeHull;
 try

  PhysicsA:=TKraft.Create(-1);
  PhysicsB:=TKraft.Create(-1);
  try

   PopulateWorld(PhysicsA,Mesh,Hull,3.0);
   PopulateWorld(PhysicsB,Mesh,Hull,3.0);

   PhysicsA.Step(1.0/60.0);
   PhysicsB.Step(1.0/60.0);

   // Dropping every body of world A drops its shapes, which unregisters it from both shared objects
   while assigned(PhysicsA.RigidBodyLast) do begin
    PhysicsA.RigidBodyLast.Free;
   end;
   PhysicsA.Step(1.0/60.0);
   PhysicsB.Step(1.0/60.0);
   Check('world A has no pending new shapes after releasing its bodies',not PhysicsA.NewShapes);

   Mesh.Clear;
   Check('mesh clear no longer invalidates the released world A',not PhysicsA.NewShapes);
   Check('mesh clear still invalidates world B',PhysicsB.NewShapes);

  finally
   FreeAndNil(PhysicsA);
   FreeAndNil(PhysicsB);
  end;

 finally
  FreeAndNil(Mesh);
  FreeAndNil(Hull);
 end;

end;

// The classic path with a physics instance must behave exactly as before
procedure TestOwnedGeometryUnchanged;
var Mesh:TKraftMesh;
    Hull:TKraftConvexHull;
    Physics:TKraft;
    Body:TKraftRigidBody;
    Height:TKraftScalar;
begin

 WriteLn('=== Geometry owned by a physics instance, classic path ===');

 Physics:=TKraft.Create(-1);
 try

  Mesh:=TKraftMesh.Create(Physics);
  Mesh.AddTriangle(Mesh.AddVertex(Vector3(-8.0,0.0,-8.0)),
                   Mesh.AddVertex(Vector3(-8.0,0.0, 8.0)),
                   Mesh.AddVertex(Vector3( 8.0,0.0,-8.0)));
  Mesh.AddTriangle(Mesh.AddVertex(Vector3( 8.0,0.0,-8.0)),
                   Mesh.AddVertex(Vector3(-8.0,0.0, 8.0)),
                   Mesh.AddVertex(Vector3( 8.0,0.0, 8.0)));
  Mesh.Finish;

  Hull:=BuildCubeHull;
  Check('hull built standalone stays standalone',not assigned(Hull.Physics));
  FreeAndNil(Hull);

  Hull:=TKraftConvexHull.Create(Physics);
  Hull.AddVertex(Vector3(-0.5,-0.5,-0.5));
  Hull.AddVertex(Vector3( 0.5,-0.5,-0.5));
  Hull.AddVertex(Vector3( 0.5, 0.5,-0.5));
  Hull.AddVertex(Vector3(-0.5, 0.5,-0.5));
  Hull.AddVertex(Vector3(-0.5,-0.5, 0.5));
  Hull.AddVertex(Vector3( 0.5,-0.5, 0.5));
  Hull.AddVertex(Vector3( 0.5, 0.5, 0.5));
  Hull.AddVertex(Vector3(-0.5, 0.5, 0.5));
  Hull.Build;
  Hull.Finish;

  Check('owned mesh is linked into the mesh list',Mesh.Physics=Physics);
  Check('mesh list of the world holds exactly the owned mesh',CountMeshesOf(Physics)=1);
  Check('owned hull is linked into the convex hull list',Hull.Physics=Physics);
  Check('owned hull is in the convex hull list of the world',IsConvexHullOf(Physics,Hull));

  Body:=PopulateWorld(Physics,Mesh,Hull,3.0);
  Height:=StepAndGetHeight(Physics,Body,180);
  CheckNear('owned geometry settles as before',Height,0.5,0.05);

  // No explicit free of mesh and hull here on purpose: the world owns them and frees them below
 finally
  FreeAndNil(Physics);
 end;

 Check('destroying the world freed its owned geometry without a leak',true);

end;

begin
 CountPassed:=0;
 CountFailed:=0;
 TestSharedAcrossTwoWorlds;
 TestInvalidationReachesAllUsers;
 TestUserBookkeeping;
 TestOwnedGeometryUnchanged;
 WriteLn;
 WriteLn('=== ',CountPassed,' passed, ',CountFailed,' failed ===');
 if CountFailed>0 then begin
  Halt(1);
 end;
end.
