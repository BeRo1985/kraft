program terraintest;
{$ifdef fpc}
 {$mode delphi}
{$endif}
{$apptype console}
uses {$ifdef unix}cthreads,{$endif} SysUtils,Math,kraft;

var CountPassed,CountFailed:longint;

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

// Ramp terrain with slope 0.5 along x: h(x) = 0.5*x, over a 16x16 area
function MakeRampHeights(const aCount:TKraftInt32;const aSize,aSlope:TKraftScalar):TKraftScalarArray;
var x,z:TKraftInt32;
    WorldX:TKraftScalar;
begin
 result:=nil;
 SetLength(result,aCount*aCount);
 for z:=0 to aCount-1 do begin
  for x:=0 to aCount-1 do begin
   WorldX:=((x*(aSize/(aCount-1)))-(aSize*0.5));
   result[(z*aCount)+x]:=aSlope*WorldX;
  end;
 end;
end;

procedure TestAnalytics;
var Physics:TKraft;
    Terrain:TKraftSignedDistanceFieldTerrain;
    Heights:TKraftScalarArray;
    ExpectedDistance,Slope:TKraftScalar;
    ExpectedNormal:TKraftVector3;
begin
 WriteLn('=== analytics on a slope 0.5 ramp terrain ===');
 Physics:=TKraft.Create(-1);
 try
  Slope:=0.5;
  Heights:=MakeRampHeights(17,16.0,Slope);
  Terrain:=TKraftSignedDistanceFieldTerrain.Create(Physics,17,17,16.0,16.0,@Heights[0]);
  Terrain.Finish;
  CheckScalar('terrain height at x=2',Terrain.GetTerrainHeight(2.0,0.0),1.0,1e-4);
  CheckScalar('terrain height at x=-4',Terrain.GetTerrainHeight(-4.0,1.5),-2.0,1e-4);
  // True distance from a point to the inclined plane = (y-h)/sqrt(1+slope^2), thanks to the Lipschitz scale
  ExpectedDistance:=(3.0-1.0)/sqrt(1.0+sqr(Slope));
  CheckScalar('signed distance is plane exact',Terrain.GetLocalSignedDistance(Vector3(2.0,3.0,0.0)),ExpectedDistance,1e-4);
  ExpectedNormal:=Vector3Norm(Vector3(-Slope,1.0,0.0));
  CheckVector('analytic normal',Terrain.GetLocalSignedDistanceNormalizedGradient(Vector3(2.0,3.0,0.0)),ExpectedNormal,1e-4);
  CheckVector('closest point on plane',Terrain.GetLocalClosestPointTo(Vector3(2.0,3.0,0.0)),
              Vector3Sub(Vector3(2.0,3.0,0.0),Vector3ScalarMul(ExpectedNormal,ExpectedDistance)),1e-3);
 finally
  Physics.Free;
 end;
end;

procedure TestSteepRayCast;
var Physics:TKraft;
    FloorBody:TKraftRigidBody;
    Terrain:TKraftSignedDistanceFieldTerrain;
    Heights:TKraftScalarArray;
    HitShape:TKraftShape;
    HitTime,SurfaceHeight:TKraftScalar;
    HitPoint,HitNormal:TKraftVector3;
    OK:boolean;
begin
 WriteLn('=== ray casts on a steep slope 4 ramp (Lipschitz correctness) ===');
 Physics:=TKraft.Create(-1);
 try
  Heights:=MakeRampHeights(17,16.0,4.0);
  FloorBody:=TKraftRigidBody.Create(Physics);
  FloorBody.SetRigidBodyType(krbtSTATIC);
  Terrain:=TKraftSignedDistanceFieldTerrain.Create(Physics,17,17,16.0,16.0,@Heights[0]);
  Terrain.Finish;
  TKraftShapeSignedDistanceField.Create(Physics,FloorBody,Terrain);
  FloorBody.Finish;
  FloorBody.SetWorldTransformation(Matrix4x4Translate(0.0,0.0,0.0));
  FloorBody.CollisionGroups:=[0];
  // Vertical ray onto the steep face: hit exactly at the surface height
  OK:=Physics.RayCast(Vector3(2.0,40.0,0.0),Vector3(0.0,-1.0,0.0),64.0,HitShape,HitTime,HitPoint,HitNormal,[0]);
  Check('vertical ray hits',OK);
  if OK then begin
   CheckScalar('vertical ray hit y = surface',HitPoint.y,8.0,1e-2);
   CheckScalar('vertical ray time',HitTime,32.0,1e-2);
  end;
  // Shallow diagonal ray against the slope: the hit point has to lie on the surface
  OK:=Physics.RayCast(Vector3(-6.0,20.0,0.0),Vector3Norm(Vector3(0.5,-1.0,0.0)),64.0,HitShape,HitTime,HitPoint,HitNormal,[0]);
  Check('diagonal ray hits',OK);
  if OK then begin
   SurfaceHeight:=Terrain.GetTerrainHeight(HitPoint.x,HitPoint.z);
   CheckScalar('diagonal ray hit on surface',HitPoint.y-SurfaceHeight,0.0,2e-2);
  end;
  // Sphere cast down onto the steep face
  OK:=Physics.SphereCast(Vector3(2.0,40.0,0.0),0.25,Vector3(0.0,-1.0,0.0),64.0,HitShape,HitTime,HitPoint,HitNormal,[0]);
  Check('sphere cast hits',OK);
  if OK then begin
   SurfaceHeight:=Terrain.GetTerrainHeight(HitPoint.x,HitPoint.z);
   CheckScalar('sphere cast hit on surface',HitPoint.y-SurfaceHeight,0.0,2e-2);
  end;
 finally
  Physics.Free;
 end;
end;

procedure TestDropAndQueries;
var Physics:TKraft;
    FloorBody,Body:TKraftRigidBody;
    Terrain:TKraftSignedDistanceFieldTerrain;
    Heights:TKraftScalarArray;
    CastBody:TKraftRigidBody;
    CastShape:TKraftShape;
    HitShape:TKraftShape;
    HitTime:TKraftScalar;
    HitPoint,HitNormal:TKraftVector3;
    PushCenter:TKraftVector3;
    StepIndex:longint;
    OK:boolean;
begin
 WriteLn('=== drop, shape cast, CCD and push on a bumpy terrain (flat center) ===');
 Physics:=TKraft.Create(-1);
 try
  Physics.SetFrequency(120.0);
  Physics.Gravity.y:=-9.81;
  Physics.ContinuousMode:=kcmSpeculativeContacts;
  // Bumpy border, flat center at height 1.0
  Heights:=MakeRampHeights(33,32.0,0.0);
  for StepIndex:=0 to (33*33)-1 do begin
   Heights[StepIndex]:=1.0;
  end;
  Heights[0]:=5.0;
  Heights[32]:=5.0;
  FloorBody:=TKraftRigidBody.Create(Physics);
  FloorBody.SetRigidBodyType(krbtSTATIC);
  Terrain:=TKraftSignedDistanceFieldTerrain.Create(Physics,33,33,32.0,32.0,@Heights[0]);
  Terrain.Finish;
  TKraftShapeSignedDistanceField.Create(Physics,FloorBody,Terrain);
  FloorBody.Finish;
  FloorBody.SetWorldTransformation(Matrix4x4Translate(0.0,0.0,0.0));
  FloorBody.CollisionGroups:=[0];

  // Fast sphere with speculative CCD onto the terrain
  Body:=TKraftRigidBody.Create(Physics);
  Body.SetRigidBodyType(krbtDYNAMIC);
  TKraftShapeSphere.Create(Physics,Body,0.25).Density:=1.0;
  Body.Finish;
  Body.SetWorldTransformation(Matrix4x4Translate(0.0,9.25,0.0));
  Body.LinearVelocity:=Vector3(0.0,-240.0,0.0);
  Body.CollisionGroups:=[0];
  for StepIndex:=1 to 240 do begin
   Physics.Step(1.0/120.0);
  end;
  CheckScalar('fast sphere rests on terrain',Body.Sweep.c.y,1.25,1e-2);

  // Shape cast a box down onto the terrain
  CastBody:=TKraftRigidBody.Create(Physics);
  CastBody.SetRigidBodyType(krbtSTATIC);
  CastShape:=TKraftShapeBox.Create(Physics,CastBody,Vector3(0.25,0.25,0.25));
  CastBody.Finish;
  CastBody.SetWorldTransformation(Matrix4x4Translate(3.0,8.0,3.0));
  CastBody.CollisionGroups:=[1];
  OK:=Physics.ShapeCast(CastShape,Vector3(0.0,-1.0,0.0),32.0,HitShape,HitTime,HitPoint,HitNormal,[0]);
  Check('shape cast hits terrain',OK);
  if OK then begin
   CheckScalar('shape cast time',HitTime,8.0-(1.0+0.25),1e-2);
   CheckVector('shape cast normal',HitNormal,Vector3(0.0,1.0,0.0),1e-2);
  end;

  // Push a sphere out of the terrain
  PushCenter:=Vector3(-3.0,0.9,-3.0);
  Check('push sphere out of terrain',Physics.PushSphere(PushCenter,0.25,[0]));
  CheckScalar('pushed to surface plus radius',PushCenter.y,1.25,1e-2);

 finally
  Physics.Free;
 end;
end;

procedure TestMeshFactory;
var Physics:TKraft;
    FloorBody,Body:TKraftRigidBody;
    Mesh:TKraftMesh;
    MeshShape:TKraftShapeMesh;
    Heights:TKraftScalarArray;
    HitShape:TKraftShape;
    HitTime:TKraftScalar;
    HitPoint,HitNormal:TKraftVector3;
    StepIndex:longint;
    OK:boolean;
begin
 WriteLn('=== TKraftMesh.AddHeightField factory: flat grid at height 1.0 ===');
 Physics:=TKraft.Create(-1);
 try
  Physics.SetFrequency(120.0);
  Physics.Gravity.y:=-9.81;
  Heights:=nil;
  SetLength(Heights,33*33);
  for StepIndex:=0 to (33*33)-1 do begin
   Heights[StepIndex]:=1.0;
  end;
  Mesh:=TKraftMesh.Create(Physics);
  Mesh.AddHeightField(Heights,33,33,32.0,32.0);
  Mesh.Finish;
  FloorBody:=TKraftRigidBody.Create(Physics);
  FloorBody.SetRigidBodyType(krbtSTATIC);
  MeshShape:=TKraftShapeMesh.Create(Physics,FloorBody,Mesh);
  MeshShape.Finish;
  FloorBody.Finish;
  FloorBody.SetWorldTransformation(Matrix4x4Translate(0.0,0.0,0.0));
  FloorBody.CollisionGroups:=[0];
  OK:=Physics.RayCast(Vector3(2.0,8.0,2.0),Vector3(0.0,-1.0,0.0),32.0,HitShape,HitTime,HitPoint,HitNormal,[0]);
  Check('ray hits mesh terrain',OK and (HitShape=MeshShape));
  if OK then begin
   CheckScalar('ray hit y',HitPoint.y,1.0,1e-3);
  end;
  Body:=TKraftRigidBody.Create(Physics);
  Body.SetRigidBodyType(krbtDYNAMIC);
  TKraftShapeSphere.Create(Physics,Body,0.25).Density:=1.0;
  Body.Finish;
  Body.SetWorldTransformation(Matrix4x4Translate(0.0,3.0,0.0));
  Body.CollisionGroups:=[0];
  for StepIndex:=1 to 480 do begin
   Physics.Step(1.0/120.0);
  end;
  CheckScalar('sphere rests on mesh terrain',Body.Sweep.c.y,1.25,2e-2);
 finally
  Physics.Free;
 end;
end;

procedure TestWallCorridor;
var Physics:TKraft;
    FloorBody:TKraftRigidBody;
    Terrain:TKraftSignedDistanceFieldTerrain;
    Heights:TKraftScalarArray;
    HitShape:TKraftShape;
    HitTime:TKraftScalar;
    HitPoint,HitNormal:TKraftVector3;
    Index:longint;
    OK:boolean;
begin
 WriteLn('=== ridge wall beside the cast track (lateral corridor case) ===');
 Physics:=TKraft.Create(-1);
 try
  // Flat h=0 terrain with a ridge wall of height 8 along the z=0 row for x>=0 only
  Heights:=nil;
  SetLength(Heights,33*33);
  for Index:=0 to (33*33)-1 do begin
   Heights[Index]:=0.0;
  end;
  for Index:=16 to 32 do begin
   Heights[(16*33)+Index]:=8.0;
  end;
  FloorBody:=TKraftRigidBody.Create(Physics);
  FloorBody.SetRigidBodyType(krbtSTATIC);
  Terrain:=TKraftSignedDistanceFieldTerrain.Create(Physics,33,33,32.0,32.0,@Heights[0]);
  Terrain.Finish;
  TKraftShapeSignedDistanceField.Create(Physics,FloorBody,Terrain);
  FloorBody.Finish;
  FloorBody.SetWorldTransformation(Matrix4x4Translate(0.0,0.0,0.0));
  FloorBody.CollisionGroups:=[0];
  // The sphere center track runs at z=1.5 over cells that stay flat, well above the ground (bottom at
  // y=2), and the wall flank plane (y=8-8z over z in 0..1) sits 0.99 away from the track, so the hit can
  // only come from the corridor cells one row beside the track column
  OK:=Physics.SphereCast(Vector3(-14.0,4.0,1.5),2.0,Vector3(1.0,0.0,0.0),64.0,HitShape,HitTime,HitPoint,HitNormal,[0]);
  Check('sphere cast hits the wall over the corridor',OK);
  if OK then begin
   Check('wall hit around the wall start',(HitTime>=10.0) and (HitTime<=14.0));
   Check('wall hit point in the flank row',(HitPoint.z>=0.0) and (HitPoint.z<=1.5));
  end;
  // The same track without the wall in reach (z=4.5) has to fly through
  Check('sphere cast beside the wall misses',not Physics.SphereCast(Vector3(-14.0,4.0,4.5),2.0,Vector3(1.0,0.0,0.0),64.0,HitShape,HitTime,HitPoint,HitNormal,[0]));
 finally
  Physics.Free;
 end;
end;

procedure TestSpikeTunneling;
var Physics:TKraft;
    FloorBody:TKraftRigidBody;
    Terrain:TKraftSignedDistanceFieldTerrain;
    Heights:TKraftScalarArray;
    HitShape:TKraftShape;
    HitTime:TKraftScalar;
    HitPoint,HitNormal:TKraftVector3;
    Index:longint;
    OK:boolean;
begin
 WriteLn('=== single-cell spike on flat ground (tunneling hazard cases) ===');
 Physics:=TKraft.Create(-1);
 try
  // Flat h=0 terrain, 33x33 over 32x32 (1x1 cells), with one spike of height 8 in the middle
  Heights:=nil;
  SetLength(Heights,33*33);
  for Index:=0 to (33*33)-1 do begin
   Heights[Index]:=0.0;
  end;
  Heights[(16*33)+16]:=8.0;
  FloorBody:=TKraftRigidBody.Create(Physics);
  FloorBody.SetRigidBodyType(krbtSTATIC);
  Terrain:=TKraftSignedDistanceFieldTerrain.Create(Physics,33,33,32.0,32.0,@Heights[0]);
  Terrain.Finish;
  TKraftShapeSignedDistanceField.Create(Physics,FloorBody,Terrain);
  FloorBody.Finish;
  FloorBody.SetWorldTransformation(Matrix4x4Translate(0.0,0.0,0.0));
  FloorBody.CollisionGroups:=[0];
  // Shallow horizontal ray over the flat ground straight into the spike flank; the flank at z=0 rises
  // linearly from (-1,0) to the apex (0,8), so a ray at height 6 has to hit at x=-0.25
  OK:=Physics.RayCast(Vector3(-14.0,6.0,0.0),Vector3(1.0,0.0,0.0),64.0,HitShape,HitTime,HitPoint,HitNormal,[0]);
  Check('grazing ray hits the spike',OK);
  if OK then begin
   CheckScalar('spike hit x',HitPoint.x,-0.25,1e-2);
   CheckScalar('spike hit y',HitPoint.y,6.0,1e-2);
  end;
  // The same ray one unit above the apex has to fly through
  Check('ray over the apex misses',not Physics.RayCast(Vector3(-14.0,9.0,0.0),Vector3(1.0,0.0,0.0),64.0,HitShape,HitTime,HitPoint,HitNormal,[0]));
  // Grazing sphere cast into the spike flank
  OK:=Physics.SphereCast(Vector3(-14.0,6.0,0.15),0.5,Vector3(1.0,0.0,0.0),64.0,HitShape,HitTime,HitPoint,HitNormal,[0]);
  Check('grazing sphere cast hits the spike',OK);
  if OK then begin
   Check('grazing sphere cast hit near the flank',(HitPoint.x>=-1.5) and (HitPoint.x<=0.0));
  end;
  // Sphere cast that already overlaps the flat ground at the start
  OK:=Physics.SphereCast(Vector3(8.0,0.1,8.0),0.25,Vector3(0.0,-1.0,0.0),64.0,HitShape,HitTime,HitPoint,HitNormal,[0]);
  Check('initial overlap sphere cast hits',OK);
  if OK then begin
   CheckScalar('initial overlap time is zero',HitTime,0.0,1e-4);
  end;
  // Ray starting inside the solid below the surface
  OK:=Physics.RayCast(Vector3(8.0,-0.5,8.0),Vector3(0.0,1.0,0.0),64.0,HitShape,HitTime,HitPoint,HitNormal,[0]);
  Check('ray from inside the solid hits',OK);
  if OK then begin
   CheckScalar('inside ray time is zero',HitTime,0.0,1e-4);
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
 TestAnalytics;
 TestSteepRayCast;
 TestDropAndQueries;
 TestMeshFactory;
 TestSpikeTunneling;
 TestWallCorridor;
 WriteLn('=== ',CountPassed,' passed, ',CountFailed,' failed ===');
 if CountFailed>0 then begin
  Halt(1);
 end;
end.
