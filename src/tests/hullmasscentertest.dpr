program hullmasscentertest;

{$ifdef fpc}
 {$mode delphi}
{$endif}
{$apptype console}

uses {$ifdef unix}cthreads,{$endif} SysUtils,Classes,Math,kraft;

// The mass center of a convex hull or of a closed mesh whose origin is not at its centroid, such as a
// bowling pin modelled with its pivot at the foot. The mass center has to be the centroid and the body
// inertia the one about it, also through a shape local transform and a uniform scale, checked against
// the analytic values of a box standing on its origin. A pin standing upright stays upright, one tilted
// past its tipping angle falls over instead of righting itself again around a mass center down at its
// foot. Hull streams which still carry the origin as mass center get the centroid on loading.

const BoxHalfWidth=0.25;
      BoxHeight=1.0;
      // 0.5 by 1 by 0.5 at unit density
      BoxMass=0.25;
      BoxInertiaX=(BoxMass/12.0)*((BoxHeight*BoxHeight)+(4.0*BoxHalfWidth*BoxHalfWidth));
      BoxInertiaY=(BoxMass/12.0)*(8.0*BoxHalfWidth*BoxHalfWidth);

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

procedure CheckVector(const aName:string;const aGot,aWant:TKraftVector3;const aTolerance:TKraftScalar);
begin
 if (abs(aGot.x-aWant.x)<=aTolerance) and (abs(aGot.y-aWant.y)<=aTolerance) and (abs(aGot.z-aWant.z)<=aTolerance) then begin
  inc(CountPassed);
  WriteLn(' PASS ',aName,' (',aGot.x:8:5,',',aGot.y:8:5,',',aGot.z:8:5,')');
 end else begin
  inc(CountFailed);
  WriteLn(' FAIL ',aName,' (got ',aGot.x:8:5,',',aGot.y:8:5,',',aGot.z:8:5,' want ',aWant.x:8:5,',',aWant.y:8:5,',',aWant.z:8:5,')');
 end;
end;

// All nine entries, relative to the largest diagonal entry of the wanted tensor
procedure CheckTensor(const aName:string;const aGot,aWant:TKraftMatrix3x3;const aRelativeTolerance:TKraftScalar);
var Row,Column:longint;
    Scale,MaxError:TKraftScalar;
begin
 Scale:=Max(Max(abs(aWant[0,0]),abs(aWant[1,1])),abs(aWant[2,2]));
 MaxError:=0.0;
 for Row:=0 to 2 do begin
  for Column:=0 to 2 do begin
   MaxError:=Max(MaxError,abs(aGot[Row,Column]-aWant[Row,Column]));
  end;
 end;
 if MaxError<=(Scale*aRelativeTolerance) then begin
  inc(CountPassed);
  WriteLn(' PASS ',aName,' (diag ',aGot[0,0]:9:6,',',aGot[1,1]:9:6,',',aGot[2,2]:9:6,')');
 end else begin
  inc(CountFailed);
  WriteLn(' FAIL ',aName,' (got diag ',aGot[0,0]:9:6,',',aGot[1,1]:9:6,',',aGot[2,2]:9:6,
                         ' want diag ',aWant[0,0]:9:6,',',aWant[1,1]:9:6,',',aWant[2,2]:9:6,' max error ',MaxError:9:6,')');
 end;
end;

function DiagonalTensor(const aX,aY,aZ:TKraftScalar):TKraftMatrix3x3;
begin
 FillChar(result,SizeOf(TKraftMatrix3x3),#0);
 result[0,0]:=aX;
 result[1,1]:=aY;
 result[2,2]:=aZ;
end;

// A half turn about the given unit axis, 2*a*a^T-I. It is symmetric and its own inverse, so it moves the
// inertia the same way whichever way round a rotation is applied to it.
function HalfTurn(const aAxis:TKraftVector3):TKraftMatrix4x4;
var Row,Column:longint;
    Axis:array[0..2] of TKraftScalar;
begin
 Axis[0]:=aAxis.x;
 Axis[1]:=aAxis.y;
 Axis[2]:=aAxis.z;
 result:=Matrix4x4Identity;
 for Row:=0 to 2 do begin
  for Column:=0 to 2 do begin
   result[Row,Column]:=2.0*Axis[Row]*Axis[Column];
   if Row=Column then begin
    result[Row,Column]:=result[Row,Column]-1.0;
   end;
  end;
 end;
end;

// A 0.5 by 1 by 0.5 box standing on its origin
function BuildBoxHull(const aPhysics:TKraft):TKraftConvexHull;
var CornerIndex:longint;
begin
 result:=TKraftConvexHull.Create(aPhysics);
 for CornerIndex:=0 to 7 do begin
  result.AddVertex(Vector3(BoxHalfWidth*(((CornerIndex and 1) shl 1)-1),
                           BoxHeight*((CornerIndex shr 1) and 1),
                           BoxHalfWidth*((((CornerIndex shr 2) and 1) shl 1)-1)));
 end;
 result.Build;
 result.Finish;
end;

// A pin like solid of revolution with its foot at the origin: a narrow base, a wide belly and a narrow
// head, as the hull of three rings
function BuildPinHull(const aPhysics:TKraft):TKraftConvexHull;
const RingSegments=16;
      RingHeights:array[0..2] of TKraftScalar=(0.0,0.35,1.1);
      RingRadii:array[0..2] of TKraftScalar=(0.08,0.18,0.06);
var RingIndex,SegmentIndex:longint;
    Angle:TKraftScalar;
begin
 result:=TKraftConvexHull.Create(aPhysics);
 for RingIndex:=0 to 2 do begin
  for SegmentIndex:=0 to RingSegments-1 do begin
   Angle:=(SegmentIndex*2.0*pi)/RingSegments;
   result.AddVertex(Vector3(cos(Angle)*RingRadii[RingIndex],RingHeights[RingIndex],sin(Angle)*RingRadii[RingIndex]));
  end;
 end;
 result.Build;
 result.Finish;
end;

function CreatePhysics:TKraft;
begin
 result:=TKraft.Create(-1);
 result.SetFrequency(60.0);
 result.VelocityIterations:=16;
 result.PositionIterations:=16;
 result.Gravity.x:=0.0;
 result.Gravity.y:=-9.81;
 result.Gravity.z:=0.0;
end;

function CreateHullBody(const aPhysics:TKraft;const aHull:TKraftConvexHull;const aLocalTransform:TKraftMatrix4x4;const aScale:TKraftScalar):TKraftRigidBody;
var Shape:TKraftShapeConvexHull;
begin
 result:=TKraftRigidBody.Create(aPhysics);
 result.SetRigidBodyType(krbtDYNAMIC);
 Shape:=TKraftShapeConvexHull.Create(aPhysics,result,aHull);
 Shape.Density:=1.0;
 Shape.Friction:=0.5;
 Shape.Restitution:=0.1;
 Shape.LocalTransform:=aLocalTransform;
 if aScale<>1.0 then begin
  Shape.Scale:=aScale;
 end;
 result.Finish;
 result.CollisionGroups:=[0];
end;

procedure TestHullMassCenter;
var Physics:TKraft;
    Hull:TKraftConvexHull;
    Body:TKraftRigidBody;
begin
 WriteLn('box hull standing on its origin');
 Physics:=CreatePhysics;
 try
  Hull:=BuildBoxHull(Physics);
  CheckVector('hull mass center is the centroid',Hull.MassData.Center,Vector3(0.0,BoxHeight*0.5,0.0),1e-5);
  Body:=CreateHullBody(Physics,Hull,Matrix4x4Identity,1.0);
  Check('body mass is the analytic one',abs(Body.Mass-BoxMass)<=1e-5);
  CheckVector('body mass center is the centroid',Body.Sweep.LocalCenter,Vector3(0.0,BoxHeight*0.5,0.0),1e-5);
  CheckTensor('body inertia is the analytic one about the centroid',Body.BodyInertiaTensor,DiagonalTensor(BoxInertiaX,BoxInertiaY,BoxInertiaX),1e-4);
 finally
  Physics.Free;
 end;
end;

procedure TestHullLocalTransform;
var Physics:TKraft;
    Body:TKraftRigidBody;
    LocalTransform:TKraftMatrix4x4;
    Rotation:TKraftMatrix3x3;
begin
 WriteLn('box hull standing on its origin, half turned about a skew axis and moved by the shape local transform');
 Physics:=CreatePhysics;
 try
  LocalTransform:=Matrix4x4TermMul(HalfTurn(Vector3Norm(Vector3(1.0,2.0,0.5))),Matrix4x4Translate(0.2,0.1,-0.3));
  Rotation:=Matrix3x3(LocalTransform);
  Body:=CreateHullBody(Physics,BuildBoxHull(Physics),LocalTransform,1.0);
  CheckVector('body mass center is the moved centroid',Body.Sweep.LocalCenter,Vector3TermMatrixMul(Vector3(0.0,BoxHeight*0.5,0.0),LocalTransform),1e-5);
  CheckTensor('body inertia is the turned analytic one',Body.BodyInertiaTensor,Matrix3x3TermMul(Matrix3x3TermMul(Rotation,DiagonalTensor(BoxInertiaX,BoxInertiaY,BoxInertiaX)),Rotation),1e-4);
 finally
  Physics.Free;
 end;
end;

procedure TestHullScale;
const Scale=2.0;
var Physics:TKraft;
    Body:TKraftRigidBody;
    InertiaScale:TKraftScalar;
begin
 WriteLn('box hull standing on its origin, uniformly scaled');
 Physics:=CreatePhysics;
 try
  Body:=CreateHullBody(Physics,BuildBoxHull(Physics),Matrix4x4Identity,Scale);
  InertiaScale:=Scale*Scale*Scale*Scale*Scale;
  Check('body mass is the scaled analytic one',abs(Body.Mass-(BoxMass*Scale*Scale*Scale))<=1e-4);
  CheckVector('body mass center is the scaled centroid',Body.Sweep.LocalCenter,Vector3(0.0,BoxHeight*0.5*Scale,0.0),1e-5);
  CheckTensor('body inertia is the scaled analytic one',Body.BodyInertiaTensor,DiagonalTensor(BoxInertiaX*InertiaScale,BoxInertiaY*InertiaScale,BoxInertiaX*InertiaScale),1e-4);
 finally
  Physics.Free;
 end;
end;

procedure TestTinyHullKeepsOrigin;
var Physics:TKraft;
    Hull:TKraftConvexHull;
    Body:TKraftRigidBody;
    CornerIndex:longint;
begin
 WriteLn('hull too small to get an inertia, away from its origin');
 Physics:=CreatePhysics;
 try
  Hull:=TKraftConvexHull.Create(Physics);
  for CornerIndex:=0 to 7 do begin
   Hull.AddVertex(Vector3(0.005*(CornerIndex and 1),0.5+(0.005*((CornerIndex shr 1) and 1)),0.005*((CornerIndex shr 2) and 1)));
  end;
  Hull.Build;
  Hull.Finish;
  CheckVector('mass center stays at the origin',Hull.MassData.Center,Vector3Origin,0.0);
  Body:=CreateHullBody(Physics,Hull,Matrix4x4Identity,1.0);
  Check('body inertia does not turn negative',(Body.BodyInertiaTensor[0,0]>=0.0) and (Body.BodyInertiaTensor[1,1]>=0.0) and (Body.BodyInertiaTensor[2,2]>=0.0));
 finally
  Physics.Free;
 end;
end;

procedure TestHullStreamWithoutMassCenter;
var Physics:TKraft;
    Hull,LoadedHull:TKraftConvexHull;
    Stream:TMemoryStream;
    Zero:TKraftScalar;
begin
 WriteLn('hull stream which carries the origin as mass center');
 Physics:=CreatePhysics;
 Stream:=TMemoryStream.Create;
 try
  Hull:=BuildPinHull(Physics);
  Hull.SaveToStream(Stream);
  // The stream ends with the mass center, the mass, the volume and the centroid, so the mass center
  // starts eight scalars before its end. Zeroed there, it is what streams from before carried.
  Zero:=0.0;
  Stream.Seek(Stream.Size-(8*SizeOf(TKraftScalar)),soBeginning);
  Stream.WriteBuffer(Zero,SizeOf(TKraftScalar));
  Stream.WriteBuffer(Zero,SizeOf(TKraftScalar));
  Stream.WriteBuffer(Zero,SizeOf(TKraftScalar));
  Stream.Seek(0,soBeginning);
  LoadedHull:=TKraftConvexHull.Create(Physics);
  LoadedHull.LoadFromStream(Stream);
  Check('centroid is well above the foot',(Hull.Centroid.y>0.3) and (Hull.Centroid.y<0.6));
  CheckVector('loaded hull mass center is the centroid',LoadedHull.MassData.Center,Hull.Centroid,1e-6);
 finally
  Stream.Free;
  Physics.Free;
 end;
end;

procedure TestMeshMassCenter;
var Physics:TKraft;
    Body:TKraftRigidBody;
    Shape:TKraftShapeMesh;
    Mesh:TKraftMesh;
    VertexIndices:array[0..7] of TKraftInt32;
    CornerIndex:longint;
begin
 WriteLn('closed box mesh standing on its origin');
 Physics:=CreatePhysics;
 try
  Mesh:=TKraftMesh.Create(Physics);
  for CornerIndex:=0 to 7 do begin
   VertexIndices[CornerIndex]:=Mesh.AddVertex(Vector3(BoxHalfWidth*(((CornerIndex and 1) shl 1)-1),
                                                      BoxHeight*((CornerIndex shr 1) and 1),
                                                      BoxHalfWidth*((((CornerIndex shr 2) and 1) shl 1)-1)),false);
  end;
  // Outward counter clockwise winding on all six faces, corner index bits are x, y and z
  Mesh.AddTriangle(VertexIndices[4],VertexIndices[5],VertexIndices[7]); // +z
  Mesh.AddTriangle(VertexIndices[4],VertexIndices[7],VertexIndices[6]);
  Mesh.AddTriangle(VertexIndices[1],VertexIndices[0],VertexIndices[2]); // -z
  Mesh.AddTriangle(VertexIndices[1],VertexIndices[2],VertexIndices[3]);
  Mesh.AddTriangle(VertexIndices[5],VertexIndices[1],VertexIndices[3]); // +x
  Mesh.AddTriangle(VertexIndices[5],VertexIndices[3],VertexIndices[7]);
  Mesh.AddTriangle(VertexIndices[0],VertexIndices[4],VertexIndices[6]); // -x
  Mesh.AddTriangle(VertexIndices[0],VertexIndices[6],VertexIndices[2]);
  Mesh.AddTriangle(VertexIndices[2],VertexIndices[6],VertexIndices[7]); // +y
  Mesh.AddTriangle(VertexIndices[2],VertexIndices[7],VertexIndices[3]);
  Mesh.AddTriangle(VertexIndices[0],VertexIndices[1],VertexIndices[5]); // -y
  Mesh.AddTriangle(VertexIndices[0],VertexIndices[5],VertexIndices[4]);
  Mesh.Finish;
  Body:=TKraftRigidBody.Create(Physics);
  Body.SetRigidBodyType(krbtDYNAMIC);
  Shape:=TKraftShapeMesh.Create(Physics,Body,Mesh);
  Shape.Density:=1.0;
  Body.Finish;
  CheckVector('body mass center is the centroid',Body.Sweep.LocalCenter,Vector3(0.0,BoxHeight*0.5,0.0),1e-5);
  CheckTensor('body inertia is the analytic one about the centroid',Body.BodyInertiaTensor,DiagonalTensor(BoxInertiaX,BoxInertiaY,BoxInertiaX),1e-4);
  // A rescale runs the mass computation again, which must come out the same and not pile up
  Shape.Finish;
  Body.Finish;
  CheckVector('body mass center stays the same over a second run',Body.Sweep.LocalCenter,Vector3(0.0,BoxHeight*0.5,0.0),1e-5);
 finally
  Physics.Free;
 end;
end;

function RunPin(const aTiltDegrees:TKraftScalar):TKraftScalar;
var Physics:TKraft;
    FloorBody,PinBody:TKraftRigidBody;
    FloorShape:TKraftShapePlane;
    StepIndex:longint;
    Angle:TKraftScalar;
begin
 Physics:=CreatePhysics;
 try
  FloorBody:=TKraftRigidBody.Create(Physics);
  FloorBody.SetRigidBodyType(krbtSTATIC);
  FloorShape:=TKraftShapePlane.Create(Physics,FloorBody,Plane(Vector3(0.0,1.0,0.0),0.0));
  FloorShape.Friction:=0.5;
  FloorBody.Finish;
  FloorBody.SetWorldTransformation(Matrix4x4Identity);
  FloorBody.CollisionGroups:=[0];
  PinBody:=CreateHullBody(Physics,BuildPinHull(Physics),Matrix4x4Identity,1.0);
  // Tilted about the center of the foot, so lifted by what that sinks the lower edge of the foot
  Angle:=aTiltDegrees*(pi/180.0);
  PinBody.SetWorldTransformation(Matrix4x4TermMul(Matrix4x4RotateZ(Angle),Matrix4x4Translate(0.0,0.002+(0.08*sin(Angle)),0.0)));
  for StepIndex:=1 to 240 do begin
   Physics.Step(1.0/60.0);
  end;
  result:=ArcCos(Min(1.0,Max(-1.0,PinBody.WorldTransform[1,1])))*(180.0/pi);
 finally
  Physics.Free;
 end;
end;

procedure TestPinStandsAndFalls;
var Tilt:TKraftScalar;
begin
 WriteLn('pin on a floor');
 Tilt:=RunPin(0.0);
 Check('upright pin stays upright (tilt '+FloatToStrF(Tilt,ffFixed,7,2)+')',Tilt<3.0);
 Tilt:=RunPin(30.0);
 Check('pin tilted past its tipping angle falls over (tilt '+FloatToStrF(Tilt,ffFixed,7,2)+')',Tilt>60.0);
end;

begin
 FormatSettings.DecimalSeparator:='.';
 CountPassed:=0;
 CountFailed:=0;
 TestHullMassCenter;
 TestHullLocalTransform;
 TestHullScale;
 TestTinyHullKeepsOrigin;
 TestHullStreamWithoutMassCenter;
 TestMeshMassCenter;
 TestPinStandsAndFalls;
 WriteLn;
 WriteLn('=== ',CountPassed,' passed, ',CountFailed,' failed ===');
 if CountFailed>0 then begin
  Halt(1);
 end;
end.
