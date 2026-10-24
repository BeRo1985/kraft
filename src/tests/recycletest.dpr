program recycletest;

{$ifdef fpc}
 {$mode delphi}
{$endif}
{$apptype console}

uses {$ifdef unix}cthreads,{$endif} SysUtils,Classes,Math,kraft;

// Contact recycling keeps the manifold of a pair whose relative pose has barely changed since its last real
// narrow phase run. A touching manifold holds only the points which touched at that run, so it must not be kept
// past the point where one of the points the collide routine left out could have come into touch: a cube
// tipping over an edge onto its face, or a slender pin rocking on a base of several faces, would otherwise sink
// in or tip over unseen. Both are compared against the same scene with recycling switched off. A resting pile,
// where everything touches, has to keep recycling all the same.

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

function CreatePhysics(const aRecycle:boolean):TKraft;
begin
 result:=TKraft.Create(1);
 result.SetFrequency(60.0);
 result.VelocityIterations:=16;
 result.PositionIterations:=16;
 result.Gravity.y:=-9.81;
 if not aRecycle then begin
  result.ContactRecycleDistance:=0.0;
 end;
end;

// A static floor through the origin with the given normal, either an analytic plane or a mesh of two big triangles
procedure CreateFloor(const aPhysics:TKraft;const aMesh:boolean;const aRotation:TKraftMatrix4x4);
var Body:TKraftRigidBody;
    Mesh:TKraftMesh;
    Shape:TKraftShape;
 function Corner(const aX,aZ:TKraftScalar):TKraftInt32;
 begin
  result:=Mesh.AddVertex(Vector3TermMatrixMul(Vector3(aX,0.0,aZ),aRotation));
 end;
begin
 Body:=TKraftRigidBody.Create(aPhysics);
 Body.SetRigidBodyType(krbtSTATIC);
 if aMesh then begin
  Mesh:=TKraftMesh.Create(aPhysics);
  Mesh.AddTriangle(Corner(-20.0,-20.0),Corner(-20.0,20.0),Corner(20.0,20.0));
  Mesh.AddTriangle(Corner(-20.0,-20.0),Corner(20.0,20.0),Corner(20.0,-20.0));
  Mesh.Finish;
  Shape:=TKraftShapeMesh.Create(aPhysics,Body,Mesh);
 end else begin
  Shape:=TKraftShapePlane.Create(aPhysics,Body,Plane(Vector3TermMatrixMulBasis(Vector3(0.0,1.0,0.0),aRotation),0.0));
 end;
 Shape.Friction:=0.5;
 Body.Finish;
 Body.SetWorldTransformation(Matrix4x4Identity);
 Body.CollisionGroups:=[0];
end;

// A cube of the given size resting on one edge, tilted so that it falls over onto its face; returns how deep its
// lowest corner sank into the floor at worst, in millimetres
function TipSink(const aSize,aTiltDegrees:TKraftScalar;const aMesh,aRecycle:boolean):TKraftScalar;
var Physics:TKraft;
    Body:TKraftRigidBody;
    Shape:TKraftShapeBox;
    StepIndex,Corner:longint;
    Point:TKraftVector3;
begin
 Physics:=CreatePhysics(aRecycle);
 try
  CreateFloor(Physics,aMesh,Matrix4x4Identity);
  Body:=TKraftRigidBody.Create(Physics);
  Body.SetRigidBodyType(krbtDYNAMIC);
  Shape:=TKraftShapeBox.Create(Physics,Body,Vector3(aSize*0.5,aSize*0.5,aSize*0.5));
  Shape.Restitution:=0.0;
  Body.Finish;
  Body.SetWorldTransformation(Matrix4x4TermMul(Matrix4x4Translate(aSize*0.5,aSize*0.5,0.0),Matrix4x4RotateZ(aTiltDegrees*pi/180.0)));
  Body.CollisionGroups:=[0];
  result:=0.0;
  for StepIndex:=1 to 180 do begin
   Physics.Step(1.0/60.0);
   for Corner:=0 to 7 do begin
    Point:=Vector3TermMatrixMul(Vector3(IfThen((Corner and 1)<>0,0.5,-0.5)*aSize,IfThen((Corner and 2)<>0,0.5,-0.5)*aSize,IfThen((Corner and 4)<>0,0.5,-0.5)*aSize),Body.WorldTransform);
    result:=Max(result,-Point.y);
   end;
  end;
  result:=result*1000.0;
 finally
  Physics.Free;
 end;
end;

// Mean sink of a cube over a range of starting tilts, since a single drop depends on where the steps happen to
// fall relative to the moment of touch
function MeanTipSink(const aSize:TKraftScalar;const aMesh,aRecycle:boolean):TKraftScalar;
var Index:longint;
begin
 result:=0.0;
 for Index:=0 to 29 do begin
  result:=result+TipSink(aSize,12.0+(Index*1.1),aMesh,aRecycle);
 end;
 result:=result/30.0;
end;

// A slender pin, about a metre tall, whose base is a very flat cone of eight faces with its tip a millimetre lower
// than the rim, set along the normal of a floor tilted by aTiltDegrees with a random heading; returns how many of
// aCount still stand after five seconds
function StandingPins(const aTiltDegrees:TKraftScalar;const aMesh,aRecycle:boolean;const aCount:longint):longint;
var Physics:TKraft;
    Body:TKraftRigidBody;
    Hull:TKraftConvexHull;
    Shape:TKraftShapeConvexHull;
    Rotation:TKraftMatrix4x4;
    Up:TKraftVector3;
    Index,PointIndex,StepIndex:longint;
    Angle:TKraftScalar;
begin
 result:=0;
 Rotation:=Matrix4x4RotateZ(aTiltDegrees*pi/180.0);
 Up:=Vector3TermMatrixMulBasis(Vector3(0.0,1.0,0.0),Rotation);
 RandSeed:=1234;
 for Index:=0 to aCount-1 do begin
  Physics:=CreatePhysics(aRecycle);
  try
   CreateFloor(Physics,aMesh,Rotation);
   Hull:=TKraftConvexHull.Create(Physics);
   Hull.AddVertex(Vector3(0.0,0.0,0.0));
   for PointIndex:=0 to 7 do begin
    Angle:=PointIndex*pi*0.25;
    Hull.AddVertex(Vector3(cos(Angle)*0.085,0.0011,sin(Angle)*0.085));
    Hull.AddVertex(Vector3(cos(Angle)*0.12,0.3,sin(Angle)*0.12));
    Hull.AddVertex(Vector3(cos(Angle)*0.06,1.0,sin(Angle)*0.06));
   end;
   Hull.Build;
   Hull.Finish;
   Body:=TKraftRigidBody.Create(Physics);
   Body.SetRigidBodyType(krbtDYNAMIC);
   Shape:=TKraftShapeConvexHull.Create(Physics,Body,Hull);
   Shape.Friction:=0.5;
   Shape.Restitution:=0.3;
   Body.Finish;
   Body.SetWorldTransformation(Matrix4x4TermMul(Matrix4x4RotateY(Random*2.0*pi),Rotation));
   Body.CollisionGroups:=[0];
   for StepIndex:=1 to 300 do begin
    Physics.Step(1.0/60.0);
   end;
   if Vector3Dot(Vector3(Body.WorldTransform[1,0],Body.WorldTransform[1,1],Body.WorldTransform[1,2]),Up)>cos(10.0*pi/180.0) then begin
    inc(result);
   end;
  finally
   Physics.Free;
  end;
 end;
end;

// A settled pile of boxes with sleeping off; returns the share of pair steps which skipped the narrow phase
function PileRecycleRate:TKraftScalar;
var Physics:TKraft;
    Body:TKraftRigidBody;
    Shape:TKraftShapeBox;
    X,Y,Z,StepIndex:longint;
    Recycled,Pairs:int64;
begin
 Physics:=CreatePhysics(true);
 try
  CreateFloor(Physics,false,Matrix4x4Identity);
  for Y:=0 to 2 do begin
   for Z:=0 to 3 do begin
    for X:=0 to 3 do begin
     Body:=TKraftRigidBody.Create(Physics);
     Body.SetRigidBodyType(krbtDYNAMIC);
     Shape:=TKraftShapeBox.Create(Physics,Body,Vector3(0.25,0.25,0.25));
     Shape.Friction:=0.5;
     Body.Flags:=Body.Flags-[krbfAllowSleep];
     Body.Finish;
     Body.SetWorldTransformation(Matrix4x4Translate(X*0.52,0.25+(Y*0.51),Z*0.52));
     Body.CollisionGroups:=[0];
    end;
   end;
  end;
  for StepIndex:=1 to 120 do begin
   Physics.Step(1.0/60.0);
  end;
  Recycled:=0;
  Pairs:=0;
  for StepIndex:=1 to 120 do begin
   Physics.Step(1.0/60.0);
   inc(Recycled,Physics.ContactManager.CountRecycledContactPairs);
   inc(Pairs,Physics.ContactManager.CountContactPairs);
  end;
  result:=Recycled/Max(1,Pairs);
 finally
  Physics.Free;
 end;
end;

var Size,On,Off:TKraftScalar;
    Mesh:boolean;
    StandingOn,StandingOff:longint;
    Rate:TKraftScalar;
const Sizes:array[0..1] of TKraftScalar=(0.1,0.5);
      FloorNames:array[boolean] of string=('plane','mesh');
begin
 FormatSettings.DecimalSeparator:='.';
 CountPassed:=0;
 CountFailed:=0;
 for Mesh:=false to true do begin
  for Size in Sizes do begin
   On:=MeanTipSink(Size,Mesh,true);
   Off:=MeanTipSink(Size,Mesh,false);
   WriteLn(Format('  cube %.2f m tipping onto its face on a %s: mean sink %.1f mm, %.1f mm without recycling',[Size,FloorNames[Mesh],On,Off]));
   // Within one step a falling face sinks in by up to its own motion, and where in that range it ends up
   // shifts with every small difference of the path, so only a clear excess counts
   Check(Format('a %.2f m cube tipping onto its face on a %s sinks in about as deep as without recycling',[Size,FloorNames[Mesh]]),On<(Off*1.6));
  end;
 end;
 for Mesh:=false to true do begin
  StandingOn:=StandingPins(5.0,Mesh,true,20);
  StandingOff:=StandingPins(5.0,Mesh,false,20);
  WriteLn(Format('  pins on a %s tilted by 5 deg: %d/20 standing, %d/20 without recycling',[FloorNames[Mesh],StandingOn,StandingOff]));
  Check(Format('pins on a %s tilted by 5 deg stand about as well as without recycling',[FloorNames[Mesh]]),StandingOn>=(StandingOff-2));
 end;
 Rate:=PileRecycleRate;
 WriteLn(Format('  resting pile: %.1f %% of the pair steps recycled',[Rate*100.0]));
 Check('a resting pile keeps recycling its contacts',Rate>0.95);
 WriteLn;
 WriteLn('=== ',CountPassed,' passed, ',CountFailed,' failed ===');
 if CountFailed>0 then begin
  Halt(1);
 end;
end.
