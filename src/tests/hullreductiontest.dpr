program hullreductiontest;

{$ifdef fpc}
 {$mode delphi}
{$endif}
{$apptype console}

uses {$ifdef unix}cthreads,{$endif} SysUtils,Classes,Math,kraft;

// Point limited convex hulls. The default reduction keeps adding the point furthest outside the hull so far,
// which keeps the volume but drops a flat face early, since its rim sticks out only a little: a bowling pin
// whose base is a flat disc with a slight chamfer ends up balancing on a single point. kchrmSupportSampling
// keeps the large flat faces of the full hull flat and spreads the rest of the budget evenly over all
// directions instead. Checked here on a pin, a box with rounded edges and an evenly tessellated sphere: flat
// faces stay flat and large enough to stand on, the point limit holds, pins stand, and the default reduction
// is untouched.

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

type TPoints=array of TKraftVector3;

const PinBaseRadius=0.079;
      PinCenterOfMassY=0.44;

// A bowling pin about 1.15 m tall as a solid of revolution with 32 segments: a flat base disc, a chamfer ring
// 1.1 mm above it which sticks out further, then belly, neck, head and a pointed top
function PinPoints:TPoints;
const Profile:array[0..9,0..1] of double=((0.0011,0.086),(0.03,0.104),(0.215,0.169),(0.368,0.182),(0.55,0.15),
                                          (0.75,0.06),(0.9,0.085),(1.045,0.103),(1.12,0.06),(1.145,0.014));
      Segments=32;
      SubSteps=6;
var Rings:array of array[0..1] of double;
    Count,Index,SubIndex,SegmentIndex:longint;
    t,Angle:double;
 procedure AddRing(const aY,aRadius:double);
 begin
  SetLength(Rings,length(Rings)+1);
  Rings[high(Rings)][0]:=aY;
  Rings[high(Rings)][1]:=aRadius;
 end;
begin
 Rings:=nil;
 AddRing(0.0,PinBaseRadius);
 for Index:=0 to high(Profile)-1 do begin
  for SubIndex:=0 to SubSteps-1 do begin
   t:=SubIndex/SubSteps;
   t:=t*t*(3.0-(2.0*t));
   AddRing(Profile[Index,0]+((Profile[Index+1,0]-Profile[Index,0])*t),Profile[Index,1]+((Profile[Index+1,1]-Profile[Index,1])*t));
  end;
 end;
 AddRing(Profile[high(Profile),0],Profile[high(Profile),1]);
 SetLength(result,(length(Rings)*Segments)+2);
 Count:=0;
 for Index:=0 to high(Rings) do begin
  for SegmentIndex:=0 to Segments-1 do begin
   Angle:=(SegmentIndex*2.0*pi)/Segments;
   result[Count]:=Vector3(cos(Angle)*Rings[Index][1],Rings[Index][0],sin(Angle)*Rings[Index][1]);
   inc(Count);
  end;
 end;
 result[Count]:=Vector3(0.0,0.0,0.0);
 inc(Count);
 result[Count]:=Vector3(0.0,1.148,0.0);
 inc(Count);
 SetLength(result,Count);
end;

// A 1.0 x 0.5 x 0.8 m box whose edges and corners are rounded with a radius of 2 cm: spheres of 66 points at
// the eight inner corners, with the six axis poles among them, so each side is an exact rectangle
function RoundedBoxPoints:TPoints;
const Radius=0.02;
      HalfExtents:array[0..2] of double=(0.5,0.25,0.4);
var Sphere:TPoints;
    Count,Corner,Index:longint;
    y,r,Angle:double;
    Center:TKraftVector3;
begin
 SetLength(Sphere,66);
 for Index:=0 to 59 do begin
  y:=1.0-(((Index*2.0)+1.0)/60.0);
  r:=sqrt(Max(0.0,1.0-(y*y)));
  Angle:=Index*(pi*(3.0-sqrt(5.0)));
  Sphere[Index]:=Vector3(cos(Angle)*r*Radius,y*Radius,sin(Angle)*r*Radius);
 end;
 Sphere[60]:=Vector3(Radius,0.0,0.0);
 Sphere[61]:=Vector3(-Radius,0.0,0.0);
 Sphere[62]:=Vector3(0.0,Radius,0.0);
 Sphere[63]:=Vector3(0.0,-Radius,0.0);
 Sphere[64]:=Vector3(0.0,0.0,Radius);
 Sphere[65]:=Vector3(0.0,0.0,-Radius);
 SetLength(result,8*length(Sphere));
 Count:=0;
 for Corner:=0 to 7 do begin
  Center:=Vector3(IfThen((Corner and 1)<>0,1.0,-1.0)*(HalfExtents[0]-Radius),
                  IfThen((Corner and 2)<>0,1.0,-1.0)*(HalfExtents[1]-Radius),
                  IfThen((Corner and 4)<>0,1.0,-1.0)*(HalfExtents[2]-Radius));
  for Index:=0 to high(Sphere) do begin
   result[Count]:=Vector3Add(Center,Sphere[Index]);
   inc(Count);
  end;
 end;
end;

// 500 points spread evenly over a sphere of radius 0.5 m
function SpherePoints:TPoints;
var Index:longint;
    y,r,Angle:double;
begin
 SetLength(result,500);
 for Index:=0 to high(result) do begin
  y:=1.0-(((Index*2.0)+1.0)/length(result));
  r:=sqrt(Max(0.0,1.0-(y*y)));
  Angle:=Index*(pi*(3.0-sqrt(5.0)));
  result[Index]:=Vector3(cos(Angle)*r*0.5,y*0.5,sin(Angle)*r*0.5);
 end;
end;

function BuildHull(const aPhysics:TKraft;const aPoints:TPoints;const aLimit:longint;const aMode:TKraftConvexHullReductionMode):TKraftConvexHull;
var Index:longint;
begin
 result:=TKraftConvexHull.Create(aPhysics);
 result.ReductionMode:=aMode;
 for Index:=0 to high(aPoints) do begin
  result.AddVertex(aPoints[Index]);
 end;
 result.Build(aLimit);
 result.Finish;
end;

// The face whose normal is closest to aNormal: its angle to it in degrees, its vertex count, and the smallest
// distance of its edges from the line through the origin along aNormal (the support radius of the face
// around that line, which decides how far a body standing on it can lean before it tips over)
procedure FaceTowards(const aHull:TKraftConvexHull;const aNormal:TKraftVector3;out aAngle:double;out aCountVertices:longint;out aSupportRadius:double);
var Index,BestIndex,VertexIndex,Step:longint;
    BestDot,d,Support,Phi:double;
    Tangent,Bitangent,Direction,p:TKraftVector3;
begin
 BestIndex:=0;
 BestDot:=-2.0;
 for Index:=0 to aHull.CountFaces-1 do begin
  d:=Vector3Dot(aHull.Faces[Index].Plane.Normal,aNormal);
  if BestDot<d then begin
   BestDot:=d;
   BestIndex:=Index;
  end;
 end;
 aAngle:=ArcCos(Min(1.0,BestDot))*180.0/pi;
 aCountVertices:=aHull.Faces[BestIndex].CountVertices;
 if abs(aNormal.x)<0.5 then begin
  Tangent:=Vector3Norm(Vector3Cross(aNormal,Vector3(1.0,0.0,0.0)));
 end else begin
  Tangent:=Vector3Norm(Vector3Cross(aNormal,Vector3(0.0,1.0,0.0)));
 end;
 Bitangent:=Vector3Cross(aNormal,Tangent);
 aSupportRadius:=1e30;
 for Step:=0 to 359 do begin
  Phi:=Step*pi/180.0;
  Direction:=Vector3Add(Vector3ScalarMul(Tangent,cos(Phi)),Vector3ScalarMul(Bitangent,sin(Phi)));
  Support:=-1e30;
  for VertexIndex:=0 to aHull.Faces[BestIndex].CountVertices-1 do begin
   p:=aHull.Vertices[aHull.Faces[BestIndex].Vertices[VertexIndex]].Position;
   Support:=Max(Support,Vector3Dot(p,Direction));
  end;
  aSupportRadius:=Min(aSupportRadius,Support);
 end;
end;

// The smallest support distance of the hull over 2000 directions, compared to aRadius
function MinimumSupport(const aHull:TKraftConvexHull):double;
var Index,VertexIndex:longint;
    y,r,Angle,Support:double;
    Direction:TKraftVector3;
begin
 result:=1e30;
 for Index:=0 to 1999 do begin
  y:=1.0-(((Index*2.0)+1.0)/2000.0);
  r:=sqrt(Max(0.0,1.0-(y*y)));
  Angle:=Index*(pi*(3.0-sqrt(5.0)));
  Direction:=Vector3(cos(Angle)*r,y,sin(Angle)*r);
  Support:=-1e30;
  for VertexIndex:=0 to aHull.CountVertices-1 do begin
   Support:=Max(Support,Vector3Dot(aHull.Vertices[VertexIndex].Position,Direction));
  end;
  result:=Min(result,Support);
 end;
end;

function CreatePhysics:TKraft;
begin
 result:=TKraft.Create(1);
 result.SetFrequency(60.0);
 result.VelocityIterations:=16;
 result.PositionIterations:=16;
 result.Gravity.y:=-9.81;
end;

// A static floor mesh of two big triangles through the origin, turned by aRotation
procedure CreateFloor(const aPhysics:TKraft;const aRotation:TKraftMatrix4x4);
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
 Mesh:=TKraftMesh.Create(aPhysics);
 Mesh.AddTriangle(Corner(-20.0,-20.0),Corner(-20.0,20.0),Corner(20.0,20.0));
 Mesh.AddTriangle(Corner(-20.0,-20.0),Corner(20.0,20.0),Corner(20.0,-20.0));
 Mesh.Finish;
 Shape:=TKraftShapeMesh.Create(aPhysics,Body,Mesh);
 Shape.Friction:=0.5;
 Body.Finish;
 Body.SetWorldTransformation(Matrix4x4Identity);
 Body.CollisionGroups:=[0];
end;

// Pins with the given hull limit and reduction, set down along the normal of a floor tilted by aTiltDegrees
// with a random heading, their centre of mass low in the belly as on a real pin; returns how many of aCount
// still stand after five seconds
function StandingPins(const aLimit:longint;const aMode:TKraftConvexHullReductionMode;const aTiltDegrees:TKraftScalar;const aCount:longint):longint;
var Points:TPoints;
    Physics:TKraft;
    Body:TKraftRigidBody;
    Hull:TKraftConvexHull;
    Shape:TKraftShapeConvexHull;
    Rotation:TKraftMatrix4x4;
    Up:TKraftVector3;
    Index,StepIndex,VertexIndex:longint;
    MinimumY:TKraftScalar;
begin
 result:=0;
 Points:=PinPoints;
 Rotation:=Matrix4x4RotateZ(aTiltDegrees*pi/180.0);
 Up:=Vector3TermMatrixMulBasis(Vector3(0.0,1.0,0.0),Rotation);
 RandSeed:=1234;
 for Index:=0 to aCount-1 do begin
  Physics:=CreatePhysics;
  try
   CreateFloor(Physics,Rotation);
   Hull:=BuildHull(Physics,Points,aLimit,aMode);
   MinimumY:=1e30;
   for VertexIndex:=0 to Hull.CountVertices-1 do begin
    MinimumY:=Min(MinimumY,Hull.Vertices[VertexIndex].Position.y);
   end;
   Body:=TKraftRigidBody.Create(Physics);
   Body.SetRigidBodyType(krbtDYNAMIC);
   Shape:=TKraftShapeConvexHull.Create(Physics,Body,Hull);
   Shape.Friction:=0.5;
   Shape.Restitution:=0.3;
   Shape.ForcedCenterOfMass.Vector:=Vector3(0.0,PinCenterOfMassY,0.0);
   Shape.Flags:=Shape.Flags+[ksfHasForcedCenterOfMass];
   Body.ForcedMass:=1.6;
   Body.Finish;
   Body.SetWorldTransformation(Matrix4x4TermMul(Matrix4x4TermMul(Matrix4x4RotateY(Random*2.0*pi),Matrix4x4Translate(0.0,0.001-MinimumY,0.0)),Rotation));
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

const Limits:array[0..5] of longint=(16,24,32,48,64,96);
      ModeNames:array[TKraftConvexHullReductionMode] of string=('farthest point','support sampling');
      AxisNormals:array[0..5,0..2] of double=((1.0,0.0,0.0),(-1.0,0.0,0.0),(0.0,1.0,0.0),(0.0,-1.0,0.0),(0.0,0.0,1.0),(0.0,0.0,-1.0));
var Physics:TKraft;
    Hull,OtherHull:TKraftConvexHull;
    Points:TPoints;
    Mode:TKraftConvexHullReductionMode;
    Limit,CountVertices,Index,Standing,StandingTilted:longint;
    Angle,SupportRadius:double;
    AllFlat,Same:boolean;
begin
 FormatSettings.DecimalSeparator:='.';
 CountPassed:=0;
 CountFailed:=0;
 Physics:=TKraft.Create(1);
 try

  WriteLn('pin, base radius ',Format('%.0f',[PinBaseRadius*1000.0]),' mm, bottom face of the reduced hull:');
  Points:=PinPoints;
  for Limit in Limits do begin
   for Mode:=low(TKraftConvexHullReductionMode) to high(TKraftConvexHullReductionMode) do begin
    Hull:=BuildHull(Physics,Points,Limit,Mode);
    FaceTowards(Hull,Vector3(0.0,-1.0,0.0),Angle,CountVertices,SupportRadius);
    WriteLn(Format('  limit %2d, %-16s: %2d vertices, bottom face %2d vertices tilted %5.2f deg, support radius %5.1f mm',
                   [Limit,ModeNames[Mode],Hull.CountVertices,CountVertices,Angle,SupportRadius*1000.0]));
    if Mode=kchrmSupportSampling then begin
     Check(Format('pin with limit %d, support sampling: within the limit',[Limit]),Hull.CountVertices<=Limit);
     Check(Format('pin with limit %d, support sampling: the base stays flat',[Limit]),(Angle<0.05) and (CountVertices>=3));
     Check(Format('pin with limit %d, support sampling: the base stays nearly as wide as the disc',[Limit]),SupportRadius>=(PinBaseRadius*0.85));
    end;
   end;
  end;

  WriteLn('rounded box 1.0 x 0.5 x 0.8 m:');
  Points:=RoundedBoxPoints;
  Hull:=BuildHull(Physics,Points,48,kchrmSupportSampling);
  AllFlat:=true;
  for Index:=0 to 5 do begin
   FaceTowards(Hull,Vector3(AxisNormals[Index,0],AxisNormals[Index,1],AxisNormals[Index,2]),Angle,CountVertices,SupportRadius);
   WriteLn(Format('  limit 48, support sampling, side %d: %d vertices, tilted %.2f deg',[Index,CountVertices,Angle]));
   AllFlat:=AllFlat and (Angle<0.05) and (CountVertices>=4);
  end;
  Check('rounded box with limit 48, support sampling: all six sides stay flat rectangles',AllFlat and (Hull.CountVertices<=48));
  Hull:=BuildHull(Physics,Points,24,kchrmSupportSampling);
  AllFlat:=true;
  for Index:=2 to 3 do begin
   FaceTowards(Hull,Vector3(AxisNormals[Index,0],AxisNormals[Index,1],AxisNormals[Index,2]),Angle,CountVertices,SupportRadius);
   WriteLn(Format('  limit 24, support sampling, large side %d: %d vertices, tilted %.2f deg',[Index,CountVertices,Angle]));
   AllFlat:=AllFlat and (Angle<0.05) and (CountVertices>=4);
  end;
  Check('rounded box with limit 24, support sampling: the two largest sides stay flat rectangles',AllFlat and (Hull.CountVertices<=24));

  WriteLn('evenly tessellated sphere, radius 0.5 m:');
  Points:=SpherePoints;
  for Mode:=low(TKraftConvexHullReductionMode) to high(TKraftConvexHullReductionMode) do begin
   Hull:=BuildHull(Physics,Points,32,Mode);
   WriteLn(Format('  limit 32, %-16s: %d vertices, smallest support %.1f mm',[ModeNames[Mode],Hull.CountVertices,MinimumSupport(Hull)*1000.0]));
   if Mode=kchrmSupportSampling then begin
    Check('sphere with limit 32, support sampling: uses most of the budget and covers every side',
          (Hull.CountVertices<=32) and (Hull.CountVertices>=28) and (MinimumSupport(Hull)>=(0.5*0.85)));
   end;
  end;

  // Within the limit anyway, the reduction must not drop anything
  Hull:=BuildHull(Physics,Points,1000,kchrmSupportSampling);
  OtherHull:=BuildHull(Physics,Points,-1,kchrmFarthestPoint);
  Check('support sampling with a limit above the full hull keeps the full hull',Hull.CountVertices=OtherHull.CountVertices);

  // The default must stay the old reduction
  Points:=PinPoints;
  Hull:=TKraftConvexHull.Create(Physics);
  Check('a new convex hull reduces by the farthest point by default',Hull.ReductionMode=kchrmFarthestPoint);
  for Index:=0 to high(Points) do begin
   Hull.AddVertex(Points[Index]);
  end;
  Hull.Build(24);
  Hull.Finish;
  OtherHull:=BuildHull(Physics,Points,24,kchrmFarthestPoint);
  Same:=Hull.CountVertices=OtherHull.CountVertices;
  if Same then begin
   for Index:=0 to Hull.CountVertices-1 do begin
    Same:=Same and Vector3Compare(Hull.Vertices[Index].Position,OtherHull.Vertices[Index].Position);
   end;
  end;
  Check('the default reduction builds the same hull as the farthest point reduction',Same);

 finally
  Physics.Free;
 end;

 WriteLn('pins standing after five seconds, out of 20:');
 for Mode:=low(TKraftConvexHullReductionMode) to high(TKraftConvexHullReductionMode) do begin
  Standing:=StandingPins(24,Mode,0.0,20);
  StandingTilted:=StandingPins(24,Mode,5.0,20);
  WriteLn(Format('  limit 24, %-16s: %2d on a flat floor, %2d on a floor tilted by 5 deg',[ModeNames[Mode],Standing,StandingTilted]));
  if Mode=kchrmSupportSampling then begin
   Check('pins with limit 24, support sampling: all stand on a flat floor',Standing=20);
   Check('pins with limit 24, support sampling: nearly all stand on a floor tilted by 5 deg',StandingTilted>=18);
  end;
 end;

 WriteLn;
 WriteLn('=== ',CountPassed,' passed, ',CountFailed,' failed ===');
 if CountFailed>0 then begin
  Halt(1);
 end;
end.
