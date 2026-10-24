program queryscaletest;

{$ifdef fpc}
 {$mode delphi}
{$endif}
{$apptype console}

uses {$ifdef unix}cthreads,{$endif} SysUtils,Classes,Math,kraft;

// Geometric queries have to give the same answer at any size, relative to that size. Every scene here is
// built once in units of a scale and run from half a millimetre up to a hundred metres: a shape cast of a
// box down onto a thin floor box and onto a plane shape (GJK conservative advancement), the distance of a
// box hovering above either (GJK, where the plane shape is far larger than anything it meets at any of
// these scales), the penetration of a box into a floor box and of two boxes with close centers into
// each other (MPR), and a small box flying with speculative contacts at a thin wall, faster per step than
// its own size and the wall thickness together (contact recycling and the speculative contacts).
// Fixed thresholds, which are of the size of millimetres, let all of them fail at small scales before.
// Every scene has its own single threaded instance, as worker threads would only make starting and
// stopping each of them slow.

const Scales:array[0..8] of TKraftScalar=(0.0005,0.001,0.002,0.005,0.01,0.1,1.0,10.0,100.0);
      Tolerance=1e-5; // relative to the scale for lengths, absolute for normals

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

function ScaleName(const aScale:TKraftScalar):string;
begin
 result:=Format('scale %.6g',[aScale]);
end;

function NormalError(const aNormal,aAxis:TKraftVector3):TKraftScalar;
begin
 result:=1.0-abs(Vector3Dot(Vector3SafeNorm(aNormal),aAxis));
end;

// The tilt of the query boxes, so that they meet the floor with a corner instead of a face
function Tilt:TKraftMatrix4x4;
begin
 result:=Matrix4x4TermMul(Matrix4x4RotateZ(0.3),Matrix4x4RotateX(0.2));
end;

// The lowest point of a cube with the given half extent under the given rotation
function LowestCorner(const aHalfExtent:TKraftScalar;const aRotation:TKraftMatrix4x4):TKraftScalar;
var Index:longint;
    Corner:TKraftVector3;
begin
 result:=MAX_SCALAR;
 for Index:=0 to 7 do begin
  Corner:=Vector3TermMatrixMul(Vector3(aHalfExtent*(((Index and 1) shl 1)-1),
                                       aHalfExtent*((((Index shr 1) and 1) shl 1)-1),
                                       aHalfExtent*((((Index shr 2) and 1) shl 1)-1)),aRotation);
  result:=Min(result,Corner.y);
 end;
end;

function CreateStaticBox(const aPhysics:TKraft;const aHalfExtents:TKraftVector3;const aTransform:TKraftMatrix4x4;const aGroup:longint;out aShape:TKraftShapeBox):TKraftRigidBody;
begin
 result:=TKraftRigidBody.Create(aPhysics);
 result.SetRigidBodyType(krbtSTATIC);
 aShape:=TKraftShapeBox.Create(aPhysics,result,aHalfExtents);
 result.Finish;
 result.SetWorldTransformation(aTransform);
 result.CollisionGroups:=[aGroup];
end;

function Placed(const aRotation:TKraftMatrix4x4;const aX,aY,aZ:TKraftScalar):TKraftMatrix4x4;
begin
 result:=aRotation;
 result[3,0]:=aX;
 result[3,1]:=aY;
 result[3,2]:=aZ;
end;

// A floor with its top at zero: a thin box, or a plane shape, which is a far larger shape than anything
// that meets it, whatever the scale
function CreateFloor(const aPhysics:TKraft;const aScale:TKraftScalar;const aPlane:boolean;out aName:string):TKraftShape;
var Body:TKraftRigidBody;
    FloorBox:TKraftShapeBox;
begin
 if aPlane then begin
  Body:=TKraftRigidBody.Create(aPhysics);
  Body.SetRigidBodyType(krbtSTATIC);
  result:=TKraftShapePlane.Create(aPhysics,Body,Plane(Vector3(0.0,1.0,0.0),0.0));
  Body.Finish;
  Body.SetWorldTransformation(Matrix4x4Identity);
  Body.CollisionGroups:=[0];
  aName:='a plane';
 end else begin
  CreateStaticBox(aPhysics,Vector3(20.0*aScale,0.1*aScale,20.0*aScale),Matrix4x4Translate(0.0,-0.1*aScale,0.0),0,FloorBox);
  result:=FloorBox;
  aName:='a thin floor';
 end;
end;

procedure TestShapeCast(const aScale:TKraftScalar;const aTilted,aPlane:boolean);
var Physics:TKraft;
    FloorShape:TKraftShape;
    CastShape:TKraftShapeBox;
    HitShape:TKraftShape;
    HitTime:TKraftScalar;
    HitPoint:TKraftPosition;
    HitNormal:TKraftVector3;
    Rotation:TKraftMatrix4x4;
    Name,FloorName:string;
    Hit:boolean;
begin
 Physics:=TKraft.Create(1);
 try
  FloorShape:=CreateFloor(Physics,aScale,aPlane,FloorName);
  if aTilted then begin
   Rotation:=Tilt;
   Name:='a tilted';
  end else begin
   Rotation:=Matrix4x4Identity;
   Name:='an upright';
  end;
  CreateStaticBox(Physics,Vector3(0.25*aScale,0.25*aScale,0.25*aScale),Placed(Rotation,0.0,4.0*aScale,0.0),1,CastShape);
  Hit:=Physics.ShapeCast(CastShape,Vector3(0.0,-1.0,0.0),100.0*aScale,HitShape,HitTime,HitPoint,HitNormal,[0]);
  Check('shape cast of '+Name+' box onto '+FloorName+', '+ScaleName(aScale),
        Hit and
        (HitShape=FloorShape) and
        (abs(HitTime-((4.0+LowestCorner(0.25,Rotation))*aScale))<=(Tolerance*aScale)) and
        (NormalError(HitNormal,Vector3(0.0,1.0,0.0))<=Tolerance));
 finally
  Physics.Free;
 end;
end;

procedure TestDistance(const aScale:TKraftScalar;const aPlane:boolean);
var Physics:TKraft;
    FloorShape:TKraftShape;
    QueryShape:TKraftShapeBox;
    FloorName:string;
begin
 Physics:=TKraft.Create(1);
 try
  FloorShape:=CreateFloor(Physics,aScale,aPlane,FloorName);
  CreateStaticBox(Physics,Vector3(0.25*aScale,0.25*aScale,0.25*aScale),Placed(Tilt,0.0,(0.02-LowestCorner(0.25,Tilt))*aScale,0.0),1,QueryShape);
  Check('distance of a box hovering above '+FloorName+', '+ScaleName(aScale),
        abs(Physics.GetDistance(QueryShape,FloorShape)-(0.02*aScale))<=(Tolerance*aScale));
 finally
  Physics.Free;
 end;
end;

function DeepestContact(const aPhysics:TKraft;const aShape:TKraftShape;out aContact:TKraftShapeCollisionContact):boolean;
var Contacts:TKraftShapeCollisionContacts;
    Count,Index:longint;
begin
 Contacts:=nil;
 result:=aPhysics.CollideShape(aShape,Contacts,Count,[0]) and (Count>0);
 if result then begin
  aContact:=Contacts[0];
  for Index:=1 to Count-1 do begin
   if Contacts[Index].PenetrationDepth>aContact.PenetrationDepth then begin
    aContact:=Contacts[Index];
   end;
  end;
 end;
end;

procedure TestPenetration(const aScale:TKraftScalar);
var Physics:TKraft;
    FloorShape,QueryShape:TKraftShapeBox;
    Contact:TKraftShapeCollisionContact;
begin

 // A tilted box sinking with its lowest corner into a thick floor box
 Physics:=TKraft.Create(1);
 try
  CreateStaticBox(Physics,Vector3(20.0*aScale,1.0*aScale,20.0*aScale),Matrix4x4Translate(0.0,-1.0*aScale,0.0),0,FloorShape);
  CreateStaticBox(Physics,Vector3(0.25*aScale,0.25*aScale,0.25*aScale),Placed(Tilt,0.0,(-0.05-LowestCorner(0.25,Tilt))*aScale,0.0),1,QueryShape);
  Check('penetration of a tilted box into a floor, '+ScaleName(aScale),
        DeepestContact(Physics,QueryShape,Contact) and
        (abs(Contact.PenetrationDepth-(0.05*aScale))<=(Tolerance*aScale)) and
        (NormalError(Contact.Normal,Vector3(0.0,1.0,0.0))<=Tolerance));
 finally
  Physics.Free;
 end;

 // Two boxes with close centers, overlapping by 0.1 sideways and by 0.4 upwards
 Physics:=TKraft.Create(1);
 try
  CreateStaticBox(Physics,Vector3(0.25*aScale,0.25*aScale,0.25*aScale),Matrix4x4Identity,0,FloorShape);
  CreateStaticBox(Physics,Vector3(0.25*aScale,0.25*aScale,0.25*aScale),Matrix4x4Translate(0.4*aScale,0.1*aScale,0.0),1,QueryShape);
  Check('penetration of two boxes with close centers, '+ScaleName(aScale),
        DeepestContact(Physics,QueryShape,Contact) and
        (abs(Contact.PenetrationDepth-(0.1*aScale))<=(Tolerance*aScale)) and
        (NormalError(Contact.Normal,Vector3(1.0,0.0,0.0))<=Tolerance));
 finally
  Physics.Free;
 end;

end;

procedure TestSpeculativeWall(const aScale:TKraftScalar);
var Physics:TKraft;
    WallShape,BoxShape:TKraftShapeBox;
    Box:TKraftRigidBody;
    StepIndex:longint;
begin
 Physics:=TKraft.Create(1);
 try
  Physics.SetFrequency(120.0);
  Physics.Gravity.y:=0.0;
  Physics.ContinuousMode:=kcmSpeculativeContacts;
  CreateStaticBox(Physics,Vector3(0.05*aScale,2.0*aScale,2.0*aScale),Matrix4x4Identity,0,WallShape);
  Box:=TKraftRigidBody.Create(Physics);
  Box.SetRigidBodyType(krbtDYNAMIC);
  BoxShape:=TKraftShapeBox.Create(Physics,Box,Vector3(0.1*aScale,0.1*aScale,0.1*aScale));
  BoxShape.Density:=1.0;
  Box.Finish;
  Box.SetWorldTransformation(Matrix4x4Translate(-2.0*aScale,0.0,0.0));
  Box.CollisionGroups:=[0];
  // 0.8 of the scale per step: four times the box size and eight times the wall thickness
  Box.LinearVelocity:=Vector3(0.8*aScale*120.0,0.0,0.0);
  for StepIndex:=1 to 120 do begin
   Physics.Step(1.0/120.0);
  end;
  Check('fast small box stops at a thin wall with speculative contacts, '+ScaleName(aScale),
        (Box.WorldTransform[3,0]+(0.1*aScale))<=(-0.05*aScale)+(0.001*aScale));
 finally
  Physics.Free;
 end;
end;

var Index:longint;
begin
 FormatSettings.DecimalSeparator:='.';
 CountPassed:=0;
 CountFailed:=0;
 for Index:=0 to high(Scales) do begin
  TestShapeCast(Scales[Index],false,false);
  TestShapeCast(Scales[Index],true,false);
  TestShapeCast(Scales[Index],false,true);
  TestShapeCast(Scales[Index],true,true);
  TestDistance(Scales[Index],false);
  TestDistance(Scales[Index],true);
  TestPenetration(Scales[Index]);
  TestSpeculativeWall(Scales[Index]);
 end;
 WriteLn;
 WriteLn('=== ',CountPassed,' passed, ',CountFailed,' failed ===');
 if CountFailed>0 then begin
  Halt(1);
 end;
end.
