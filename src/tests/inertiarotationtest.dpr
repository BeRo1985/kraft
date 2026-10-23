program inertiarotationtest;

{$ifdef fpc}
 {$mode delphi}
{$endif}
{$apptype console}

uses {$ifdef unix}cthreads,{$endif} SysUtils,Classes,Math,kraft;

// Turning an inertia tensor. All transforms here take a row vector from the left, so a matrix has the
// turned axes as its rows and a tensor goes along as T^T*I*T. Checked by bodies whose shape is turned by
// a shape local transform or by the body orientation against the same geometry built turned already,
// by the response to an impulse, and for signed distance field shapes, whose inertia has to follow
// density, placement, turn and scale like that of every other shape. Also checks that the axis angle
// rotation matrices are proper rotations.

type TBoxSDF=class(TKraftSignedDistanceField)
      private
       fHalfExtents:TKraftVector3;
       fOffset:TKraftVector3;
      public
       constructor Create(const aPhysics:TKraft;const aHalfExtents,aOffset:TKraftVector3); reintroduce;
       function GetLocalSignedDistance(const Position:TKraftVector3):TKraftScalar; override;
     end;

const HalfExtentX=0.1;
      HalfExtentY=0.3;
      HalfExtentZ=0.5;

var CountPassed,CountFailed:longint;

constructor TBoxSDF.Create(const aPhysics:TKraft;const aHalfExtents,aOffset:TKraftVector3);
var AABB:TKraftAABB;
begin
 fHalfExtents:=aHalfExtents;
 fOffset:=aOffset;
 AABB.Min:=Vector3Sub(aOffset,aHalfExtents);
 AABB.Max:=Vector3Add(aOffset,aHalfExtents);
 inherited Create(aPhysics,false,@AABB);
 Finish;
end;

function TBoxSDF.GetLocalSignedDistance(const Position:TKraftVector3):TKraftScalar;
var q:TKraftVector3;
begin
 q.x:=abs(Position.x-fOffset.x)-fHalfExtents.x;
 q.y:=abs(Position.y-fOffset.y)-fHalfExtents.y;
 q.z:=abs(Position.z-fOffset.z)-fHalfExtents.z;
 result:=sqrt(sqr(Max(q.x,0.0))+sqr(Max(q.y,0.0))+sqr(Max(q.z,0.0)))+Min(Max(q.x,Max(q.y,q.z)),0.0);
end;

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
  WriteLn(' PASS ',aName,' (',aGot.x:9:5,',',aGot.y:9:5,',',aGot.z:9:5,')');
 end else begin
  inc(CountFailed);
  WriteLn(' FAIL ',aName,' (got ',aGot.x:9:5,',',aGot.y:9:5,',',aGot.z:9:5,' want ',aWant.x:9:5,',',aWant.y:9:5,',',aWant.z:9:5,')');
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
  WriteLn(' PASS ',aName,' (max error ',MaxError:10:7,')');
 end else begin
  inc(CountFailed);
  WriteLn(' FAIL ',aName,' (max error ',MaxError:10:7,', allowed ',(Scale*aRelativeTolerance):10:7,')');
 end;
end;

// Largest deviation of m*m^T from the identity and of the determinant from one
function RotationError(const m:TKraftMatrix3x3):TKraftScalar;
var Row,Column,Index:longint;
    Sum:TKraftScalar;
begin
 result:=0.0;
 for Row:=0 to 2 do begin
  for Column:=0 to 2 do begin
   Sum:=0.0;
   for Index:=0 to 2 do begin
    Sum:=Sum+(m[Row,Index]*m[Column,Index]);
   end;
   if Row=Column then begin
    Sum:=Sum-1.0;
   end;
   result:=Max(result,abs(Sum));
  end;
 end;
 result:=Max(result,abs(Matrix3x3Determinant(m)-1.0));
end;

function Turned(const aInertia,aRotation:TKraftMatrix3x3):TKraftMatrix3x3;
begin
 result:=Matrix3x3TermMul(Matrix3x3TermTranspose(aRotation),Matrix3x3TermMul(aInertia,aRotation));
end;

function CreatePhysics:TKraft;
begin
 result:=TKraft.Create(-1);
 result.SetFrequency(60.0);
 result.Gravity.x:=0.0;
 result.Gravity.y:=0.0;
 result.Gravity.z:=0.0;
end;

function BuildBoxHull(const aPhysics:TKraft;const aTransform:TKraftMatrix4x4):TKraftConvexHull;
var CornerIndex:longint;
begin
 result:=TKraftConvexHull.Create(aPhysics);
 for CornerIndex:=0 to 7 do begin
  result.AddVertex(Vector3TermMatrixMul(Vector3(HalfExtentX*(((CornerIndex and 1) shl 1)-1),
                                                HalfExtentY*((((CornerIndex shr 1) and 1) shl 1)-1),
                                                HalfExtentZ*((((CornerIndex shr 2) and 1) shl 1)-1)),aTransform));
 end;
 result.Build;
 result.Finish;
end;

function CreateBody(const aPhysics:TKraft;const aShapeKind:longint;const aGeometry:TObject;const aLocalTransform:TKraftMatrix4x4;const aDensity,aForcedMass,aScale:TKraftScalar):TKraftRigidBody;
var Shape:TKraftShape;
begin
 result:=TKraftRigidBody.Create(aPhysics);
 result.SetRigidBodyType(krbtDYNAMIC);
 if aShapeKind=0 then begin
  Shape:=TKraftShapeConvexHull.Create(aPhysics,result,TKraftConvexHull(aGeometry));
 end else begin
  Shape:=TKraftShapeSignedDistanceField.Create(aPhysics,result,TKraftSignedDistanceField(aGeometry));
 end;
 Shape.Density:=aDensity;
 if aForcedMass>0.0 then begin
  Shape.ForcedMass:=aForcedMass;
 end;
 Shape.LocalTransform:=aLocalTransform;
 if aScale<>1.0 then begin
  Shape.Scale:=aScale;
 end;
 result.Finish;
end;

// A turn about two axes, so that the turned tensor is full and the direction of the turn shows
function TestTurn:TKraftMatrix4x4;
begin
 result:=Matrix4x4TermMul(Matrix4x4RotateX(0.5),Matrix4x4RotateY(0.3));
end;

procedure TestAxisAngleRotations;
var Axis:TKraftVector3;
    Index:longint;
    Angle,Error3,Error4,AxisError:TKraftScalar;
begin
 WriteLn('axis angle rotation matrices');
 Error3:=0.0;
 Error4:=0.0;
 RandSeed:=1;
 for Index:=1 to 200 do begin
  Axis:=Vector3Norm(Vector3(Random-0.5,Random-0.5,Random-0.5));
  Angle:=(Random-0.5)*4.0*pi;
  Error3:=Max(Error3,RotationError(Matrix3x3Rotate(Angle,Axis)));
  Error4:=Max(Error4,RotationError(Matrix3x3(Matrix4x4Rotate(Angle,Axis))));
 end;
 Check('Matrix3x3Rotate gives proper rotations (error '+FloatToStrF(Error3,ffExponent,3,2)+')',Error3<1e-5);
 Check('Matrix4x4Rotate gives proper rotations (error '+FloatToStrF(Error4,ffExponent,3,2)+')',Error4<1e-5);
 AxisError:=0.0;
 for Index:=0 to 15 do begin
  Angle:=Index*0.4;
  AxisError:=Max(AxisError,abs(Matrix4x4Rotate(Angle,Vector3(1.0,0.0,0.0))[1,2]-Matrix4x4RotateX(Angle)[1,2]));
  AxisError:=Max(AxisError,abs(Matrix4x4Rotate(Angle,Vector3(0.0,1.0,0.0))[2,0]-Matrix4x4RotateY(Angle)[2,0]));
  AxisError:=Max(AxisError,abs(Matrix4x4Rotate(Angle,Vector3(0.0,0.0,1.0))[0,1]-Matrix4x4RotateZ(Angle)[0,1]));
 end;
 Check('Matrix4x4Rotate about the coordinate axes matches RotateX/Y/Z',AxisError<1e-6);
end;

procedure TestHullTurns;
var Physics:TKraft;
    Reference,ByShape,ByBody:TKraftRigidBody;
    Turn:TKraftMatrix4x4;
    Velocity,ReferenceVelocity:TKraftVector3;
begin
 WriteLn('box hull turned by the shape local transform and by the body orientation');
 Physics:=CreatePhysics;
 try
  Turn:=TestTurn;
  Reference:=CreateBody(Physics,0,BuildBoxHull(Physics,Turn),Matrix4x4Identity,1.0,0.0,1.0);
  ByShape:=CreateBody(Physics,0,BuildBoxHull(Physics,Matrix4x4Identity),Turn,1.0,0.0,1.0);
  ByBody:=CreateBody(Physics,0,BuildBoxHull(Physics,Matrix4x4Identity),Matrix4x4Identity,1.0,0.0,1.0);
  ByBody.SetWorldTransformation(Turn);
  ByBody.UpdateWorldInertiaTensor;
  Reference.SetWorldTransformation(Matrix4x4Identity);
  Reference.UpdateWorldInertiaTensor;
  CheckTensor('shape local transform turns the inertia like the geometry',ByShape.BodyInertiaTensor,Reference.BodyInertiaTensor,1e-4);
  CheckTensor('body orientation turns the world inertia like the geometry',ByBody.WorldInertiaTensor,Reference.WorldInertiaTensor,1e-4);
  // The same impulse at the same world point has to spin both the same way
  ByBody.ApplyImpulseAtPosition(Vector3(0.05,0.2,0.3),Vector3(1.0,0.5,-0.25));
  Reference.ApplyImpulseAtPosition(Vector3(0.05,0.2,0.3),Vector3(1.0,0.5,-0.25));
  Velocity:=ByBody.AngularVelocity;
  ReferenceVelocity:=Reference.AngularVelocity;
  CheckVector('impulse on the turned body spins it like the turned geometry',Velocity,ReferenceVelocity,Vector3Length(ReferenceVelocity)*1e-4);
 finally
  Physics.Free;
 end;
end;

procedure TestSignedDistanceField;
var Physics:TKraft;
    Centered,Raised,Dense,TurnedBody,Forced,ForcedScaled:TKraftRigidBody;
    Turn:TKraftMatrix4x4;
    Expected:TKraftMatrix3x3;
    Row,Column:longint;
begin
 WriteLn('signed distance field box shape');
 Physics:=CreatePhysics;
 try
  Centered:=CreateBody(Physics,1,TBoxSDF.Create(Physics,Vector3(HalfExtentX,HalfExtentY,HalfExtentZ),Vector3(0.0,0.0,0.0)),Matrix4x4Identity,1.0,0.0,1.0);
  Raised:=CreateBody(Physics,1,TBoxSDF.Create(Physics,Vector3(HalfExtentX,HalfExtentY,HalfExtentZ),Vector3(0.0,HalfExtentY,0.0)),Matrix4x4Identity,1.0,0.0,1.0);
  Dense:=CreateBody(Physics,1,TBoxSDF.Create(Physics,Vector3(HalfExtentX,HalfExtentY,HalfExtentZ),Vector3(0.0,0.0,0.0)),Matrix4x4Identity,2.0,0.0,1.0);
  Turn:=TestTurn;
  TurnedBody:=CreateBody(Physics,1,TBoxSDF.Create(Physics,Vector3(HalfExtentX,HalfExtentY,HalfExtentZ),Vector3(0.0,0.0,0.0)),Turn,1.0,0.0,1.0);
  Forced:=CreateBody(Physics,1,TBoxSDF.Create(Physics,Vector3(HalfExtentX,HalfExtentY,HalfExtentZ),Vector3(0.0,0.0,0.0)),Matrix4x4Identity,1.0,3.0,1.0);
  ForcedScaled:=CreateBody(Physics,1,TBoxSDF.Create(Physics,Vector3(HalfExtentX,HalfExtentY,HalfExtentZ),Vector3(0.0,0.0,0.0)),Matrix4x4Identity,1.0,3.0,2.0);
  // Without these, everything below could hold for a field without any mass at all
  Check('field mass is the box volume at unit density',abs(Centered.Mass-(8.0*HalfExtentX*HalfExtentY*HalfExtentZ))<=1e-4);
  Check('field inertia is there',(Centered.BodyInertiaTensor[0,0]>1e-4) and (Centered.BodyInertiaTensor[1,1]>1e-4) and (Centered.BodyInertiaTensor[2,2]>1e-4));
  CheckVector('raised field has its mass center raised',Raised.Sweep.LocalCenter,Vector3(0.0,HalfExtentY,0.0),1e-4);
  CheckTensor('raised field keeps the inertia about its mass center',Raised.BodyInertiaTensor,Centered.BodyInertiaTensor,1e-4);
  Check('double density doubles the mass',abs(Dense.Mass-(2.0*Centered.Mass))<=(Centered.Mass*1e-5));
  Expected:=Centered.BodyInertiaTensor;
  for Row:=0 to 2 do begin
   for Column:=0 to 2 do begin
    Expected[Row,Column]:=Expected[Row,Column]*2.0;
   end;
  end;
  CheckTensor('double density doubles the inertia',Dense.BodyInertiaTensor,Expected,1e-4);
  CheckTensor('shape local transform turns the inertia',TurnedBody.BodyInertiaTensor,Turned(Centered.BodyInertiaTensor,Matrix3x3(Turn)),1e-4);
  Check('forced mass is the mass',abs(Forced.Mass-3.0)<=1e-5);
  Expected:=Forced.BodyInertiaTensor;
  for Row:=0 to 2 do begin
   for Column:=0 to 2 do begin
    Expected[Row,Column]:=Expected[Row,Column]*4.0;
   end;
  end;
  CheckTensor('forced mass, doubled in size, quadruples the inertia',ForcedScaled.BodyInertiaTensor,Expected,1e-4);
 finally
  Physics.Free;
 end;
end;

begin
 FormatSettings.DecimalSeparator:='.';
 CountPassed:=0;
 CountFailed:=0;
 TestAxisAngleRotations;
 TestHullTurns;
 TestSignedDistanceField;
 WriteLn;
 WriteLn('=== ',CountPassed,' passed, ',CountFailed,' failed ===');
 if CountFailed>0 then begin
  Halt(1);
 end;
end.
