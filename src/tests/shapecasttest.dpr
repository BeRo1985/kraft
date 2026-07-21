program shapecasttest;
{$ifdef fpc}
 {$mode delphi}
{$endif}
{$apptype console}
uses {$ifdef unix}cthreads,{$endif} SysUtils,Math,kraft;

type { TSlabSDF }
     // Wide flat rounded slab, outer half extents (8.0,0.5,8.0), top face at y=0.5
     TSlabSDF=class(TKraftSignedDistanceField)
      public
       function GetLocalSignedDistance(const Position:TKraftVector3):TKraftScalar; override;
     end;

     { TRoundedBoxSDF }
     // Rounded box, outer half extent 0.5, edge rounding 0.25
     TRoundedBoxSDF=class(TKraftSignedDistanceField)
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

function TRoundedBoxSDF.GetLocalSignedDistance(const Position:TKraftVector3):TKraftScalar;
begin
 result:=SdRoundedBox(Position,Vector3(0.5,0.5,0.5),0.25);
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

function MakeStaticBody(const aPhysics:TKraft;const aTransform:TKraftMatrix4x4):TKraftRigidBody;
begin
 result:=TKraftRigidBody.Create(aPhysics);
 result.SetRigidBodyType(krbtSTATIC);
end;

var Physics:TKraft;
    FloorBody,CastBody,BlockerBody:TKraftRigidBody;
    SlabSDF:TSlabSDF;
    BoxSDF:TRoundedBoxSDF;
    CastShape:TKraftShape;
    FloorShape,BlockerShape:TKraftShape;
    HitShape:TKraftShape;
    HitTime:TKraftScalar;
    HitPoint:TKraftPosition;
    HitNormal:TKraftVector3;
    Mesh:TKraftMesh;
    MeshShape:TKraftShapeMesh;
    OK:boolean;
    v0,v1,v2,v3:TKraftInt32;

procedure NewWorld;
begin
 FreeAndNil(Physics);
 Physics:=TKraft.Create(-1);
end;

begin
 FormatSettings.DecimalSeparator:='.';
 SetExceptionMask([exInvalidOp,exDenormalized,exZeroDivide,exOverflow,exUnderflow,exPrecision]);
 CountPassed:=0;
 CountFailed:=0;
 Physics:=nil;

 WriteLn('=== box cast down onto SDF slab ===');
 NewWorld;
 FloorBody:=MakeStaticBody(Physics,Matrix4x4Identity);
 SlabSDF:=TSlabSDF.Create(Physics,true);
 SlabSDF.Finish;
 FloorShape:=TKraftShapeSignedDistanceField.Create(Physics,FloorBody,SlabSDF);
 FloorBody.Finish;
 FloorBody.SetWorldTransformation(Matrix4x4Translate(0.0,0.0,0.0));
 FloorBody.CollisionGroups:=[0];
 CastBody:=MakeStaticBody(Physics,Matrix4x4Identity);
 CastShape:=TKraftShapeBox.Create(Physics,CastBody,Vector3(0.25,0.25,0.25));
 CastBody.Finish;
 CastBody.SetWorldTransformation(Matrix4x4Translate(0.0,4.0,0.0));
 CastBody.CollisionGroups:=[1];
 OK:=Physics.ShapeCast(CastShape,Vector3(0.0,-1.0,0.0),16.0,HitShape,HitTime,HitPoint,HitNormal,[0]);
 Check('hit',OK and (HitShape=FloorShape));
 if OK then begin
  CheckScalar('time',HitTime,3.25,1e-3);
  CheckScalar('point y',HitPoint.y,0.5,1e-2);
  CheckVector('normal',HitNormal,Vector3(0.0,1.0,0.0),1e-2);
 end;

 WriteLn('=== 45 degree rotated box cast down onto SDF slab (orientation aware) ===');
 CastBody.SetWorldTransformation(Matrix4x4TermMul(Matrix4x4RotateZ(pi*0.25),Matrix4x4Translate(0.0,4.0,0.0)));
 OK:=Physics.ShapeCast(CastShape,Vector3(0.0,-1.0,0.0),16.0,HitShape,HitTime,HitPoint,HitNormal,[0]);
 Check('hit',OK and (HitShape=FloorShape));
 if OK then begin
  CheckScalar('time',HitTime,4.0-(0.5+(0.25*sqrt(2.0))),2e-2);
 end;
 CastBody.SetWorldTransformation(Matrix4x4Translate(0.0,4.0,0.0));

 WriteLn('=== initial overlap: box inside SDF slab reports hit at time zero ===');
 CastBody.SetWorldTransformation(Matrix4x4Translate(0.0,0.5,0.0));
 OK:=Physics.ShapeCast(CastShape,Vector3(0.0,-1.0,0.0),16.0,HitShape,HitTime,HitPoint,HitNormal,[0]);
 Check('hit',OK and (HitShape=FloorShape));
 if OK then begin
  CheckScalar('time zero',HitTime,0.0,1e-3);
  Check('normal points up',HitNormal.y>0.5);
 end;
 CastBody.SetWorldTransformation(Matrix4x4Translate(0.0,4.0,0.0));

 WriteLn('=== miss and maxtime cutoff ===');
 CastBody.SetWorldTransformation(Matrix4x4Translate(20.0,4.0,0.0));
 Check('miss beside slab',not Physics.ShapeCast(CastShape,Vector3(0.0,-1.0,0.0),16.0,HitShape,HitTime,HitPoint,HitNormal,[0]));
 CastBody.SetWorldTransformation(Matrix4x4Translate(0.0,4.0,0.0));
 Check('maxtime cutoff',not Physics.ShapeCast(CastShape,Vector3(0.0,-1.0,0.0),2.0,HitShape,HitTime,HitPoint,HitNormal,[0]));

 WriteLn('=== capsule cast down onto static thin box floor ===');
 NewWorld;
 FloorBody:=MakeStaticBody(Physics,Matrix4x4Identity);
 FloorShape:=TKraftShapeBox.Create(Physics,FloorBody,Vector3(8.0,0.1,8.0));
 FloorBody.Finish;
 FloorBody.SetWorldTransformation(Matrix4x4Translate(0.0,0.0,0.0));
 FloorBody.CollisionGroups:=[0];
 CastBody:=MakeStaticBody(Physics,Matrix4x4Identity);
 CastShape:=TKraftShapeCapsule.Create(Physics,CastBody,0.25,1.0);
 CastBody.Finish;
 CastBody.SetWorldTransformation(Matrix4x4Translate(0.0,4.0,0.0));
 CastBody.CollisionGroups:=[1];
 OK:=Physics.ShapeCast(CastShape,Vector3(0.0,-1.0,0.0),16.0,HitShape,HitTime,HitPoint,HitNormal,[0]);
 Check('hit',OK and (HitShape=FloorShape));
 if OK then begin
  CheckScalar('time',HitTime,3.15,1e-2);
  CheckVector('normal',HitNormal,Vector3(0.0,1.0,0.0),1e-2);
 end;

 WriteLn('=== SDF rounded box as CAST shape onto static thin box floor ===');
 CastBody:=MakeStaticBody(Physics,Matrix4x4Identity);
 BoxSDF:=TRoundedBoxSDF.Create(Physics);
 BoxSDF.Finish;
 CastShape:=TKraftShapeSignedDistanceField.Create(Physics,CastBody,BoxSDF);
 CastBody.Finish;
 CastBody.SetWorldTransformation(Matrix4x4Translate(2.0,4.0,0.0));
 CastBody.CollisionGroups:=[1];
 OK:=Physics.ShapeCast(CastShape,Vector3(0.0,-1.0,0.0),16.0,HitShape,HitTime,HitPoint,HitNormal,[0]);
 Check('hit',OK and (HitShape=FloorShape));
 if OK then begin
  CheckScalar('time',HitTime,3.4,1e-2);
  CheckScalar('point y',HitPoint.y,0.1,2e-2);
  CheckVector('normal',HitNormal,Vector3(0.0,1.0,0.0),1e-2);
 end;

 WriteLn('=== sidewards cast: box against unit box, plus nearest ordering ===');
 NewWorld;
 FloorBody:=MakeStaticBody(Physics,Matrix4x4Identity);
 FloorShape:=TKraftShapeBox.Create(Physics,FloorBody,Vector3(0.5,0.5,0.5));
 FloorBody.Finish;
 FloorBody.SetWorldTransformation(Matrix4x4Translate(0.0,0.0,0.0));
 FloorBody.CollisionGroups:=[0];
 BlockerBody:=MakeStaticBody(Physics,Matrix4x4Identity);
 BlockerShape:=TKraftShapeBox.Create(Physics,BlockerBody,Vector3(0.5,0.5,0.5));
 BlockerBody.Finish;
 BlockerBody.SetWorldTransformation(Matrix4x4Translate(2.0,0.0,0.0));
 BlockerBody.CollisionGroups:=[0];
 CastBody:=MakeStaticBody(Physics,Matrix4x4Identity);
 CastShape:=TKraftShapeBox.Create(Physics,CastBody,Vector3(0.25,0.25,0.25));
 CastBody.Finish;
 CastBody.SetWorldTransformation(Matrix4x4Translate(6.0,0.0,0.0));
 CastBody.CollisionGroups:=[1];
 OK:=Physics.ShapeCast(CastShape,Vector3(-1.0,0.0,0.0),16.0,HitShape,HitTime,HitPoint,HitNormal,[0]);
 Check('nearest blocker wins',OK and (HitShape=BlockerShape));
 if OK then begin
  CheckScalar('time',HitTime,6.0-(2.0+0.5+0.25),1e-3);
  CheckVector('normal',HitNormal,Vector3(1.0,0.0,0.0),1e-2);
 end;

 WriteLn('=== box cast down onto mesh ground quad ===');
 NewWorld;
 Mesh:=TKraftMesh.Create(Physics);
 v0:=Mesh.AddVertex(Vector3(-8.0,0.0,-8.0));
 v1:=Mesh.AddVertex(Vector3(-8.0,0.0,8.0));
 v2:=Mesh.AddVertex(Vector3(8.0,0.0,8.0));
 v3:=Mesh.AddVertex(Vector3(8.0,0.0,-8.0));
 Mesh.AddTriangle(v0,v1,v2);
 Mesh.AddTriangle(v0,v2,v3);
 Mesh.Finish;
 FloorBody:=MakeStaticBody(Physics,Matrix4x4Identity);
 MeshShape:=TKraftShapeMesh.Create(Physics,FloorBody,Mesh);
 MeshShape.Finish;
 FloorBody.Finish;
 FloorBody.SetWorldTransformation(Matrix4x4Translate(0.0,0.0,0.0));
 FloorBody.CollisionGroups:=[0];
 CastBody:=MakeStaticBody(Physics,Matrix4x4Identity);
 CastShape:=TKraftShapeBox.Create(Physics,CastBody,Vector3(0.25,0.25,0.25));
 CastBody.Finish;
 CastBody.SetWorldTransformation(Matrix4x4Translate(1.0,4.0,1.0));
 CastBody.CollisionGroups:=[1];
 OK:=Physics.ShapeCast(CastShape,Vector3(0.0,-1.0,0.0),16.0,HitShape,HitTime,HitPoint,HitNormal,[0]);
 Check('hit mesh',OK and (HitShape=MeshShape));
 if OK then begin
  CheckScalar('time',HitTime,3.75,1e-2);
  CheckVector('normal',HitNormal,Vector3(0.0,1.0,0.0),1e-2);
 end;

 WriteLn('=== SDF rounded box as CAST shape onto SDF slab (both sides SDF) ===');
 NewWorld;
 FloorBody:=MakeStaticBody(Physics,Matrix4x4Identity);
 SlabSDF:=TSlabSDF.Create(Physics,true);
 SlabSDF.Finish;
 FloorShape:=TKraftShapeSignedDistanceField.Create(Physics,FloorBody,SlabSDF);
 FloorBody.Finish;
 FloorBody.SetWorldTransformation(Matrix4x4Translate(0.0,0.0,0.0));
 FloorBody.CollisionGroups:=[0];
 CastBody:=MakeStaticBody(Physics,Matrix4x4Identity);
 BoxSDF:=TRoundedBoxSDF.Create(Physics);
 BoxSDF.Finish;
 CastShape:=TKraftShapeSignedDistanceField.Create(Physics,CastBody,BoxSDF);
 CastBody.Finish;
 CastBody.SetWorldTransformation(Matrix4x4Translate(0.0,4.0,0.0));
 CastBody.CollisionGroups:=[1];
 OK:=Physics.ShapeCast(CastShape,Vector3(0.0,-1.0,0.0),16.0,HitShape,HitTime,HitPoint,HitNormal,[0]);
 Check('hit',OK and (HitShape=FloorShape));
 if OK then begin
  CheckScalar('time',HitTime,3.0,2e-2);
  CheckScalar('point y',HitPoint.y,0.5,2e-2);
  CheckVector('normal',HitNormal,Vector3(0.0,1.0,0.0),2e-2);
 end;

 FreeAndNil(Physics);

 WriteLn('=== ',CountPassed,' passed, ',CountFailed,' failed ===');
 if CountFailed>0 then begin
  Halt(1);
 end;
end.
