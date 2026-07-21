program collidepushtest;
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

function MakeSlabWorld(out aFloorShape:TKraftShape):TKraft;
var FloorBody:TKraftRigidBody;
    SDF:TSlabSDF;
begin
 result:=TKraft.Create(-1);
 FloorBody:=TKraftRigidBody.Create(result);
 FloorBody.SetRigidBodyType(krbtSTATIC);
 SDF:=TSlabSDF.Create(result,true);
 SDF.Finish;
 aFloorShape:=TKraftShapeSignedDistanceField.Create(result,FloorBody,SDF);
 FloorBody.Finish;
 FloorBody.SetWorldTransformation(Matrix4x4Translate(0.0,0.0,0.0));
 FloorBody.CollisionGroups:=[0];
end;

function MakeQueryBody(const aPhysics:TKraft):TKraftRigidBody;
begin
 result:=TKraftRigidBody.Create(aPhysics);
 result.SetRigidBodyType(krbtSTATIC);
end;

procedure TestCollideShapeSphereOnSlab;
var Physics:TKraft;
    FloorShape,QueryShape:TKraftShape;
    QueryBody:TKraftRigidBody;
    Contacts:TKraftShapeCollisionContacts;
    CountContacts:TKraftInt32;
begin
 WriteLn('=== CollideShape: sphere r=0.25 at y=0.6 over SDF slab (penetration 0.15) ===');
 Physics:=MakeSlabWorld(FloorShape);
 try
  QueryBody:=MakeQueryBody(Physics);
  QueryShape:=TKraftShapeSphere.Create(Physics,QueryBody,0.25);
  QueryBody.Finish;
  QueryBody.SetWorldTransformation(Matrix4x4Translate(0.0,0.6,0.0));
  QueryBody.CollisionGroups:=[1];
  Contacts:=nil;
  if Physics.CollideShape(QueryShape,Contacts,CountContacts,[0]) and (CountContacts>0) then begin
   Check('contact found',true);
   Check('contact with slab',Contacts[0].ShapeB=FloorShape);
   CheckVector('contact normal',Contacts[0].Normal,Vector3(0.0,1.0,0.0),1e-2);
   CheckScalar('contact depth',Contacts[0].PenetrationDepth,0.15,1e-2);
  end else begin
   Check('contact found',false);
  end;
 finally
  Physics.Free;
 end;
end;

procedure TestCollideShapeCapsuleOnSlab;
var Physics:TKraft;
    FloorShape,QueryShape:TKraftShape;
    QueryBody:TKraftRigidBody;
    Contacts:TKraftShapeCollisionContacts;
    CountContacts:TKraftInt32;
begin
 WriteLn('=== CollideShape: upright capsule r=0.25 h=1.0 at y=0.9 over SDF slab (penetration 0.35) ===');
 Physics:=MakeSlabWorld(FloorShape);
 try
  QueryBody:=MakeQueryBody(Physics);
  QueryShape:=TKraftShapeCapsule.Create(Physics,QueryBody,0.25,1.0);
  QueryBody.Finish;
  QueryBody.SetWorldTransformation(Matrix4x4Translate(0.0,0.9,0.0));
  QueryBody.CollisionGroups:=[1];
  Contacts:=nil;
  if Physics.CollideShape(QueryShape,Contacts,CountContacts,[0]) and (CountContacts>0) then begin
   Check('contact found',true);
   Check('contact with slab',Contacts[0].ShapeB=FloorShape);
   CheckVector('contact normal',Contacts[0].Normal,Vector3(0.0,1.0,0.0),1e-2);
   CheckScalar('contact depth',Contacts[0].PenetrationDepth,0.35,2e-2);
  end else begin
   Check('contact found',false);
  end;
 finally
  Physics.Free;
 end;
end;

procedure TestCollideShapeSDFOnPlane;
var Physics:TKraft;
    FloorBody,QueryBody:TKraftRigidBody;
    FloorShape,QueryShape:TKraftShape;
    SDF:TRoundedBoxSDF;
    Contacts:TKraftShapeCollisionContacts;
    CountContacts:TKraftInt32;
begin
 WriteLn('=== CollideShape: SDF rounded box body at y=0.3 over plane (penetration 0.2) ===');
 Physics:=TKraft.Create(-1);
 try
  FloorBody:=TKraftRigidBody.Create(Physics);
  FloorBody.SetRigidBodyType(krbtSTATIC);
  FloorShape:=TKraftShapePlane.Create(Physics,FloorBody,Plane(Vector3Norm(Vector3(0.0,1.0,0.0)),0.0));
  FloorBody.Finish;
  FloorBody.SetWorldTransformation(Matrix4x4Translate(0.0,0.0,0.0));
  FloorBody.CollisionGroups:=[0];
  QueryBody:=MakeQueryBody(Physics);
  SDF:=TRoundedBoxSDF.Create(Physics);
  SDF.Finish;
  QueryShape:=TKraftShapeSignedDistanceField.Create(Physics,QueryBody,SDF);
  QueryBody.Finish;
  QueryBody.SetWorldTransformation(Matrix4x4Translate(0.0,0.3,0.0));
  QueryBody.CollisionGroups:=[1];
  Contacts:=nil;
  if Physics.CollideShape(QueryShape,Contacts,CountContacts,[0]) and (CountContacts>0) then begin
   Check('contact found',true);
   Check('contact with plane',Contacts[0].ShapeB=FloorShape);
   CheckVector('contact normal',Contacts[0].Normal,Vector3(0.0,1.0,0.0),1e-2);
   CheckScalar('contact depth',Contacts[0].PenetrationDepth,0.2,2e-2);
  end else begin
   Check('contact found',false);
  end;
 finally
  Physics.Free;
 end;
end;

procedure TestCharacterKitOnSlab;
var Physics:TKraft;
    FloorShape,QueryShape:TKraftShape;
    QueryBody:TKraftRigidBody;
    Contacts:TKraftShapeCollisionContacts;
    CountContacts:TKraftInt32;
    Position:TKraftPosition;
    Velocity:TKraftVector3;
begin
 WriteLn('=== Character kit: capsule at y=0.8 over SDF slab, velocity (2,-3,0), solve position+velocity ===');
 Physics:=MakeSlabWorld(FloorShape);
 try
  QueryBody:=MakeQueryBody(Physics);
  QueryShape:=TKraftShapeCapsule.Create(Physics,QueryBody,0.25,1.0);
  QueryBody.Finish;
  QueryBody.SetWorldTransformation(Matrix4x4Translate(0.0,0.8,0.0));
  QueryBody.CollisionGroups:=[1];
  Contacts:=nil;
  if Physics.CollideShape(QueryShape,Contacts,CountContacts,[0]) and (CountContacts>0) then begin
   Check('contacts found',true);
   Position:=Vector3(0.0,0.8,0.0);
   Velocity:=Vector3(2.0,-3.0,0.0);
   Physics.SolveShapeCollisionContacts(Contacts,CountContacts,Position,Velocity);
   CheckScalar('solved position y (capsule bottom on slab top)',Position.y,1.25,2e-2);
   CheckScalar('solved velocity y (clipped)',Velocity.y,0.0,1e-2);
   CheckScalar('solved velocity x (preserved)',Velocity.x,2.0,1e-1);
  end else begin
   Check('contacts found',false);
  end;
 finally
  Physics.Free;
 end;
end;

procedure TestPushShapeSphereOnSlab;
var Physics:TKraft;
    FloorShape,QueryShape:TKraftShape;
    QueryBody:TKraftRigidBody;
    Seperation:TKraftVector3;
begin
 WriteLn('=== PushShape: sphere r=0.25 at y=0.6 over SDF slab (delegates to PushSphere) ===');
 Physics:=MakeSlabWorld(FloorShape);
 try
  QueryBody:=MakeQueryBody(Physics);
  QueryShape:=TKraftShapeSphere.Create(Physics,QueryBody,0.25);
  QueryBody.Finish;
  QueryBody.SetWorldTransformation(Matrix4x4Translate(0.0,0.6,0.0));
  QueryBody.CollisionGroups:=[1];
  if Physics.PushShape(QueryShape,Seperation,[0]) then begin
   Check('push happened',true);
   CheckVector('push separation',Seperation,Vector3(0.0,0.15,0.0),2e-2);
  end else begin
   Check('push happened',false);
  end;
 finally
  Physics.Free;
 end;
end;

procedure TestPushShapeCapsuleOnSlab;
var Physics:TKraft;
    FloorShape,QueryShape:TKraftShape;
    QueryBody:TKraftRigidBody;
    Seperation:TKraftVector3;
begin
 WriteLn('=== PushShape: upright capsule r=0.25 h=1.0 at y=1.05 over SDF slab (penetration 0.2) ===');
 Physics:=MakeSlabWorld(FloorShape);
 try
  QueryBody:=MakeQueryBody(Physics);
  QueryShape:=TKraftShapeCapsule.Create(Physics,QueryBody,0.25,1.0);
  QueryBody.Finish;
  QueryBody.SetWorldTransformation(Matrix4x4Translate(0.0,1.05,0.0));
  QueryBody.CollisionGroups:=[1];
  if Physics.PushShape(QueryShape,Seperation,[0]) then begin
   Check('push happened',true);
   CheckVector('push separation',Seperation,Vector3(0.0,0.2,0.0),2e-2);
  end else begin
   Check('push happened',false);
  end;
 finally
  Physics.Free;
 end;
end;

procedure TestPushShapeCapsuleOnPlane;
var Physics:TKraft;
    FloorBody,QueryBody:TKraftRigidBody;
    QueryShape:TKraftShape;
    Seperation:TKraftVector3;
begin
 WriteLn('=== PushShape: upright capsule at y=0.5 over plane (penetration 0.25) ===');
 Physics:=TKraft.Create(-1);
 try
  FloorBody:=TKraftRigidBody.Create(Physics);
  FloorBody.SetRigidBodyType(krbtSTATIC);
  TKraftShapePlane.Create(Physics,FloorBody,Plane(Vector3Norm(Vector3(0.0,1.0,0.0)),0.0));
  FloorBody.Finish;
  FloorBody.SetWorldTransformation(Matrix4x4Translate(0.0,0.0,0.0));
  FloorBody.CollisionGroups:=[0];
  QueryBody:=MakeQueryBody(Physics);
  QueryShape:=TKraftShapeCapsule.Create(Physics,QueryBody,0.25,1.0);
  QueryBody.Finish;
  QueryBody.SetWorldTransformation(Matrix4x4Translate(0.0,0.5,0.0));
  QueryBody.CollisionGroups:=[1];
  if Physics.PushShape(QueryShape,Seperation,[0]) then begin
   Check('push happened',true);
   CheckVector('push separation',Seperation,Vector3(0.0,0.25,0.0),2e-2);
  end else begin
   Check('push happened',false);
  end;
 finally
  Physics.Free;
 end;
end;

procedure TestPushShapeCapsuleVersusBox;
var Physics:TKraft;
    WallBody,QueryBody:TKraftRigidBody;
    QueryShape:TKraftShape;
    Seperation:TKraftVector3;
begin
 WriteLn('=== PushShape: upright capsule at x=0.6 overlapping unit box side (penetration 0.15) ===');
 Physics:=TKraft.Create(-1);
 try
  WallBody:=TKraftRigidBody.Create(Physics);
  WallBody.SetRigidBodyType(krbtSTATIC);
  TKraftShapeBox.Create(Physics,WallBody,Vector3(0.5,0.5,0.5));
  WallBody.Finish;
  WallBody.SetWorldTransformation(Matrix4x4Translate(0.0,0.0,0.0));
  WallBody.CollisionGroups:=[0];
  QueryBody:=MakeQueryBody(Physics);
  QueryShape:=TKraftShapeCapsule.Create(Physics,QueryBody,0.25,1.0);
  QueryBody.Finish;
  QueryBody.SetWorldTransformation(Matrix4x4Translate(0.6,0.0,0.0));
  QueryBody.CollisionGroups:=[1];
  if Physics.PushShape(QueryShape,Seperation,[0]) then begin
   Check('push happened',true);
   CheckVector('push separation',Seperation,Vector3(0.15,0.0,0.0),2e-2);
  end else begin
   Check('push happened',false);
  end;
 finally
  Physics.Free;
 end;
end;

procedure TestPushShapeBoxVersusBox;
var Physics:TKraft;
    WallBody,QueryBody:TKraftRigidBody;
    QueryShape:TKraftShape;
    Seperation:TKraftVector3;
begin
 WriteLn('=== PushShape: unit box at x=0.8 overlapping static unit box (penetration 0.2) ===');
 Physics:=TKraft.Create(-1);
 try
  WallBody:=TKraftRigidBody.Create(Physics);
  WallBody.SetRigidBodyType(krbtSTATIC);
  TKraftShapeBox.Create(Physics,WallBody,Vector3(0.5,0.5,0.5));
  WallBody.Finish;
  WallBody.SetWorldTransformation(Matrix4x4Translate(0.0,0.0,0.0));
  WallBody.CollisionGroups:=[0];
  QueryBody:=MakeQueryBody(Physics);
  QueryShape:=TKraftShapeBox.Create(Physics,QueryBody,Vector3(0.5,0.5,0.5));
  QueryBody.Finish;
  QueryBody.SetWorldTransformation(Matrix4x4Translate(0.8,0.0,0.0));
  QueryBody.CollisionGroups:=[1];
  if Physics.PushShape(QueryShape,Seperation,[0]) then begin
   Check('push happened',true);
   CheckVector('push separation',Seperation,Vector3(0.2,0.0,0.0),2e-2);
  end else begin
   Check('push happened',false);
  end;
 finally
  Physics.Free;
 end;
end;

procedure TestPushShapeCapsuleOnMesh;
var Physics:TKraft;
    FloorBody,QueryBody:TKraftRigidBody;
    QueryShape:TKraftShape;
    MeshShape:TKraftShapeMesh;
    Mesh:TKraftMesh;
    Seperation:TKraftVector3;
    v0,v1,v2,v3:TKraftInt32;
begin
 WriteLn('=== PushShape: upright capsule at y=0.55 over mesh ground quad (penetration 0.2) ===');
 Physics:=TKraft.Create(-1);
 try
  Mesh:=TKraftMesh.Create(Physics);
  v0:=Mesh.AddVertex(Vector3(-8.0,0.0,-8.0));
  v1:=Mesh.AddVertex(Vector3(-8.0,0.0,8.0));
  v2:=Mesh.AddVertex(Vector3(8.0,0.0,8.0));
  v3:=Mesh.AddVertex(Vector3(8.0,0.0,-8.0));
  Mesh.AddTriangle(v0,v1,v2);
  Mesh.AddTriangle(v0,v2,v3);
  Mesh.Finish;
  FloorBody:=TKraftRigidBody.Create(Physics);
  FloorBody.SetRigidBodyType(krbtSTATIC);
  MeshShape:=TKraftShapeMesh.Create(Physics,FloorBody,Mesh);
  MeshShape.Finish;
  FloorBody.Finish;
  FloorBody.SetWorldTransformation(Matrix4x4Translate(0.0,0.0,0.0));
  FloorBody.CollisionGroups:=[0];
  QueryBody:=MakeQueryBody(Physics);
  QueryShape:=TKraftShapeCapsule.Create(Physics,QueryBody,0.25,1.0);
  QueryBody.Finish;
  QueryBody.SetWorldTransformation(Matrix4x4Translate(0.0,0.55,0.0));
  QueryBody.CollisionGroups:=[1];
  if Physics.PushShape(QueryShape,Seperation,[0]) then begin
   Check('push happened',true);
   CheckVector('push separation',Seperation,Vector3(0.0,0.2,0.0),2e-2);
  end else begin
   Check('push happened',false);
  end;
 finally
  Physics.Free;
 end;
end;

procedure TestPushShapeSingleDeepest;
var Physics:TKraft;
    WallBody,QueryBody:TKraftRigidBody;
    QueryShape:TKraftShape;
    Seperation:TKraftVector3;
begin
 WriteLn('=== PushShape: single deepest mode, capsule at x=0.6 overlapping unit box side ===');
 Physics:=TKraft.Create(-1);
 try
  WallBody:=TKraftRigidBody.Create(Physics);
  WallBody.SetRigidBodyType(krbtSTATIC);
  TKraftShapeBox.Create(Physics,WallBody,Vector3(0.5,0.5,0.5));
  WallBody.Finish;
  WallBody.SetWorldTransformation(Matrix4x4Translate(0.0,0.0,0.0));
  WallBody.CollisionGroups:=[0];
  QueryBody:=MakeQueryBody(Physics);
  QueryShape:=TKraftShapeCapsule.Create(Physics,QueryBody,0.25,1.0);
  QueryBody.Finish;
  QueryBody.SetWorldTransformation(Matrix4x4Translate(0.6,0.0,0.0));
  QueryBody.CollisionGroups:=[1];
  if Physics.PushShape(QueryShape,Seperation,[0],4,nil,nil,true) then begin
   Check('push happened',true);
   CheckVector('push separation',Seperation,Vector3(0.15,0.0,0.0),2e-2);
  end else begin
   Check('push happened',false);
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
 TestCollideShapeSphereOnSlab;
 TestCollideShapeCapsuleOnSlab;
 TestCollideShapeSDFOnPlane;
 TestCharacterKitOnSlab;
 TestPushShapeSphereOnSlab;
 TestPushShapeCapsuleOnSlab;
 TestPushShapeCapsuleOnPlane;
 TestPushShapeCapsuleVersusBox;
 TestPushShapeBoxVersusBox;
 TestPushShapeCapsuleOnMesh;
 TestPushShapeSingleDeepest;
 WriteLn('=== ',CountPassed,' passed, ',CountFailed,' failed ===');
 if CountFailed>0 then begin
  Halt(1);
 end;
end.
