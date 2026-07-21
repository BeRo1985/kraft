program sdfquery;
{$ifdef fpc}
 {$mode delphi}
{$endif}
{$apptype console}
uses {$ifdef unix}cthreads,{$endif} SysUtils,Math,kraft;

type { TRoundedBoxSDF }
     // Rounded box, outer half extent 0.5, edge rounding 0.25
     TRoundedBoxSDF=class(TKraftSignedDistanceField)
      public
       function GetLocalSignedDistance(const Position:TKraftVector3):TKraftScalar; override;
     end;

     { TSlabSDF }
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

function TRoundedBoxSDF.GetLocalSignedDistance(const Position:TKraftVector3):TKraftScalar;
begin
 result:=SdRoundedBox(Position,Vector3(0.5,0.5,0.5),0.25);
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
  WriteLn(' PASS ',aName,' (got ',aGot:9:5,', want ',aWant:9:5,')');
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

var Physics:TKraft;
    BoxBody,SlabBody,BlockerBody:TKraftRigidBody;
    BoxSDF:TRoundedBoxSDF;
    SlabSDF:TSlabSDF;
    BoxShape,SlabShape:TKraftShapeSignedDistanceField;
    BlockerShape:TKraftShapeBox;
    HitShape:TKraftShape;
    HitTime,ExpectedTime:TKraftScalar;
    HitPoint,HitNormal,Direction:TKraftVector3;
    OK:boolean;
begin
 FormatSettings.DecimalSeparator:='.';
 SetExceptionMask([exInvalidOp,exDenormalized,exZeroDivide,exOverflow,exUnderflow,exPrecision]);
 CountPassed:=0;
 CountFailed:=0;

 Physics:=TKraft.Create(-1);
 try

  // Rounded box SDF at the origin
  BoxBody:=TKraftRigidBody.Create(Physics);
  BoxBody.SetRigidBodyType(krbtSTATIC);
  BoxSDF:=TRoundedBoxSDF.Create(Physics,true);
  BoxSDF.Finish;
  BoxShape:=TKraftShapeSignedDistanceField.Create(Physics,BoxBody,BoxSDF);
  BoxBody.Finish;
  BoxBody.SetWorldTransformation(Matrix4x4Translate(0.0,0.0,0.0));
  BoxBody.CollisionGroups:=[0];

  // Rotated and translated slab SDF far to the side
  SlabBody:=TKraftRigidBody.Create(Physics);
  SlabBody.SetRigidBodyType(krbtSTATIC);
  SlabSDF:=TSlabSDF.Create(Physics,true);
  SlabSDF.Finish;
  SlabShape:=TKraftShapeSignedDistanceField.Create(Physics,SlabBody,SlabSDF);
  SlabBody.Finish;
  SlabBody.SetWorldTransformation(Matrix4x4TermMul(Matrix4x4RotateY(pi*0.25),Matrix4x4Translate(100.0,10.0,0.0)));
  SlabBody.CollisionGroups:=[0];

  // Normal box blocker above the rounded box SDF, for the nearest hit ordering test
  BlockerBody:=TKraftRigidBody.Create(Physics);
  BlockerBody.SetRigidBodyType(krbtSTATIC);
  BlockerShape:=TKraftShapeBox.Create(Physics,BlockerBody,Vector3(0.5,0.25,0.5));
  BlockerBody.Finish;
  BlockerBody.SetWorldTransformation(Matrix4x4Translate(0.0,2.0,0.0));
  BlockerBody.CollisionGroups:=[0];

  Physics.Step(1.0/60.0);

  WriteLn('=== RayCast ===');

  // Vertical hit onto the rounded box (through the blocker gap? the blocker is in the way, so cast from the side)
  OK:=Physics.RayCast(Vector3(4.0,0.0,0.0),Vector3(-1.0,0.0,0.0),16.0,HitShape,HitTime,HitPoint,HitNormal,[0]);
  Check('ray -x hits box SDF',OK and (HitShape=BoxShape));
  if OK then begin
   CheckScalar('ray -x time',HitTime,3.5,1e-3);
   CheckVector('ray -x point',HitPoint,Vector3(0.5,0.0,0.0),1e-3);
   CheckVector('ray -x normal',HitNormal,Vector3(1.0,0.0,0.0),1e-2);
  end;

  // Diagonal hit onto the rounded edge: expected hit where x=y on the edge arc
  Direction:=Vector3Norm(Vector3(-1.0,-1.0,0.0));
  ExpectedTime:=(4.0-(0.25+(0.25/sqrt(2.0))))*sqrt(2.0);
  OK:=Physics.RayCast(Vector3(4.0,4.0,0.0),Direction,16.0,HitShape,HitTime,HitPoint,HitNormal,[0]);
  Check('ray diagonal hits box SDF',OK and (HitShape=BoxShape));
  if OK then begin
   CheckScalar('ray diagonal time',HitTime,ExpectedTime,1e-2);
   CheckScalar('ray diagonal point on surface',BoxSDF.GetLocalSignedDistance(HitPoint),0.0,1e-3);
   CheckVector('ray diagonal normal',HitNormal,Vector3(sqrt(0.5),sqrt(0.5),0.0),1e-2);
  end;

  // Hit onto the rotated and translated slab from above
  OK:=Physics.RayCast(Vector3(100.0,20.0,0.0),Vector3(0.0,-1.0,0.0),32.0,HitShape,HitTime,HitPoint,HitNormal,[0]);
  Check('ray hits rotated slab SDF',OK and (HitShape=SlabShape));
  if OK then begin
   CheckScalar('ray rotated slab time',HitTime,9.5,1e-3);
   CheckVector('ray rotated slab point',HitPoint,Vector3(100.0,10.5,0.0),1e-3);
   CheckVector('ray rotated slab normal',HitNormal,Vector3(0.0,1.0,0.0),1e-2);
  end;

  // Miss beside the box (blocker is only 0.5 wide in x/z, so cast far off)
  OK:=Physics.RayCast(Vector3(4.0,0.0,3.0),Vector3(-1.0,0.0,0.0),16.0,HitShape,HitTime,HitPoint,HitNormal,[0]);
  Check('ray miss beside box',not OK);

  // MaxTime shorter than the distance
  OK:=Physics.RayCast(Vector3(4.0,0.0,0.0),Vector3(-1.0,0.0,0.0),2.0,HitShape,HitTime,HitPoint,HitNormal,[0]);
  Check('ray maxtime cutoff',not OK);

  // Start inside the solid: hit immediately with an outward normal
  OK:=Physics.RayCast(Vector3(0.0,0.0,0.0),Vector3(0.0,-1.0,0.0),16.0,HitShape,HitTime,HitPoint,HitNormal,[0]);
  Check('ray from inside hits',OK and (HitShape=BoxShape));
  if OK then begin
   CheckScalar('ray from inside time',HitTime,0.0,1e-2);
  end;

  // Nearest hit ordering: from above, the blocker box at y=2 must win against the SDF box at the origin
  OK:=Physics.RayCast(Vector3(0.0,4.0,0.0),Vector3(0.0,-1.0,0.0),16.0,HitShape,HitTime,HitPoint,HitNormal,[0]);
  Check('ray nearest ordering blocker first',OK and (HitShape=BlockerShape));
  if OK then begin
   CheckScalar('ray blocker time',HitTime,1.75,1e-3);
  end;

  WriteLn('=== SphereCast ===');

  // Vertical onto the rotated slab
  OK:=Physics.SphereCast(Vector3(100.0,20.0,0.0),0.25,Vector3(0.0,-1.0,0.0),32.0,HitShape,HitTime,HitPoint,HitNormal,[0]);
  Check('spherecast hits rotated slab',OK and (HitShape=SlabShape));
  if OK then begin
   CheckScalar('spherecast rotated slab time',HitTime,9.25,1e-2);
   CheckVector('spherecast rotated slab point',HitPoint,Vector3(100.0,10.5,0.0),1e-2);
   CheckVector('spherecast rotated slab normal',HitNormal,Vector3(0.0,1.0,0.0),1e-2);
  end;

  // Horizontal onto the rounded box side
  OK:=Physics.SphereCast(Vector3(4.0,0.0,0.0),0.25,Vector3(-1.0,0.0,0.0),16.0,HitShape,HitTime,HitPoint,HitNormal,[0]);
  Check('spherecast hits box side',OK and (HitShape=BoxShape));
  if OK then begin
   CheckScalar('spherecast box side time',HitTime,3.25,1e-2);
   CheckScalar('spherecast point on surface',BoxSDF.GetLocalSignedDistance(HitPoint),0.0,1e-2);
   CheckVector('spherecast box side normal',HitNormal,Vector3(1.0,0.0,0.0),1e-2);
  end;

  // Diagonal onto the rounded edge: verify surface consistency and plausible time
  Direction:=Vector3Norm(Vector3(-1.0,-1.0,0.0));
  OK:=Physics.SphereCast(Vector3(4.0,4.0,0.0),0.25,Direction,16.0,HitShape,HitTime,HitPoint,HitNormal,[0]);
  Check('spherecast diagonal hits box',OK and (HitShape=BoxShape));
  if OK then begin
   ExpectedTime:=(4.0-(0.25+((0.25+0.25)/sqrt(2.0))))*sqrt(2.0);
   CheckScalar('spherecast diagonal time',HitTime,ExpectedTime,1e-2);
   CheckScalar('spherecast diagonal point on surface',BoxSDF.GetLocalSignedDistance(HitPoint),0.0,1e-2);
  end;

  // Miss
  OK:=Physics.SphereCast(Vector3(4.0,0.0,3.0),0.25,Vector3(-1.0,0.0,0.0),16.0,HitShape,HitTime,HitPoint,HitNormal,[0]);
  Check('spherecast miss beside box',not OK);

  // MaxTime cutoff
  OK:=Physics.SphereCast(Vector3(4.0,0.0,0.0),0.25,Vector3(-1.0,0.0,0.0),2.0,HitShape,HitTime,HitPoint,HitNormal,[0]);
  Check('spherecast maxtime cutoff',not OK);

  WriteLn('=== TestPoint ===');

  Check('testpoint inside box SDF',Physics.TestPoint(Vector3(0.0,0.0,0.0))=BoxShape);
  Check('testpoint outside box SDF',Physics.TestPoint(Vector3(0.0,1.0,0.0))=nil);
  Check('testpoint inside rotated slab',Physics.TestPoint(Vector3(100.0,10.0,0.0))=SlabShape);

  WriteLn('=== ',CountPassed,' passed, ',CountFailed,' failed ===');

 finally
  Physics.Free;
 end;

 if CountFailed>0 then begin
  Halt(1);
 end;
end.
