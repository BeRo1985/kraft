program awakelisttest;

{$ifdef fpc}
 {$mode delphi}
{$endif}
{$apptype console}

uses {$ifdef unix}cthreads,{$endif} SysUtils,Math,kraft;

// Invariant harness for the K6 stage 1 infrastructure: the dense awake rigid body list and the stable body
// indices must stay consistent over body churn (create/destroy), sleep/wake cycles and body type switches.
// Checks after every step: every dynamic/kinematic body with krbfAwake is in the awake list exactly once at
// its AwakeIndex, nobody else is, the count matches, and all body indices are unique and inside the pool
// range. Any violation fails loudly.

var TotalChecks:int64=0;

procedure Fail(const aMessage:string);
begin
 WriteLn('FAIL: ',aMessage);
 Halt(1);
end;

procedure CheckInvariants(const aPhysics:TKraft);
var RigidBody,OtherRigidBody:TKraftRigidBody;
    ExpectedCount,Index:longint;
begin
 ExpectedCount:=0;
 RigidBody:=aPhysics.RigidBodyFirst;
 while assigned(RigidBody) do begin
  if (RigidBody.RigidBodyType in [krbtDynamic,krbtKinematic]) and (krbfAwake in RigidBody.Flags) then begin
   inc(ExpectedCount);
   if RigidBody.AwakeIndex<0 then begin
    Fail('awake body not in the awake list');
   end;
   if (RigidBody.AwakeIndex>=aPhysics.CountAwakeRigidBodies) or (aPhysics.AwakeRigidBodies[RigidBody.AwakeIndex]<>RigidBody) then begin
    Fail('awake list back reference mismatch');
   end;
  end else begin
   if RigidBody.AwakeIndex>=0 then begin
    Fail('sleeping/static body still in the awake list');
   end;
  end;
  if (RigidBody.BodyIndex<0) or (RigidBody.BodyIndex>=aPhysics.RigidBodyIndexPool.CountIndices) then begin
   Fail('body index out of pool range');
  end;
  OtherRigidBody:=RigidBody.RigidBodyNext;
  while assigned(OtherRigidBody) do begin
   if OtherRigidBody.BodyIndex=RigidBody.BodyIndex then begin
    Fail('duplicate body index');
   end;
   OtherRigidBody:=OtherRigidBody.RigidBodyNext;
  end;
  RigidBody:=RigidBody.RigidBodyNext;
 end;
 if ExpectedCount<>aPhysics.CountAwakeRigidBodies then begin
  Fail('awake count mismatch: expected '+IntToStr(ExpectedCount)+' got '+IntToStr(aPhysics.CountAwakeRigidBodies));
 end;
 for Index:=0 to aPhysics.CountAwakeRigidBodies-1 do begin
  if not assigned(aPhysics.AwakeRigidBodies[Index]) then begin
   Fail('nil entry in the awake list');
  end;
 end;
 inc(TotalChecks);
end;

function SpawnBox(const aPhysics:TKraft;const aX,aY,aZ:TKraftScalar):TKraftRigidBody;
var ShapeBox:TKraftShapeBox;
begin
 result:=TKraftRigidBody.Create(aPhysics);
 result.SetRigidBodyType(krbtDynamic);
 ShapeBox:=TKraftShapeBox.Create(aPhysics,result,Vector3(0.25,0.25,0.25));
 ShapeBox.Restitution:=0.1;
 ShapeBox.Density:=100.0;
 result.Finish;
 result.SetWorldTransformation(Matrix4x4Translate(aX,aY,aZ));
 result.CollisionGroups:=[0];
end;

var Physics:TKraft;
    FloorBody:TKraftRigidBody;
    FloorShape:TKraftShapePlane;
    Boxes:array[0..15] of TKraftRigidBody;
    StepIndex,BoxIndex,ChurnCounter:longint;
begin
 FormatSettings.DecimalSeparator:='.';

 Physics:=TKraft.Create(-1);
 try
  Physics.SetFrequency(120.0);
  Physics.Gravity.y:=-9.81;
  Physics.SolverMode:=ksmTGSSoft;
  Physics.TGSJointMode:=ktjmNativeSoft;

  FloorBody:=TKraftRigidBody.Create(Physics);
  FloorBody.SetRigidBodyType(krbtSTATIC);
  FloorShape:=TKraftShapePlane.Create(Physics,FloorBody,Plane(Vector3Norm(Vector3(0.0,1.0,0.0)),0.0));
  FloorBody.Finish;
  FloorBody.SetWorldTransformation(Matrix4x4Translate(0.0,0.0,0.0));
  FloorBody.CollisionGroups:=[0];

  for BoxIndex:=0 to 15 do begin
   Boxes[BoxIndex]:=SpawnBox(Physics,((BoxIndex mod 4)-1.5)*1.5,0.5+((BoxIndex div 4)*0.6),0.0);
  end;
  CheckInvariants(Physics);

  ChurnCounter:=0;
  for StepIndex:=1 to 600 do begin

   Physics.Step(1.0/120.0);
   CheckInvariants(Physics);

   // Body churn: destroy one box and spawn a fresh one every 10 steps, so indices get released and reused
   if (StepIndex mod 10)=0 then begin
    BoxIndex:=ChurnCounter mod 16;
    inc(ChurnCounter);
    Boxes[BoxIndex].Free;
    Boxes[BoxIndex]:=SpawnBox(Physics,((BoxIndex mod 4)-1.5)*1.5,3.0,((ChurnCounter mod 3)-1)*0.5);
    CheckInvariants(Physics);
   end;

   // Forced sleep/wake cycles
   if (StepIndex mod 37)=0 then begin
    Boxes[(StepIndex div 37) mod 16].SetToSleep;
    CheckInvariants(Physics);
   end;
   if (StepIndex mod 53)=0 then begin
    Boxes[(StepIndex div 53) mod 16].SetToAwake;
    CheckInvariants(Physics);
   end;

   // Body type switches: dynamic to static and back
   if (StepIndex mod 97)=0 then begin
    BoxIndex:=(StepIndex div 97) mod 16;
    Boxes[BoxIndex].SetRigidBodyType(krbtStatic);
    CheckInvariants(Physics);
    Boxes[BoxIndex].SetRigidBodyType(krbtDynamic);
    Boxes[BoxIndex].SetToAwake;
    CheckInvariants(Physics);
   end;

  end;

  WriteLn('PASS: ',TotalChecks,' invariant checks');
 finally
  Physics.Free;
 end;
end.
