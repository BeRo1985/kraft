program ccdbullet;
{$ifdef fpc}
 {$mode delphi}
{$endif}
{$apptype console}
uses {$ifdef unix}cthreads,{$endif} SysUtils,Math,kraft;

// Fires a small fast projectile horizontally at a thin static wall and checks that the continuous collision
// detection stops it. The projectile shapes matter here: a sphere has a single support vertex, so the
// bilateral advancement separation function always ends up in its vertex/vertex case, while a box or a
// capsule meeting a wall face on lands in the edge/edge case, whose degenerate parallel edge configuration
// used to yield a zero separation plane and therefore no time of impact at all.

type TWallKind=(wkBox,wkMesh);
     TBulletKind=(bkSphere,bkBox,bkCapsule);

     TCCDSetup=record
      Name:string;
      Mode:TKraftContinuousMode;
      Algorithm:TKraftTimeOfImpactAlgorithm;
      MustHold:boolean;   // false => still known to tunnel, reported but not counted as a failure
     end;

const Setups:array[0..5] of TCCDSetup=
       ((Name:'None (tunnel control)      ';Mode:kcmNone;Algorithm:ktoiaBilateralAdvancement;MustHold:false),
        (Name:'Speculative                ';Mode:kcmSpeculativeContacts;Algorithm:ktoiaBilateralAdvancement;MustHold:true),
        (Name:'MotionClamping+Bilateral   ';Mode:kcmMotionClamping;Algorithm:ktoiaBilateralAdvancement;MustHold:true),
        (Name:'MotionClamping+Conservative';Mode:kcmMotionClamping;Algorithm:ktoiaConservativeAdvancement;MustHold:true),
        (Name:'TOISubSteps+Bilateral      ';Mode:kcmTimeOfImpactSubSteps;Algorithm:ktoiaBilateralAdvancement;MustHold:true),
        (Name:'TOISubSteps+Conservative   ';Mode:kcmTimeOfImpactSubSteps;Algorithm:ktoiaConservativeAdvancement;MustHold:true));

      Speeds:array[0..2] of TKraftScalar=(10.0,120.0,600.0);

      WallNames:array[TWallKind] of string=('static box wall ','static mesh wall');
      BulletNames:array[TBulletKind] of string=('sphere ','box    ','capsule');

      WallHalfExtent=4.0;
      WallHalfThickness=0.05;
      BulletRadius=0.05;
      StartZ=-5.0;
      Frequency=60.0;

var CountPassed,CountFailed,CountKnownIssues:longint;

function MakeWallMesh(const aPhysics:TKraft):TKraftMesh;
var v:array[0..7] of TKraftInt32;
    hx,hy,hz:TKraftScalar;
begin
 hx:=WallHalfExtent;
 hy:=WallHalfExtent;
 hz:=WallHalfThickness;
 result:=TKraftMesh.Create(aPhysics);
 v[0]:=result.AddVertex(Vector3(-hx,-hy,-hz),false);
 v[1]:=result.AddVertex(Vector3( hx,-hy,-hz),false);
 v[2]:=result.AddVertex(Vector3( hx, hy,-hz),false);
 v[3]:=result.AddVertex(Vector3(-hx, hy,-hz),false);
 v[4]:=result.AddVertex(Vector3(-hx,-hy, hz),false);
 v[5]:=result.AddVertex(Vector3( hx,-hy, hz),false);
 v[6]:=result.AddVertex(Vector3( hx, hy, hz),false);
 v[7]:=result.AddVertex(Vector3(-hx, hy, hz),false);
 // Outward counter clockwise winding on all six faces
 result.AddTriangle(v[4],v[5],v[6]); result.AddTriangle(v[4],v[6],v[7]); // +z
 result.AddTriangle(v[1],v[0],v[3]); result.AddTriangle(v[1],v[3],v[2]); // -z
 result.AddTriangle(v[5],v[1],v[2]); result.AddTriangle(v[5],v[2],v[6]); // +x
 result.AddTriangle(v[0],v[4],v[7]); result.AddTriangle(v[0],v[7],v[3]); // -x
 result.AddTriangle(v[3],v[7],v[6]); result.AddTriangle(v[3],v[6],v[2]); // +y
 result.AddTriangle(v[0],v[1],v[5]); result.AddTriangle(v[0],v[5],v[4]); // -y
 result.Finish;
end;

// Runs one shot and returns the farthest z the projectile center ever reached. A step delta time of zero
// means stepping at the configured frequency, anything else steps at that explicit delta time instead.
function Fire(const aWallKind:TWallKind;const aBulletKind:TBulletKind;const aSetup:TCCDSetup;const aSpeed:TKraftScalar;out aFinalZ:TKraftScalar;const aStepDeltaTime:TKraftScalar=0.0):TKraftScalar;
var Physics:TKraft;
    WallBody,Bullet:TKraftRigidBody;
    StepIndex:longint;
    DeltaTime:TKraftScalar;
begin
 if aStepDeltaTime>0.0 then begin
  DeltaTime:=aStepDeltaTime;
 end else begin
  DeltaTime:=1.0/Frequency;
 end;
 Physics:=TKraft.Create(0);
 try
  Physics.SetFrequency(Frequency);
  Physics.Gravity.y:=0.0;
  Physics.ContinuousMode:=aSetup.Mode;
  Physics.TimeOfImpactAlgorithm:=aSetup.Algorithm;

  WallBody:=TKraftRigidBody.Create(Physics);
  WallBody.SetRigidBodyType(krbtSTATIC);
  case aWallKind of
   wkMesh:begin
    TKraftShapeMesh.Create(Physics,WallBody,MakeWallMesh(Physics));
   end;
   else {wkBox:}begin
    TKraftShapeBox.Create(Physics,WallBody,Vector3(WallHalfExtent,WallHalfExtent,WallHalfThickness));
   end;
  end;
  WallBody.Finish;
  WallBody.SetWorldTransformation(Matrix4x4Translate(0.0,0.0,0.0));
  WallBody.CollisionGroups:=[0];

  Bullet:=TKraftRigidBody.Create(Physics);
  Bullet.SetRigidBodyType(krbtDYNAMIC);
  case aBulletKind of
   bkBox:TKraftShapeBox.Create(Physics,Bullet,Vector3(BulletRadius,BulletRadius,BulletRadius)).Density:=1.0;
   bkCapsule:TKraftShapeCapsule.Create(Physics,Bullet,BulletRadius,BulletRadius*4.0).Density:=1.0;
   else {bkSphere:}TKraftShapeSphere.Create(Physics,Bullet,BulletRadius).Density:=1.0;
  end;
  Bullet.Finish;
  Bullet.SetWorldTransformation(Matrix4x4Translate(0.0,0.0,StartZ));
  Bullet.LinearVelocity:=Vector3(0.0,0.0,aSpeed);
  Bullet.CollisionGroups:=[0];
  Bullet.SetToAwake;

  result:=StartZ;
  for StepIndex:=1 to round(2.0/DeltaTime) do begin
   Physics.Step(DeltaTime);
   result:=Max(result,Bullet.Sweep.c.z);
  end;
  aFinalZ:=Bullet.Sweep.c.z;
 finally
  Physics.Free;
 end;
end;

procedure RunCombination(const aWallKind:TWallKind;const aBulletKind:TBulletKind);
var SetupIndex,SpeedIndex:longint;
    MaxZ,FinalZ:TKraftScalar;
    WentThrough:boolean;
    Line:string;
begin
 WriteLn('=== ',BulletNames[aBulletKind],' r=',BulletRadius:4:2,' -> ',WallNames[aWallKind],
         ' thickness=',(2.0*WallHalfThickness):4:2,' at ',Frequency:5:1,' Hz ===');
 for SetupIndex:=low(Setups) to high(Setups) do begin
  Line:='';
  for SpeedIndex:=low(Speeds) to high(Speeds) do begin
   MaxZ:=Fire(aWallKind,aBulletKind,Setups[SetupIndex],Speeds[SpeedIndex],FinalZ);
   // The projectile is through once its center passed the wall plus its own radius plus some margin
   WentThrough:=MaxZ>(WallHalfThickness+BulletRadius+0.05);
   if Setups[SetupIndex].Mode=kcmNone then begin
    // Control: without continuous collision the projectile must tunnel wherever one step carries it clear
    // across the wall, otherwise the case would be too easy to say anything about the continuous modes.
    // Below that speed the discrete detection catches the projectile on its own, which is fine here.
    if WentThrough then begin
     Line:=Line+'   tunnels';
    end else if (Speeds[SpeedIndex]/Frequency)>(2.0*(WallHalfThickness+BulletRadius)) then begin
     Line:=Line+'      HELD';
     inc(CountFailed);
    end else begin
     Line:=Line+'  discrete';
    end;
   end else if WentThrough then begin
    if Setups[SetupIndex].MustHold then begin
     Line:=Line+'   THROUGH';
     inc(CountFailed);
    end else begin
     Line:=Line+'   through';
     inc(CountKnownIssues);
    end;
   end else begin
    Line:=Line+'      held';
    if Setups[SetupIndex].MustHold then begin
     inc(CountPassed);
    end;
   end;
  end;
  WriteLn(' ',Setups[SetupIndex].Name,Line);
 end;
end;

// Step also takes an explicit delta time, and a caller stepping slower than the configured frequency must
// still get its motion predicted over the whole step, or the mid phase hands the continuous modes no
// triangle to test against and every one of them lets the projectile pass
procedure RunStepDeltaTimeMismatch;
const Divisors:array[0..3] of TKraftScalar=(120.0,60.0,30.0,20.0);
var SetupIndex,DivisorIndex:longint;
    MaxZ,FinalZ:TKraftScalar;
    Line:string;
begin
 WriteLn('=== sphere -> static mesh wall at 600 m/s, SetFrequency(',Frequency:5:1,') with mismatched Step delta times ===');
 Write('   Step(1/x), x =              ');
 for DivisorIndex:=low(Divisors) to high(Divisors) do begin
  Write(Format('%10.0f',[Divisors[DivisorIndex]]));
 end;
 WriteLn;
 for SetupIndex:=low(Setups) to high(Setups) do begin
  if not Setups[SetupIndex].MustHold then begin
   continue;
  end;
  Line:='';
  for DivisorIndex:=low(Divisors) to high(Divisors) do begin
   MaxZ:=Fire(wkMesh,bkSphere,Setups[SetupIndex],600.0,FinalZ,1.0/Divisors[DivisorIndex]);
   if MaxZ>(WallHalfThickness+BulletRadius+0.05) then begin
    Line:=Line+'   THROUGH';
    inc(CountFailed);
   end else begin
    Line:=Line+'      held';
    inc(CountPassed);
   end;
  end;
  WriteLn(' ',Setups[SetupIndex].Name,Line);
 end;
end;

var WallKind:TWallKind;
    BulletKind:TBulletKind;
    SpeedIndex:longint;
    Header:string;
begin
 FormatSettings.DecimalSeparator:='.';
 SetExceptionMask([exInvalidOp,exDenormalized,exZeroDivide,exOverflow,exUnderflow,exPrecision]);
 CountPassed:=0;
 CountFailed:=0;
 CountKnownIssues:=0;
 Header:='';
 for SpeedIndex:=low(Speeds) to high(Speeds) do begin
  Header:=Header+Format('%10.0f',[Speeds[SpeedIndex]]);
 end;
 WriteLn('projectile speeds [m/s]:    ',Header);
 Header:='';
 for SpeedIndex:=low(Speeds) to high(Speeds) do begin
  Header:=Header+Format('%10.2f',[Speeds[SpeedIndex]/Frequency]);
 end;
 WriteLn('travel per step    [m]:    ',Header);
 WriteLn;
 for WallKind:=low(TWallKind) to high(TWallKind) do begin
  for BulletKind:=low(TBulletKind) to high(TBulletKind) do begin
   RunCombination(WallKind,BulletKind);
  end;
 end;
 RunStepDeltaTimeMismatch;
 WriteLn;
 // Lower case "through" marks the speculative contact and time of impact sub stepping modes, which are
 // tracked separately and do not fail this test yet
 WriteLn('=== ',CountPassed,' passed, ',CountFailed,' failed, ',CountKnownIssues,' known issues ===');
 if CountFailed>0 then begin
  Halt(1);
 end;
end.
