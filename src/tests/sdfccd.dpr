program sdfccd;
{$ifdef fpc}
 {$mode delphi}
{$endif}
{$apptype console}
uses {$ifdef unix}cthreads,{$endif} SysUtils,Math,kraft;

type { TThinSlabSDF }
     // Thin flat rounded slab, outer half extents (8.0,0.1,8.0), top face at y=0.1
     TThinSlabSDF=class(TKraftSignedDistanceField)
      public
       function GetLocalSignedDistance(const Position:TKraftVector3):TKraftScalar; override;
     end;

     { TRoundedBoxSDF }
     // Rounded box, outer half extent 0.5, edge rounding 0.25
     TRoundedBoxSDF=class(TKraftSignedDistanceField)
      public
       function GetLocalSignedDistance(const Position:TKraftVector3):TKraftScalar; override;
     end;

     TScenario=(scSphereOnThinBox,scSphereOnSDFSlab,scBoxOnSDFSlab,scSDFBoxOnThinBox,scSDFBoxOnSDFSlab);

     TCCDSetup=record
      Name:string;
      Mode:TKraftContinuousMode;
      Algorithm:TKraftTimeOfImpactAlgorithm;
     end;

const Scenarios:array[TScenario] of string=
       ('sphere r=0.25 -> thin static box    ',
        'sphere r=0.25 -> thin SDF slab      ',
        'box half=0.25 -> thin SDF slab      ',
        'SDF rounded box -> thin static box  ',
        'SDF rounded box -> thin SDF slab    ');

      Setups:array[0..5] of TCCDSetup=
       ((Name:'None (tunnel control)      ';Mode:kcmNone;Algorithm:ktoiaBilateralAdvancement),
        (Name:'Speculative                ';Mode:kcmSpeculativeContacts;Algorithm:ktoiaBilateralAdvancement),
        (Name:'MotionClamping+Bilateral   ';Mode:kcmMotionClamping;Algorithm:ktoiaBilateralAdvancement),
        (Name:'MotionClamping+Conservative';Mode:kcmMotionClamping;Algorithm:ktoiaConservativeAdvancement),
        (Name:'TOISubSteps+Bilateral      ';Mode:kcmTimeOfImpactSubSteps;Algorithm:ktoiaBilateralAdvancement),
        (Name:'TOISubSteps+Conservative   ';Mode:kcmTimeOfImpactSubSteps;Algorithm:ktoiaConservativeAdvancement));

var CountPassed,CountFailed:longint;

function SdRoundedBox(const Position,b:TKraftVector3;const r:TKraftScalar):TKraftScalar;
var q:TKraftVector3;
begin
 q:=Vector3Add(Vector3Sub(Vector3Abs(Position),b),Vector3(r,r,r));
 result:=(Vector3Length(Vector3(Max(0.0,q.x),Max(0.0,q.y),Max(0.0,q.z)))+Min(Max(q.x,Max(q.y,q.z)),0.0))-r;
end;

function TThinSlabSDF.GetLocalSignedDistance(const Position:TKraftVector3):TKraftScalar;
begin
 result:=SdRoundedBox(Position,Vector3(8.0,0.1,8.0),0.05);
end;

function TRoundedBoxSDF.GetLocalSignedDistance(const Position:TKraftVector3):TKraftScalar;
begin
 result:=SdRoundedBox(Position,Vector3(0.5,0.5,0.5),0.25);
end;

// Runs one drop and returns the lowest body center ever seen; the start heights are phased so that a
// 240 m/s body at 120 Hz (2 m per step) would step right over the thin target without continuous collision
function RunDrop(const aScenario:TScenario;const aSetup:TCCDSetup;out aFinalY:TKraftScalar):TKraftScalar;
var Physics:TKraft;
    FloorBody,Body:TKraftRigidBody;
    SlabSDF:TThinSlabSDF;
    BodySDF:TRoundedBoxSDF;
    StepIndex:longint;
    StartY:TKraftScalar;
begin
 Physics:=TKraft.Create(-1);
 try
  Physics.SetFrequency(120.0);
  Physics.Gravity.y:=-9.81;
  Physics.ContinuousMode:=aSetup.Mode;
  Physics.TimeOfImpactAlgorithm:=aSetup.Algorithm;

  FloorBody:=TKraftRigidBody.Create(Physics);
  FloorBody.SetRigidBodyType(krbtSTATIC);
  case aScenario of
   scSphereOnSDFSlab,scBoxOnSDFSlab,scSDFBoxOnSDFSlab:begin
    SlabSDF:=TThinSlabSDF.Create(Physics,true);
    SlabSDF.Finish;
    TKraftShapeSignedDistanceField.Create(Physics,FloorBody,SlabSDF);
   end;
   else {scSphereOnThinBox,scSDFBoxOnThinBox:}begin
    TKraftShapeBox.Create(Physics,FloorBody,Vector3(8.0,0.1,8.0));
   end;
  end;
  FloorBody.Finish;
  FloorBody.SetWorldTransformation(Matrix4x4Translate(0.0,0.0,0.0));
  FloorBody.CollisionGroups:=[0];

  Body:=TKraftRigidBody.Create(Physics);
  Body.SetRigidBodyType(krbtDYNAMIC);
  case aScenario of
   scSphereOnThinBox,scSphereOnSDFSlab:begin
    TKraftShapeSphere.Create(Physics,Body,0.25).Density:=1.0;
    StartY:=8.9;
   end;
   scBoxOnSDFSlab:begin
    TKraftShapeBox.Create(Physics,Body,Vector3(0.25,0.25,0.25)).Density:=1.0;
    StartY:=8.9;
   end;
   else {scSDFBoxOnThinBox,scSDFBoxOnSDFSlab:}begin
    BodySDF:=TRoundedBoxSDF.Create(Physics);
    BodySDF.Finish;
    TKraftShapeSignedDistanceField.Create(Physics,Body,BodySDF).Density:=1.0;
    StartY:=8.8;
   end;
  end;
  Body.Finish;
  Body.SetWorldTransformation(Matrix4x4Translate(0.0,StartY,0.0));
  Body.LinearVelocity:=Vector3(0.0,-240.0,0.0);
  Body.CollisionGroups:=[0];

  result:=StartY;
  for StepIndex:=1 to 240 do begin
   Physics.Step(1.0/120.0);
   result:=Min(result,Body.Sweep.c.y);
  end;
  aFinalY:=Body.Sweep.c.y;
 finally
  Physics.Free;
 end;
end;

procedure RunScenario(const aScenario:TScenario;const aExpectedSettleY:TKraftScalar);
var SetupIndex:longint;
    MinY,FinalY:TKraftScalar;
    OK:boolean;
begin
 WriteLn('=== ',Scenarios[aScenario],' (expected settle y=',aExpectedSettleY:5:2,') ===');
 for SetupIndex:=low(Setups) to high(Setups) do begin
  MinY:=RunDrop(aScenario,Setups[SetupIndex],FinalY);
  if Setups[SetupIndex].Mode=kcmNone then begin
   // Control: without continuous collision the body must tunnel, otherwise the scenario is too easy
   OK:=FinalY<-10.0;
   if OK then begin
    inc(CountPassed);
    WriteLn(' PASS ',Setups[SetupIndex].Name,' tunnels as expected (finalY=',FinalY:9:3,')');
   end else begin
    inc(CountFailed);
    WriteLn(' FAIL ',Setups[SetupIndex].Name,' should tunnel but finalY=',FinalY:9:3);
   end;
  end else begin
   OK:=(FinalY>(aExpectedSettleY-0.1)) and (FinalY<(aExpectedSettleY+0.2)) and (MinY>(aExpectedSettleY-0.5));
   if OK then begin
    inc(CountPassed);
    WriteLn(' PASS ',Setups[SetupIndex].Name,' finalY=',FinalY:8:4,' minY=',MinY:8:4);
   end else begin
    inc(CountFailed);
    WriteLn(' FAIL ',Setups[SetupIndex].Name,' finalY=',FinalY:8:4,' minY=',MinY:8:4);
   end;
  end;
 end;
end;

begin
 FormatSettings.DecimalSeparator:='.';
 SetExceptionMask([exInvalidOp,exDenormalized,exZeroDivide,exOverflow,exUnderflow,exPrecision]);
 CountPassed:=0;
 CountFailed:=0;
 RunScenario(scSphereOnThinBox,0.35);
 RunScenario(scSphereOnSDFSlab,0.35);
 RunScenario(scBoxOnSDFSlab,0.35);
 RunScenario(scSDFBoxOnThinBox,0.6);
 RunScenario(scSDFBoxOnSDFSlab,0.6);
 WriteLn('=== ',CountPassed,' passed, ',CountFailed,' failed ===');
 if CountFailed>0 then begin
  Halt(1);
 end;
end.
