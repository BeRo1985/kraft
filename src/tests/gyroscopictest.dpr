program gyroscopictest;

{$ifdef fpc}
 {$mode delphi}
{$endif}
{$apptype console}

uses {$ifdef unix}cthreads,{$endif} SysUtils,Classes,Math,kraft;

// The gyroscopic term of free rotating bodies (EnableGyroscopicForce), in both solver modes. A torque free
// body keeps its angular momentum and its rotational energy, which the implicit gyroscopic step has to keep
// up to a small dissipation for a box spinning about its stable major axis with a little tumble. A thin
// capsule spinning about its own axis, the one with the smallest moment, blew the step up while it was
// applied as a torque; the implicit step is dissipative there by design, as it turns such a spin towards the
// major axes, so it only has to stay bounded and must never gain energy. Without the gyroscopic term the
// angular velocity just stays as it is, so the momentum drifts with every turn, which the first check shows
// for comparison.

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

procedure RunFreeBody(const aName:string;const aSolverMode:TKraftSolverMode;const aGyroscopic,aCapsule:boolean;out aMomentumDeviation,aEnergyDeviation,aMaximalEnergyRatio,aMaximalAngularSpeed:double);
var Physics:TKraft;
    Body:TKraftRigidBody;
    StepIndex:longint;
    Momentum0:TKraftVector3;
    Energy0:double;
 function Momentum:TKraftVector3;
 begin
  result:=Vector3TermMatrixMul(Body.AngularVelocity,Body.WorldInertiaTensor);
 end;
 function Energy:double;
 begin
  result:=0.5*Vector3Dot(Body.AngularVelocity,Momentum);
 end;
begin
 Physics:=TKraft.Create(1);
 try
  Physics.SetFrequency(120.0);
  Physics.Gravity.y:=0.0;
  Physics.SolverMode:=aSolverMode;
  Body:=TKraftRigidBody.Create(Physics);
  Body.SetRigidBodyType(krbtDYNAMIC);
  if aCapsule then begin
   TKraftShapeCapsule.Create(Physics,Body,0.02,0.4).Density:=1.0;
  end else begin
   TKraftShapeBox.Create(Physics,Body,Vector3(0.5,0.2,0.05)).Density:=1.0;
  end;
  Body.Finish;
  Body.SetWorldTransformation(Matrix4x4Identity);
  Body.EnableGyroscopicForce:=aGyroscopic;
  Body.AngularVelocityDamp:=0.0;
  Body.LinearVelocityDamp:=0.0;
  Body.Flags:=Body.Flags-[krbfAllowSleep];
  if aCapsule then begin
   // Mostly about its own (y) axis, with a tumble
   Body.AngularVelocity:=Vector3(0.5,5.0,0.3);
  end else begin
   // Mostly about the major (z) axis of the flat box
   Body.AngularVelocity:=Vector3(0.3,0.4,4.0);
  end;
  Body.SetToAwake;
  Physics.Step(0.0);
  Momentum0:=Momentum;
  Energy0:=Energy;
  aMomentumDeviation:=0.0;
  aEnergyDeviation:=0.0;
  aMaximalEnergyRatio:=0.0;
  aMaximalAngularSpeed:=0.0;
  for StepIndex:=1 to 1200 do begin
   Physics.Step(1.0/120.0);
   aMomentumDeviation:=Max(aMomentumDeviation,Vector3Dist(Momentum,Momentum0)/Vector3Length(Momentum0));
   aEnergyDeviation:=Max(aEnergyDeviation,abs(Energy-Energy0)/Energy0);
   aMaximalEnergyRatio:=Max(aMaximalEnergyRatio,Energy/Energy0);
   aMaximalAngularSpeed:=Max(aMaximalAngularSpeed,Vector3Length(Body.AngularVelocity));
  end;
  WriteLn(Format('  %-28s momentum deviation %7.3f%%  energy deviation %7.3f%%  max |w| %8.3f',[aName,aMomentumDeviation*100.0,aEnergyDeviation*100.0,aMaximalAngularSpeed]));
 finally
  Physics.Free;
 end;
end;

procedure TestSolver(const aName:string;const aSolverMode:TKraftSolverMode);
var MomentumDeviation,EnergyDeviation,MaximalEnergyRatio,MaximalAngularSpeed:double;
begin
 RunFreeBody(aName+' box, without gyroscopic',aSolverMode,false,false,MomentumDeviation,EnergyDeviation,MaximalEnergyRatio,MaximalAngularSpeed);
 Check(aName+': without the gyroscopic term the momentum of a tumbling box drifts',MomentumDeviation>0.05);
 RunFreeBody(aName+' box, with gyroscopic',aSolverMode,true,false,MomentumDeviation,EnergyDeviation,MaximalEnergyRatio,MaximalAngularSpeed);
 Check(aName+': with it the momentum of a tumbling box is kept',MomentumDeviation<0.03);
 Check(aName+': with it the energy of a tumbling box is kept',EnergyDeviation<0.02);
 RunFreeBody(aName+' thin capsule, with gyroscopic',aSolverMode,true,true,MomentumDeviation,EnergyDeviation,MaximalEnergyRatio,MaximalAngularSpeed);
 Check(aName+': a thin capsule spinning about its own axis does not blow up',(not IsNaN(MaximalAngularSpeed)) and (MaximalAngularSpeed<10.0));
 Check(aName+': a thin capsule spinning about its own axis never gains energy',MaximalEnergyRatio<1.01);
end;

begin
 FormatSettings.DecimalSeparator:='.';
 SetExceptionMask([exInvalidOp,exDenormalized,exZeroDivide,exOverflow,exUnderflow,exPrecision]);
 CountPassed:=0;
 CountFailed:=0;
 TestSolver('SI',ksmSequentialImpulse);
 TestSolver('TGS',ksmTGSSoft);
 WriteLn;
 WriteLn('=== ',CountPassed,' passed, ',CountFailed,' failed ===');
 if CountFailed>0 then begin
  Halt(1);
 end;
end.
