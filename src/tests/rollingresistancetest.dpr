program rollingresistancetest;

{$ifdef fpc}
 {$mode delphi}
{$endif}
{$apptype console}

uses {$ifdef unix}cthreads,{$endif} SysUtils,Classes,Math,kraft;

// A ball rolling on a plane with rolling resistance has to slow down gradually, the faster the larger the
// resistance, in every solver path (sequential impulse, TGS soft, and the wide TGS contact solver in Pascal
// and in assembler). The clamp of the rolling impulse once compared against its budget plus a fixed epsilon,
// which is far larger than the squared impulses of ordinary bodies, so it never took hold and any rolling
// resistance locked the rotation at once.

const Resistances:array[0..3] of TKraftScalar=(0.0,0.001,0.005,0.05);
      SolverNames:array[0..3] of string=('SI','TGS','TGS wide','TGS wide asm');

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

procedure Roll(const aSolver:longint;const aResistance:TKraftScalar;out aEarlySpeed,aEndSpeed:TKraftScalar);
var Physics:TKraft;
    Floor,Ball:TKraftRigidBody;
    Shape:TKraftShapeSphere;
    StepIndex:longint;
begin
 Physics:=TKraft.Create(1);
 try
  Physics.SetFrequency(120.0);
  Physics.Gravity.y:=-9.81;
  if aSolver=0 then begin
   Physics.SolverMode:=ksmSequentialImpulse;
  end else begin
   Physics.SolverMode:=ksmTGSSoft;
   Physics.WideContactSolver:=aSolver>1;
   Physics.WideContactSolverAssembler:=aSolver>2;
  end;
  Floor:=TKraftRigidBody.Create(Physics);
  Floor.SetRigidBodyType(krbtSTATIC);
  TKraftShapePlane.Create(Physics,Floor,Plane(Vector3(0.0,1.0,0.0),0.0));
  Floor.Finish;
  Floor.SetWorldTransformation(Matrix4x4Identity);
  Floor.CollisionGroups:=[0];
  Ball:=TKraftRigidBody.Create(Physics);
  Ball.SetRigidBodyType(krbtDYNAMIC);
  Shape:=TKraftShapeSphere.Create(Physics,Ball,0.1);
  Shape.Density:=1.0;
  Shape.RollingResistance:=aResistance;
  Ball.Finish;
  Ball.SetWorldTransformation(Matrix4x4Translate(0.0,0.1,0.0));
  // Rolling without slipping along x
  Ball.AngularVelocity:=Vector3(0.0,0.0,-5.0);
  Ball.LinearVelocity:=Vector3(0.5,0.0,0.0);
  Ball.CollisionGroups:=[0];
  Ball.SetToAwake;
  aEarlySpeed:=0.0;
  for StepIndex:=1 to 600 do begin
   Physics.Step(1.0/120.0);
   if StepIndex=12 then begin
    aEarlySpeed:=Vector3Length(Ball.AngularVelocity);
   end;
  end;
  aEndSpeed:=Vector3Length(Ball.AngularVelocity);
 finally
  Physics.Free;
 end;
end;

var Solver,Index:longint;
    EarlySpeeds,EndSpeeds:array[0..3] of TKraftScalar;
    Decreasing:boolean;
begin
 FormatSettings.DecimalSeparator:='.';
 CountPassed:=0;
 CountFailed:=0;
 for Solver:=0 to 3 do begin
  for Index:=0 to 3 do begin
   Roll(Solver,Resistances[Index],EarlySpeeds[Index],EndSpeeds[Index]);
   WriteLn(Format('  %-12s resistance %6.3f: |w| after 0.1 s %6.3f, after 5 s %6.3f',[SolverNames[Solver],Resistances[Index],EarlySpeeds[Index],EndSpeeds[Index]]));
  end;
  Check(SolverNames[Solver]+': rolling resistance does not lock the rotation at once',EarlySpeeds[2]>4.5);
  Decreasing:=true;
  for Index:=1 to 3 do begin
   if not ((EndSpeeds[Index]<EndSpeeds[Index-1]) or (EndSpeeds[Index-1]=0.0)) then begin
    Decreasing:=false;
   end;
  end;
  Check(SolverNames[Solver]+': the larger the resistance, the slower the ball after 5 s',Decreasing);
  Check(SolverNames[Solver]+': a small resistance still lets the ball roll on after 5 s',EndSpeeds[1]>1.0);
  Check(SolverNames[Solver]+': a large resistance stops the ball within 5 s',EndSpeeds[3]<0.1);
 end;
 WriteLn;
 WriteLn('=== ',CountPassed,' passed, ',CountFailed,' failed ===');
 if CountFailed>0 then begin
  Halt(1);
 end;
end.
