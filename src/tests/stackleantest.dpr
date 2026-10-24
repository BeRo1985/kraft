program stackleantest;

{$ifdef fpc}
 {$mode delphi}
{$endif}
{$apptype console}

uses {$ifdef unix}cthreads,{$endif} SysUtils,Classes,Math,kraft;

// Perfectly symmetric stacks of the sandbox kind (0.5 m boxes, density 100, restitution 0.4), a ten box tower
// and pyramids of height 10 and 20, set up exactly, must not lean: any sideways drift of a box comes only from
// the order in which the solver runs through the contact points. The sequential impulse solver runs the contact
// points backwards on every second iteration, which more than halves the lean of the pyramids. The TGS soft
// solver keeps the friction out of the solve pass like Box3D; solving it there too lets stacks lean less, but a
// box rocking flat on the floor then gains energy and never comes to rest, which the last check guards against.
// The scalar TGS contact stage and its wide variants in Pascal and assembler have to agree.

const Names:array[0..3] of string=('SI','TGS','TGS wide','TGS wide asm');

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

function CreatePhysics(const aSolver:longint):TKraft;
var Floor:TKraftRigidBody;
    FloorShape:TKraftShapePlane;
begin
 result:=TKraft.Create(1);
 result.SetFrequency(120.0);
 result.VelocityIterations:=8;
 result.PositionIterations:=3;
 result.Gravity.y:=-9.81;
 if aSolver=0 then begin
  result.SolverMode:=ksmSequentialImpulse;
 end else begin
  result.SolverMode:=ksmTGSSoft;
  result.WideContactSolver:=aSolver>1;
  result.WideContactSolverAssembler:=aSolver>2;
 end;
 Floor:=TKraftRigidBody.Create(result);
 Floor.SetRigidBodyType(krbtSTATIC);
 FloorShape:=TKraftShapePlane.Create(result,Floor,Plane(Vector3(0.0,1.0,0.0),0.0));
 FloorShape.Restitution:=0.4;
 Floor.Finish;
 Floor.SetWorldTransformation(Matrix4x4Identity);
 Floor.CollisionGroups:=[0];
end;

// Returns the largest sideways drift of any box in millimetres, and how many boxes left their place by more than
// ten centimetres
function Drift(const aSolver:longint;const aPyramid:boolean;const aSize:longint;out aMoved:longint):TKraftScalar;
var Physics:TKraft;
    Body:TKraftRigidBody;
    Shape:TKraftShapeBox;
    Bodies:array of TKraftRigidBody;
    Starts:array of TKraftVector3;
    Count,Row,Column,StepIndex,Index:longint;
    Position:TKraftVector3;
 procedure AddBox(const aX,aY:TKraftScalar);
 begin
  Body:=TKraftRigidBody.Create(Physics);
  Body.SetRigidBodyType(krbtDYNAMIC);
  Shape:=TKraftShapeBox.Create(Physics,Body,Vector3(0.25,0.25,0.25));
  Shape.Restitution:=0.4;
  Shape.Density:=100.0;
  Body.Finish;
  Body.SetWorldTransformation(Matrix4x4Translate(aX,aY,0.0));
  Body.CollisionGroups:=[0];
  SetLength(Bodies,Count+1);
  SetLength(Starts,Count+1);
  Bodies[Count]:=Body;
  Starts[Count]:=Vector3(aX,aY,0.0);
  inc(Count);
 end;
begin
 Physics:=CreatePhysics(aSolver);
 try
  Bodies:=nil;
  Starts:=nil;
  Count:=0;
  if aPyramid then begin
   for Row:=0 to aSize-1 do begin
    for Column:=0 to Row do begin
     AddBox((Column-(Row*0.5))*(0.25*2.05),0.25+((aSize-(Row+1))*0.5));
    end;
   end;
  end else begin
   for Row:=0 to aSize-1 do begin
    AddBox(0.0,0.25+(Row*0.5));
   end;
  end;
  result:=0.0;
  for StepIndex:=1 to 900 do begin
   Physics.Step(1.0/120.0);
   for Index:=0 to Count-1 do begin
    result:=Max(result,sqrt(sqr(Bodies[Index].WorldTransform[3,0]-Starts[Index].x)+sqr(Bodies[Index].WorldTransform[3,2]-Starts[Index].z)));
   end;
  end;
  aMoved:=0;
  for Index:=0 to Count-1 do begin
   Position:=Vector3(Bodies[Index].WorldTransform[3,0],Bodies[Index].WorldTransform[3,1],Bodies[Index].WorldTransform[3,2]);
   if Vector3Dist(Position,Starts[Index])>0.1 then begin
    inc(aMoved);
   end;
  end;
  result:=result*1000.0;
 finally
  Physics.Free;
 end;
end;

// A small flat box (0.36 x 0.24 x 0.24 m) dropped tilted onto the floor; returns its largest angular speed in
// the second half of ten seconds
function RockingAfterDrop(const aSolver:longint):TKraftScalar;
var Physics:TKraft;
    Body:TKraftRigidBody;
    Shape:TKraftShapeBox;
    StepIndex:longint;
begin
 Physics:=CreatePhysics(aSolver);
 try
  Body:=TKraftRigidBody.Create(Physics);
  Body.SetRigidBodyType(krbtDYNAMIC);
  Shape:=TKraftShapeBox.Create(Physics,Body,Vector3(0.18,0.12,0.12));
  Shape.Restitution:=0.1;
  Shape.Density:=1.0;
  Body.Finish;
  Body.SetWorldTransformation(Matrix4x4TermMul(Matrix4x4RotateZ(0.2),Matrix4x4Translate(0.0,0.36,0.0)));
  Body.CollisionGroups:=[0];
  result:=0.0;
  for StepIndex:=1 to 1200 do begin
   Physics.Step(1.0/120.0);
   if StepIndex>600 then begin
    result:=Max(result,Vector3Length(Body.AngularVelocity));
   end;
  end;
 finally
  Physics.Free;
 end;
end;

var Solver,Moved:longint;
    Tower,Pyramid10,Pyramid20,Rocking:array[0..3] of TKraftScalar;
begin
 FormatSettings.DecimalSeparator:='.';
 CountPassed:=0;
 CountFailed:=0;
 for Solver:=0 to 3 do begin
  Tower[Solver]:=Drift(Solver,false,10,Moved);
  Check(Names[Solver]+': the ten box tower stands',Moved=0);
  Pyramid10[Solver]:=Drift(Solver,true,10,Moved);
  Check(Names[Solver]+': the pyramid of height 10 stands',Moved=0);
  Pyramid20[Solver]:=Drift(Solver,true,20,Moved);
  Check(Names[Solver]+': the pyramid of height 20 stands',Moved=0);
  Rocking[Solver]:=RockingAfterDrop(Solver);
  Check(Names[Solver]+': a box dropped tilted onto the floor comes to rest',Rocking[Solver]<0.01);
  WriteLn(Format('  %-12s drift: tower %7.2f mm, pyramid 10 %7.2f mm, pyramid 20 %7.2f mm; late |w| of the dropped box %6.3f',[Names[Solver],Tower[Solver],Pyramid10[Solver],Pyramid20[Solver],Rocking[Solver]]));
 end;
 Check('SI: the ten box tower leans less than 25 mm',Tower[0]<25.0);
 Check('SI: the pyramid of height 10 leans less than 2.5 mm',Pyramid10[0]<2.5);
 Check('SI: the pyramid of height 20 leans less than 8 mm',Pyramid20[0]<8.0);
 Check('TGS: the ten box tower leans less than 10 mm',Tower[1]<10.0);
 Check('TGS: the pyramid of height 10 leans less than 2.5 mm',Pyramid10[1]<2.5);
 Check('TGS: the pyramid of height 20 leans less than 8 mm',Pyramid20[1]<8.0);
 for Solver:=2 to 3 do begin
  Check(Names[Solver]+' agrees with the scalar TGS stage',
        (abs(Tower[Solver]-Tower[1])<0.01) and (abs(Pyramid10[Solver]-Pyramid10[1])<0.01) and (abs(Pyramid20[Solver]-Pyramid20[1])<0.01) and
        (abs(Rocking[Solver]-Rocking[1])<0.001));
 end;
 WriteLn;
 WriteLn('=== ',CountPassed,' passed, ',CountFailed,' failed ===');
 if CountFailed>0 then begin
  Halt(1);
 end;
end.
