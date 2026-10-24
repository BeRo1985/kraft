program meshcontacttest;

{$ifdef fpc}
 {$mode delphi}
{$endif}
{$apptype console}

uses {$ifdef unix}cthreads,{$endif} SysUtils,Classes,Math,kraft;

// Mesh versus mesh contacts are built from triangle versus triangle pairs. Two triangles of solid meshes whose
// planes have passed through each other, like the bottom of a box dropped onto another box a few centimetres
// deep, do not overlap as surfaces, so a separating axis test of the two triangles alone sees them apart. This
// test drops, stacks and slides box shaped meshes in the sequential impulse and the TGS soft solver and checks
// that they behave like convex boxes: no deep impact penetration, nothing slides off a stack, a box inside a
// hollow static mesh rests on its inner floor, a box over the edge of a platform keeps its place or tips over as
// its centre of mass says, and a mesh wound the other way round gets a positive mass. Such an inside out mesh
// still collides like a shell seen from within, so a stack of them sinks into its floor, but it no longer flies
// off with a negative mass.

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

function SolverName(const aTGS:boolean):string;
begin
 if aTGS then begin
  result:='TGS';
 end else begin
  result:='SI';
 end;
end;

function MakeBoxMesh(const aPhysics:TKraft;const aExtentX,aExtentY,aExtentZ:TKraftScalar;const aInverted,aSingleSided:boolean):TKraftMesh;
var VertexIndices:array[0..7] of TKraftInt32;
 procedure AddTriangle(const aIndex0,aIndex1,aIndex2:longint);
 begin
  if aInverted then begin
   result.AddTriangle(VertexIndices[aIndex0],VertexIndices[aIndex2],VertexIndices[aIndex1]);
  end else begin
   result.AddTriangle(VertexIndices[aIndex0],VertexIndices[aIndex1],VertexIndices[aIndex2]);
  end;
 end;
begin
 result:=TKraftMesh.Create(aPhysics);
 if aSingleSided then begin
  result.DoubleSided:=false;
 end;
 VertexIndices[0]:=result.AddVertex(Vector3(-aExtentX,-aExtentY,-aExtentZ),false);
 VertexIndices[1]:=result.AddVertex(Vector3(aExtentX,-aExtentY,-aExtentZ),false);
 VertexIndices[2]:=result.AddVertex(Vector3(aExtentX,aExtentY,-aExtentZ),false);
 VertexIndices[3]:=result.AddVertex(Vector3(-aExtentX,aExtentY,-aExtentZ),false);
 VertexIndices[4]:=result.AddVertex(Vector3(-aExtentX,-aExtentY,aExtentZ),false);
 VertexIndices[5]:=result.AddVertex(Vector3(aExtentX,-aExtentY,aExtentZ),false);
 VertexIndices[6]:=result.AddVertex(Vector3(aExtentX,aExtentY,aExtentZ),false);
 VertexIndices[7]:=result.AddVertex(Vector3(-aExtentX,aExtentY,aExtentZ),false);
 // Outward counter clockwise winding on all six faces (or all clockwise when inverted)
 AddTriangle(4,5,6);
 AddTriangle(4,6,7);
 AddTriangle(1,0,3);
 AddTriangle(1,3,2);
 AddTriangle(5,1,2);
 AddTriangle(5,2,6);
 AddTriangle(0,4,7);
 AddTriangle(0,7,3);
 AddTriangle(3,7,6);
 AddTriangle(3,6,2);
 AddTriangle(0,1,5);
 AddTriangle(0,5,4);
 result.Finish;
end;

function MakeBody(const aPhysics:TKraft;const aType:TKraftRigidBodyType;const aExtentX,aExtentY,aExtentZ:TKraftScalar;const aTransform:TKraftMatrix4x4;const aInverted,aSingleSided:boolean):TKraftRigidBody;
begin
 result:=TKraftRigidBody.Create(aPhysics);
 result.SetRigidBodyType(aType);
 TKraftShapeMesh.Create(aPhysics,result,MakeBoxMesh(aPhysics,aExtentX,aExtentY,aExtentZ,aInverted,aSingleSided));
 result.Finish;
 result.SetWorldTransformation(aTransform);
 result.CollisionGroups:=[0];
end;

function CreatePhysics(const aTGS:boolean):TKraft;
begin
 result:=TKraft.Create(1);
 if aTGS then begin
  result.SolverMode:=ksmTGSSoft;
 end else begin
  result.SolverMode:=ksmSequentialImpulse;
 end;
end;

function LowestCornerY(const aBody:TKraftRigidBody;const aExtent:TKraftScalar):TKraftScalar;
var Index:longint;
    Corner:TKraftVector3;
begin
 result:=MAX_SCALAR;
 for Index:=0 to 7 do begin
  Corner:=Vector3TermMatrixMul(Vector3(aExtent*(((Index and 1) shl 1)-1),
                                       aExtent*((((Index shr 1) and 1) shl 1)-1),
                                       aExtent*((((Index shr 2) and 1) shl 1)-1)),aBody.WorldTransform);
  result:=Min(result,Corner.y);
 end;
end;

// Drops a one metre mesh box from 0.1 to 0.3 m onto a resting one, 20 variants with a slight offset, and returns
// how many slid off and the mean deepest impact penetration in millimetres
procedure DropOntoBox(const aTGS,aInverted,aSingleSided:boolean;out aFellOff:longint;out aPenetration:TKraftScalar);
var Physics:TKraft;
    Lower,Upper:TKraftRigidBody;
    Variant,StepIndex:longint;
    X,Z,Deepest:TKraftScalar;
begin
 aFellOff:=0;
 aPenetration:=0.0;
 for Variant:=0 to 19 do begin
  Physics:=CreatePhysics(aTGS);
  try
   MakeBody(Physics,krbtSTATIC,10.0,0.5,10.0,Matrix4x4Translate(0.0,-0.5,0.0),aInverted,aSingleSided);
   Lower:=MakeBody(Physics,krbtDYNAMIC,0.5,0.5,0.5,Matrix4x4Translate(0.0,0.55,0.0),aInverted,aSingleSided);
   X:=0.001*Variant;
   Z:=-0.0007*Variant;
   Upper:=MakeBody(Physics,krbtDYNAMIC,0.5,0.5,0.5,Matrix4x4Translate(X,1.6+(Variant*0.01),Z),aInverted,aSingleSided);
   Deepest:=0.0;
   for StepIndex:=1 to 240 do begin
    Physics.Step(1.0/60.0);
    if (abs(Upper.Sweep.c.x)<0.5) and (abs(Upper.Sweep.c.z)<0.5) then begin
     Deepest:=Max(Deepest,(Lower.Sweep.c.y+0.5)-LowestCornerY(Upper,0.5));
    end;
   end;
   if sqrt(sqr(Upper.Sweep.c.x-X)+sqr(Upper.Sweep.c.z-Z))>0.25 then begin
    inc(aFellOff);
   end;
   aPenetration:=aPenetration+Deepest;
  finally
   Physics.Free;
  end;
 end;
 aPenetration:=(aPenetration/20.0)*1000.0;
end;

procedure TestDrops(const aTGS:boolean);
var FellOff:longint;
    Penetration:TKraftScalar;
begin
 DropOntoBox(aTGS,false,false,FellOff,Penetration);
 WriteLn(Format('  %-3s double sided: fell off %d/20, mean impact penetration %6.1f mm',[SolverName(aTGS),FellOff,Penetration]));
 Check(SolverName(aTGS)+': no mesh box slides off the one it was dropped onto',FellOff=0);
 Check(SolverName(aTGS)+': a mesh box dropped onto another sinks in no deeper than a convex box (< 25 mm)',Penetration<25.0);
 DropOntoBox(aTGS,false,true,FellOff,Penetration);
 WriteLn(Format('  %-3s single sided: fell off %d/20, mean impact penetration %6.1f mm',[SolverName(aTGS),FellOff,Penetration]));
 Check(SolverName(aTGS)+': the same holds for single sided meshes',(FellOff=0) and (Penetration<25.0));
end;

procedure TestInvertedStack(const aTGS:boolean);
var Physics:TKraft;
    Lower,Upper:TKraftRigidBody;
    StepIndex:longint;
begin
 Physics:=CreatePhysics(aTGS);
 try
  MakeBody(Physics,krbtSTATIC,10.0,0.5,10.0,Matrix4x4Translate(0.0,-0.5,0.0),true,false);
  Lower:=MakeBody(Physics,krbtDYNAMIC,0.5,0.5,0.5,Matrix4x4Translate(0.0,0.55,0.0),true,false);
  Upper:=MakeBody(Physics,krbtDYNAMIC,0.5,0.5,0.5,Matrix4x4Translate(0.003,1.7,-0.002),true,false);
  Check(SolverName(aTGS)+': a mesh wound the other way round has a positive mass',(Lower.Mass>0.0) and (Upper.Mass>0.0));
  for StepIndex:=1 to 180 do begin
   Physics.Step(1.0/60.0);
  end;
  WriteLn(Format('  %-3s inverted stack: lower y %6.3f, upper y %6.3f',[SolverName(aTGS),Lower.Sweep.c.y,Upper.Sweep.c.y]));
  Check(SolverName(aTGS)+': a stack of inverted meshes does not fly off',
        (Lower.Sweep.c.y<0.55) and (Upper.Sweep.c.y<1.7) and
        (Vector3Length(Lower.LinearVelocity)<0.1) and (Vector3Length(Upper.LinearVelocity)<0.1));
 finally
  Physics.Free;
 end;
end;

procedure TestHollowBox(const aTGS:boolean);
var Physics:TKraft;
    Box:TKraftRigidBody;
    StepIndex:longint;
begin
 Physics:=CreatePhysics(aTGS);
 try
  // A hollow static box from -1 to 9 m, whose centre is far above the box dropped onto its inner floor
  MakeBody(Physics,krbtSTATIC,5.0,5.0,5.0,Matrix4x4Translate(0.0,4.0,0.0),false,false);
  Box:=MakeBody(Physics,krbtDYNAMIC,0.5,0.5,0.5,Matrix4x4Translate(0.3,0.0,0.2),false,false);
  for StepIndex:=1 to 300 do begin
   Physics.Step(1.0/60.0);
  end;
  WriteLn(Format('  %-3s box inside a hollow static mesh: y %8.3f (rest -0.500)',[SolverName(aTGS),Box.Sweep.c.y]));
  Check(SolverName(aTGS)+': a box inside a hollow static mesh rests on its inner floor',abs(Box.Sweep.c.y+0.5)<0.02);
 finally
  Physics.Free;
 end;
end;

procedure TestPlatformEdge(const aTGS:boolean;const aX:TKraftScalar;const aShouldFall:boolean);
var Physics:TKraft;
    Box:TKraftRigidBody;
    StepIndex:longint;
begin
 Physics:=CreatePhysics(aTGS);
 try
  MakeBody(Physics,krbtSTATIC,10.0,0.5,10.0,Matrix4x4Translate(0.0,-0.5,0.0),false,false);
  MakeBody(Physics,krbtSTATIC,1.0,0.5,1.0,Matrix4x4Translate(0.0,0.5,0.0),false,false);
  Box:=MakeBody(Physics,krbtDYNAMIC,0.25,0.25,0.25,Matrix4x4Translate(aX,1.26,0.0),false,false);
  for StepIndex:=1 to 300 do begin
   Physics.Step(1.0/60.0);
  end;
  WriteLn(Format('  %-3s box at x %4.2f over a platform edge at x 1.00: x %6.3f, y %6.3f',[SolverName(aTGS),aX,Box.Sweep.c.x,Box.Sweep.c.y]));
  if aShouldFall then begin
   Check(Format('%s: a box with its centre beyond the edge (x %4.2f) tips over',[SolverName(aTGS),aX]),(Box.Sweep.c.y<0.3) and (Box.Sweep.c.y>0.2));
  end else begin
   Check(Format('%s: a box with its centre over the platform (x %4.2f) keeps its place',[SolverName(aTGS),aX]),(abs(Box.Sweep.c.x-aX)<0.01) and (Box.Sweep.c.y>1.2));
  end;
 finally
  Physics.Free;
 end;
end;

procedure TestRotatedDrop(const aTGS:boolean);
var Physics:TKraft;
    Box:TKraftRigidBody;
    StepIndex:longint;
    Flatness:TKraftScalar;
begin
 Physics:=CreatePhysics(aTGS);
 try
  MakeBody(Physics,krbtSTATIC,10.0,0.5,10.0,Matrix4x4Translate(0.0,-0.5,0.0),false,false);
  Box:=MakeBody(Physics,krbtDYNAMIC,0.5,0.5,0.5,Matrix4x4TermMul(Matrix4x4TermMul(Matrix4x4RotateY(pi*0.25),Matrix4x4RotateX(0.35)),Matrix4x4Translate(0.0,1.5,0.0)),false,false);
  for StepIndex:=1 to 480 do begin
   Physics.Step(1.0/60.0);
  end;
  Flatness:=Max(abs(Box.WorldTransform[0,1]),Max(abs(Box.WorldTransform[1,1]),abs(Box.WorldTransform[2,1])));
  WriteLn(Format('  %-3s rotated box dropped onto a mesh floor: y %6.3f, flatness %6.4f',[SolverName(aTGS),Box.Sweep.c.y,Flatness]));
  Check(SolverName(aTGS)+': a rotated mesh box dropped onto a mesh floor comes to rest flat on it',(abs(Box.Sweep.c.y-0.5)<0.01) and (Flatness>0.999));
 finally
  Physics.Free;
 end;
end;

var TGS:boolean;
begin
 FormatSettings.DecimalSeparator:='.';
 CountPassed:=0;
 CountFailed:=0;
 for TGS:=false to true do begin
  TestDrops(TGS);
  TestInvertedStack(TGS);
  TestHollowBox(TGS);
  TestPlatformEdge(TGS,0.9,false);
  TestPlatformEdge(TGS,1.05,true);
  TestRotatedDrop(TGS);
 end;
 WriteLn;
 WriteLn('=== ',CountPassed,' passed, ',CountFailed,' failed ===');
 if CountFailed>0 then begin
  Halt(1);
 end;
end.
