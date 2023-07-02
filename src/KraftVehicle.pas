(******************************************************************************
 *                      VEHICLE PHYSICS FOR KRAFT PHYSICS ENGINE              *
 ******************************************************************************
 *                        Version 2023-07-02-10-26-0000                       *
 ******************************************************************************
 *                                zlib license                                *
 *============================================================================*
 *                                                                            *
 * Copyright (c) 2023-2023, Benjamin Rosseaux (benjamin@rosseaux.de)          *
 *                                                                            *
 * This software is provided 'as-is', without any express or implied          *
 * warranty. In no event will the authors be held liable for any damages      *
 * arising from the use of this software.                                     *
 *                                                                            *
 * Permission is granted to anyone to use this software for any purpose,      *
 * including commercial applications, and to alter it and redistribute it     *
 * freely, subject to the following restrictions:                             *
 *                                                                            *
 * 1. The origin of this software must not be misrepresented; you must not    *
 *    claim that you wrote the original software. If you use this software    *
 *    in a product, an acknowledgement in the product documentation would be  *
 *    appreciated but is not required.                                        *
 * 2. Altered source versions must be plainly marked as such, and must not be *
 *    misrepresented as being the original software.                          *
 * 3. This notice may not be removed or altered from any source distribution. *
 *                                                                            *
 ******************************************************************************
 *                  General guidelines for code contributors                  *
 *============================================================================*
 *                                                                            *
 * 1. Make sure you are legally allowed to make a contribution under the zlib *
 *    license.                                                                *
 * 2. The zlib license header goes at the top of each source file, with       *
 *    appropriate copyright notice.                                           *
 * 3. After a pull request, check the status of your pull request on          *
      http://github.com/BeRo1985/kraft                                        *
 * 4. Write code, which is compatible with lastest Delphi and lastest         *
 *    FreePascal versions                                                     *
 * 5. Don't use Delphi VCL, FreePascal FCL or Lazarus LCL libraries/units.    *
 * 6. No use of third-party libraries/units as possible, but if needed, make  *
 *    it out-ifdef-able                                                       *
 * 7. Try to use const when possible.                                         *
 * 8. Make sure to comment out writeln, used while debugging                  *
 * 9. Use TKraftScalar instead of float/double so that Kraft can be compiled  *
 *    as double/single precision.                                             *
 * 10. Make sure the code compiles on 32-bit and 64-bit platforms in single   *
 *     and double precision.                                                  *
 *                                                                            *
 ******************************************************************************)
unit KraftVehicle;
{$ifdef fpc}
 {$mode delphi}
 {$warnings off}
 {$hints off}
 {$define caninline}
 {$ifdef cpui386}
  {$define cpu386}
 {$endif}
 {$ifdef cpuamd64}
  {$define cpux86_64}
  {$define cpux64}
 {$else}
  {$ifdef cpux86_64}
   {$define cpuamd64}
   {$define cpux64}
  {$endif}
 {$endif}
 {$ifdef cpu386}
  {$define cpu386}
  {$asmmode intel}
  {$define canx86simd}
 {$endif}
 {$ifdef FPC_LITTLE_ENDIAN}
  {$define LITTLE_ENDIAN}
 {$else}
  {$ifdef FPC_BIG_ENDIAN}
   {$define BIG_ENDIAN}
  {$endif}
 {$endif}
 {$packset fixed}
{$else}
 {$define LITTLE_ENDIAN}
 {$ifndef cpu64}
  {$define cpu32}
 {$endif}
 {$safedivide off}
 {$optimization on}
 {$undef caninline}
 {$undef canx86simd}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
 {$ifdef ver180}
  {$define caninline}
  {$ifdef cpu386}
   {$define canx86simd}
  {$endif}
  {$finitefloat off}
 {$endif}
{$endif}
{$ifdef win32}
 {$define windows}
{$endif}
{$ifdef win64}
 {$define windows}
{$endif}
{$extendedsyntax on}
{$writeableconst on}
{$varstringchecks on}
{$typedaddress off}
{$overflowchecks off}
{$rangechecks off}
{$ifndef fpc}
{$realcompatibility off}
{$endif}
{$openstrings on}
{$longstrings on}
{$booleval off}
{$typeinfo on}

{-$define UseMoreCollisionGroups}

{$define UseTriangleMeshFullPerturbation}

{-$define DebugDraw}

{-$define memdebug}

{$ifdef UseDouble}
 {$define NonSIMD}
{$endif}

{-$define NonSIMD}

{$ifdef NonSIMD}
 {$undef CPU386ASMForSinglePrecision}
 {$undef SIMD}
{$else}
 {$ifdef cpu386}
  {$if not (defined(Darwin) or defined(CompileForWithPIC))}
   {$define CPU386ASMForSinglePrecision}
  {$ifend}
 {$endif}
 {$undef SIMD}
 {$ifdef CPU386ASMForSinglePrecision}
  {$define SIMD}
 {$endif}
{$endif}

interface

uses {$ifdef windows}
      Windows,
      MMSystem,
     {$else}
      {$ifdef unix}
       BaseUnix,
       Unix,
       UnixType,
       {$if defined(linux) or defined(android)}
        linux,
       {$ifend}
      {$else}
       SDL,
      {$endif}
     {$endif}
     {$ifdef DebugDraw}
      {$ifndef NoOpenGL}
       {$ifdef fpc}
        GL,
        GLext,
       {$else}
        OpenGL,
       {$endif}
      {$endif}
     {$endif}
     SysUtils,
     Classes,
     SyncObjs,
{$ifdef KraftPasMP}
     PasMP,
{$endif}
{$ifdef KraftPasJSON}
     PasJSON,
{$endif}
     Math,
     Kraft;

type { TKraftVehicle }
     TKraftVehicle=class
      public
       type { TWheel }
            TWheel=class
             private
              fVehicle:TKraftVehicle;
              fChassisConnectionPointLocal:TKraftVector3;
              fChassisConnectionPointWorld:TKraftVector3;
              fDirectionLocal:TKraftVector3;
              fDirectionWorld:TKraftVector3;
              fAxleLocal:TKraftVector3;
              fAxleWorld:TKraftVector3;
              fSuspensionRestLength:TKraftScalar;
              fSuspensionMaxLength:TKraftScalar;
              fRadius:TKraftScalar;
              fSuspensionStiffness:TKraftScalar;
              fDampingCompression:TKraftScalar;
              fDampingRelaxation:TKraftScalar;
              fFrictionSlip:TKraftScalar;
              fSteering:TKraftScalar;
              fRotation:TKraftScalar;
              fDeltaRotation:TKraftScalar;
              fRollInfluence:TKraftScalar;
              fMaxSuspensionForce:TKraftScalar;
              fIsFrontWheel:boolean;
              fClippedInvContactDotSuspension:TKraftScalar;
              fSuspensionRelativeVelocity:TKraftScalar;
              fSuspensionForce:TKraftScalar;
              fSkidInfo:TKraftScalar;
              fSuspensionLength:TKraftScalar;
              fMaxSuspensionTravel:TKraftScalar;
              fUseCustomSlidingRotationalSpeed:boolean;
              fCustomSlidingRotationalSpeed:TKraftScalar;
              fSliding:boolean;
              fEngineForce:TKraftScalar;
              fBrake:TKraftScalar;
              fSideImpulse:TKraftScalar;
              fForwardImpulse:TKraftScalar;
              fIsInContact:boolean;
              fRayCastHitValid:boolean;
              fRayCastHitPoint:TKraftVector3;
              fRayCastHitNormal:TKraftVector3;
              fRayCastHitDistance:TKraftScalar;
              fWorldTransform:TKraftMatrix4x4;
             public
              constructor Create(const aVehicle:TKraftVehicle); reintroduce;
              destructor Destroy; override;
{$ifdef KraftPasJSON}
              procedure LoadFromJSON(const aJSONItem:TPasJSONItem);
              function SaveToJSON:TPasJSONItem;
{$endif}
              procedure Update(const aChassis:TKraftRigidBody);
             public
              property ChassisConnectionPointLocal:TKraftVector3 read fChassisConnectionPointLocal write fChassisConnectionPointLocal;
              property ChassisConnectionPointWorld:TKraftVector3 read fChassisConnectionPointWorld write fChassisConnectionPointWorld;
              property DirectionLocal:TKraftVector3 read fDirectionLocal write fDirectionLocal;
              property DirectionWorld:TKraftVector3 read fDirectionWorld write fDirectionWorld;
              property AxleLocal:TKraftVector3 read fAxleLocal write fAxleLocal;
              property AxleWorld:TKraftVector3 read fAxleWorld write fAxleWorld;
              property SuspensionRestLength:TKraftScalar read fSuspensionRestLength write fSuspensionRestLength;
              property SuspensionMaxLength:TKraftScalar read fSuspensionMaxLength write fSuspensionMaxLength;
              property Radius:TKraftScalar read fRadius write fRadius;
              property SuspensionStiffness:TKraftScalar read fSuspensionStiffness write fSuspensionStiffness;
              property DampingCompression:TKraftScalar read fDampingCompression write fDampingCompression;
              property DampingRelaxation:TKraftScalar read fDampingRelaxation write fDampingRelaxation;
              property FrictionSlip:TKraftScalar read fFrictionSlip write fFrictionSlip;
              property Steering:TKraftScalar read fSteering write fSteering;
              property Rotation:TKraftScalar read fRotation write fRotation;
              property DeltaRotation:TKraftScalar read fDeltaRotation write fDeltaRotation;
              property RollInfluence:TKraftScalar read fRollInfluence write fRollInfluence;
              property MaxSuspensionForce:TKraftScalar read fMaxSuspensionForce write fMaxSuspensionForce;
              property IsFrontWheel:boolean read fIsFrontWheel write fIsFrontWheel;
              property ClippedInvContactDotSuspension:TKraftScalar read fClippedInvContactDotSuspension write fClippedInvContactDotSuspension;
              property SuspensionRelativeVelocity:TKraftScalar read fSuspensionRelativeVelocity write fSuspensionRelativeVelocity;
              property SuspensionForce:TKraftScalar read fSuspensionForce write fSuspensionForce;
              property SkidInfo:TKraftScalar read fSkidInfo write fSkidInfo;
              property SuspensionLength:TKraftScalar read fSuspensionLength write fSuspensionLength;
              property MaxSuspensionTravel:TKraftScalar read fMaxSuspensionTravel write fMaxSuspensionTravel;
              property UseCustomSlidingRotationalSpeed:boolean read fUseCustomSlidingRotationalSpeed write fUseCustomSlidingRotationalSpeed;
              property CustomSlidingRotationalSpeed:TKraftScalar read fCustomSlidingRotationalSpeed write fCustomSlidingRotationalSpeed;
              property Sliding:boolean read fSliding write fSliding;
              property EngineForce:TKraftScalar read fEngineForce write fEngineForce;
              property Brake:TKraftScalar read fBrake write fBrake;
              property SideImpulse:TKraftScalar read fSideImpulse write fSideImpulse;
              property ForwardImpulse:TKraftScalar read fForwardImpulse write fForwardImpulse;
              property IsInContact:boolean read fIsInContact write fIsInContact;
              property RayCastHitValid:boolean read fRayCastHitValid write fRayCastHitValid;
              property RayCastHitPoint:TKraftVector3 read fRayCastHitPoint write fRayCastHitPoint;
              property RayCastHitNormal:TKraftVector3 read fRayCastHitNormal write fRayCastHitNormal;
              property RayCastHitDistance:TKraftScalar read fRayCastHitDistance write fRayCastHitDistance;
              property WorldTransform:TKraftMatrix4x4 read fWorldTransform write fWorldTransform;
            end;
      private
       fPhysics:TKraft;
      public
       constructor Create(const aPhysics:TKraft); reintroduce;
       destructor Destroy; override;
     end;

implementation

function JSONToVector3(const aVectorJSONItem:TPasJSONItem):TKraftVector3;
begin
 if assigned(aVectorJSONItem) and (aVectorJSONItem is TPasJSONItemArray) and (TPasJSONItemArray(aVectorJSONItem).Count=3) then begin
  result.x:=TPasJSON.GetNumber(TPasJSONItemArray(aVectorJSONItem).Items[0],0.0);
  result.y:=TPasJSON.GetNumber(TPasJSONItemArray(aVectorJSONItem).Items[1],0.0);
  result.z:=TPasJSON.GetNumber(TPasJSONItemArray(aVectorJSONItem).Items[2],0.0);
 end else if assigned(aVectorJSONItem) and (aVectorJSONItem is TPasJSONItemObject) then begin
  result.x:=TPasJSON.GetNumber(TPasJSONItemObject(aVectorJSONItem).Properties['x'],0.0);
  result.y:=TPasJSON.GetNumber(TPasJSONItemObject(aVectorJSONItem).Properties['y'],0.0);
  result.z:=TPasJSON.GetNumber(TPasJSONItemObject(aVectorJSONItem).Properties['z'],0.0);
 end else begin
  result:=Vector3Origin;
 end; 
end;

function Vector3ToJSON(const aVector:TKraftVector3):TPasJSONItemArray;
begin
 result:=TPasJSONItemArray.Create;
 result.Add(TPasJSONItemNumber.Create(aVector.x));
 result.Add(TPasJSONItemNumber.Create(aVector.y));
 result.Add(TPasJSONItemNumber.Create(aVector.z));
end;

{ TKraftVehicle.TWheel }

constructor TKraftVehicle.TWheel.Create(const aVehicle:TKraftVehicle);
begin
 inherited Create;
 fVehicle:=aVehicle;
 fChassisConnectionPointLocal:=Vector3Origin;
 fChassisConnectionPointWorld:=Vector3Origin;
 fDirectionLocal:=Vector3Origin;
 fDirectionWorld:=Vector3Origin;
 fAxleLocal:=Vector3Origin;
 fAxleWorld:=Vector3Origin;
 fSuspensionRestLength:=1.0;
 fSuspensionMaxLength:=2.0;
 fRadius:=1.0;
 fSuspensionStiffness:=100.0;
 fDampingCompression:=10.0;
 fDampingRelaxation:=10.0;
 fFrictionSlip:=10000.0;
 fSteering:=0.0;
 fRotation:=0.0;
 fDeltaRotation:=0.0;
 fRollInfluence:=0.01;
 fMaxSuspensionForce:=Infinity;
 fIsFrontWheel:=true;
 fClippedInvContactDotSuspension:=1.0;
 fSuspensionRelativeVelocity:=0.0;
 fSuspensionForce:=0.0;
 fSkidInfo:=0.0;
 fSuspensionLength:=0.0;
 fMaxSuspensionTravel:=1.0;
 fUseCustomSlidingRotationalSpeed:=false;
 fCustomSlidingRotationalSpeed:=-0.1;
 fSliding:=false;
 fEngineForce:=0.0;
 fBrake:=0.0;
 fSideImpulse:=0.0;
 fForwardImpulse:=0.0;
 fIsInContact:=false;
 fRayCastHitValid:=false;
 fRayCastHitPoint:=Vector3Origin;
 fRayCastHitNormal:=Vector3Origin;
 fRayCastHitDistance:=0.0;
 fWorldTransform:=Matrix4x4Identity;
end;

destructor TKraftVehicle.TWheel.Destroy;
begin
 inherited Destroy;
end;

{$ifdef KraftPasJSON}
procedure TKraftVehicle.TWheel.LoadFromJSON(const aJSONItem:TPasJSONItem);
begin
 if assigned(aJSONItem) and (aJSONItem is TPasJSONItemObject) then begin
  fChassisConnectionPointLocal:=JSONToVector3(TPasJSONItemObject(aJSONItem).Properties['chassisconnectionpointlocal']);
  fDirectionLocal:=JSONToVector3(TPasJSONItemObject(aJSONItem).Properties['directionlocal']);
  fAxleLocal:=JSONToVector3(TPasJSONItemObject(aJSONItem).Properties['axlelocal']);
  fSuspensionRestLength:=TPasJSON.GetNumber(TPasJSONItemObject(aJSONItem).Properties['suspensionrestlength'],fSuspensionRestLength);
  fSuspensionMaxLength:=TPasJSON.GetNumber(TPasJSONItemObject(aJSONItem).Properties['suspensionmaxlength'],fSuspensionMaxLength);
  fRadius:=TPasJSON.GetNumber(TPasJSONItemObject(aJSONItem).Properties['radius'],fRadius);
  fSuspensionStiffness:=TPasJSON.GetNumber(TPasJSONItemObject(aJSONItem).Properties['suspensionstiffness'],fSuspensionStiffness);
  fDampingCompression:=TPasJSON.GetNumber(TPasJSONItemObject(aJSONItem).Properties['dampingcompression'],fDampingCompression);
  fDampingRelaxation:=TPasJSON.GetNumber(TPasJSONItemObject(aJSONItem).Properties['dampingrelaxation'],fDampingRelaxation);
  fFrictionSlip:=TPasJSON.GetNumber(TPasJSONItemObject(aJSONItem).Properties['frictionslip'],fFrictionSlip);
  fRollInfluence:=TPasJSON.GetNumber(TPasJSONItemObject(aJSONItem).Properties['rollinfluence'],fRollInfluence);
  fMaxSuspensionForce:=TPasJSON.GetNumber(TPasJSONItemObject(aJSONItem).Properties['maxsuspensionforce'],fMaxSuspensionForce);
  fIsFrontWheel:=TPasJSON.GetBoolean(TPasJSONItemObject(aJSONItem).Properties['isfrontwheel'],fIsFrontWheel);
  fMaxSuspensionTravel:=TPasJSON.GetNumber(TPasJSONItemObject(aJSONItem).Properties['maxsuspensiontravel'],fMaxSuspensionTravel);
  fUseCustomSlidingRotationalSpeed:=TPasJSON.GetBoolean(TPasJSONItemObject(aJSONItem).Properties['usecustomslidingrotationalspeed'],fUseCustomSlidingRotationalSpeed);
  fCustomSlidingRotationalSpeed:=TPasJSON.GetNumber(TPasJSONItemObject(aJSONItem).Properties['customslidingrotationalspeed'],fCustomSlidingRotationalSpeed);
 end;  
end;

function TKraftVehicle.TWheel.SaveToJSON:TPasJSONItem;
begin
 result:=TPasJSONItemObject.Create;
 TPasJSONItemObject(result).Add('chassisconnectionpointlocal',Vector3ToJSON(fChassisConnectionPointLocal));
 TPasJSONItemObject(result).Add('directionlocal',Vector3ToJSON(fDirectionLocal));
 TPasJSONItemObject(result).Add('axlelocal',Vector3ToJSON(fAxleLocal));
 TPasJSONItemObject(result).Add('suspensionrestlength',TPasJSONItemNumber.Create(fSuspensionRestLength));
 TPasJSONItemObject(result).Add('suspensionmaxlength',TPasJSONItemNumber.Create(fSuspensionMaxLength));
 TPasJSONItemObject(result).Add('radius',TPasJSONItemNumber.Create(fRadius));
 TPasJSONItemObject(result).Add('suspensionstiffness',TPasJSONItemNumber.Create(fSuspensionStiffness));
 TPasJSONItemObject(result).Add('dampingcompression',TPasJSONItemNumber.Create(fDampingCompression));
 TPasJSONItemObject(result).Add('dampingrelaxation',TPasJSONItemNumber.Create(fDampingRelaxation));
 TPasJSONItemObject(result).Add('frictionslip',TPasJSONItemNumber.Create(fFrictionSlip));
 TPasJSONItemObject(result).Add('rollinfluence',TPasJSONItemNumber.Create(fRollInfluence));
 TPasJSONItemObject(result).Add('maxsuspensionforce',TPasJSONItemNumber.Create(fMaxSuspensionForce));
 TPasJSONItemObject(result).Add('isfrontwheel',TPasJSONItemBoolean.Create(fIsFrontWheel));
 TPasJSONItemObject(result).Add('maxsuspensiontravel',TPasJSONItemNumber.Create(fMaxSuspensionTravel));
 TPasJSONItemObject(result).Add('usecustomslidingrotationalspeed',TPasJSONItemBoolean.Create(fUseCustomSlidingRotationalSpeed));
 TPasJSONItemObject(result).Add('customslidingrotationalspeed',TPasJSONItemNumber.Create(fCustomSlidingRotationalSpeed));
end;

{$endif}

procedure TKraftVehicle.TWheel.Update(const aChassis:TKraftRigidBody);
var Project,ProjectedVelocity,InvProject:TKraftScalar;
begin
 if fIsInContact then begin
  Project:=Vector3Dot(fRayCastHitNormal,fDirectionWorld);
  ProjectedVelocity:=Vector3Dot(aChassis.GetWorldLinearVelocityFromPoint(fRayCastHitPoint),fRayCastHitNormal);
  if Project>=-0.1 then begin
   fSuspensionRelativeVelocity:=0.0;
   fClippedInvContactDotSuspension:=1.0/0.1;
  end else begin
   InvProject:=1.0/Project;
   fSuspensionRelativeVelocity:=ProjectedVelocity*InvProject;
   fClippedInvContactDotSuspension:=InvProject;
  end;
 end else begin
  fSuspensionLength:=fSuspensionRestLength;
  fSuspensionRelativeVelocity:=0.0;
  fRayCastHitNormal:=fDirectionWorld;
  fClippedInvContactDotSuspension:=1.0;
 end;  
end;

{ TKraftVehicle }

constructor TKraftVehicle.Create(const aPhysics:TKraft);
begin

 inherited Create;

 fPhysics:=aPhysics;

end;

destructor TKraftVehicle.Destroy;
begin
 inherited Destroy;
end;

end.

