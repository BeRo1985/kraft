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
              fSlipInfo:TKraftScalar;
              fIsInContact:boolean;
              fRayCastHitValid:boolean;
              fContactPointWorld:TKraftVector3;
              fContactNormalWorld:TKraftVector3;
              fContactDistance:TKraftScalar;
              fContactShape:TKraftShape;
              fContactRigidBody:TKraftRigidBody;
              fWorldTransform:TKraftMatrix4x4;
             public
              constructor Create(const aVehicle:TKraftVehicle); reintroduce;
              destructor Destroy; override;
{$ifdef KraftPasJSON}
              procedure LoadFromJSON(const aJSONItem:TPasJSONItem);
              function SaveToJSON:TPasJSONItem;
{$endif}
              procedure Update(const aChassis:TKraftRigidBody);
              procedure UpdateWheelTransformsWS;
              procedure UpdateWheelTransform;
              procedure ResetSuspension;
              function RayCast:TKraftScalar;
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
              property SlipInfo:TKraftScalar read fSlipInfo write fSlipInfo;
              property IsInContact:boolean read fIsInContact write fIsInContact;
              property WorldTransform:TKraftMatrix4x4 read fWorldTransform write fWorldTransform;
            end;
            TWheels=array of TWheel;
            TAxisDirection=(
             RightVector,
             UpVector,
             ForwardVector
            );
      private
       fPhysics:TKraft;
       fRigidBody:TKraftRigidBody;
       fSliding:boolean;       
       fWheels:TWheels;
       fCountWheels:TKraftInt32;
       fSuspensionStiffness:TKraftScalar;
       fSuspensionCompression:TKraftScalar;
       fSuspensionDamping:TKraftScalar;
       fMaxSuspensionTravelCM:TKraftScalar;
       fFrictionSlip:TKraftScalar;
       fMaxSuspensionForce:TKraftScalar;
       fPitchControl:TKraftScalar;
       fSteeringValue:TKraftScalar;
       fCurrentVehicleSpeedKMHour:TKraftScalar;
       fWorldTransform:TKraftMatrix4x4;
       fWorldLeft:TKraftVector3;
       fWorldRight:TKraftVector3;
       fWorldDown:TKraftVector3;
       fWorldUp:TKraftVector3;
       fWorldBackward:TKraftVector3;
       fWorldForward:TKraftVector3;
       fWorldPosition:TKraftVector3;
       fForwardVectors:array of TKraftVector3;
       fAxleVectors:array of TKraftVector3;
       function GetWheel(const aIndex:TKraftInt32):TWheel;
       function GetAxisDirection(const aAxisDirection:TAxisDirection):TKraftVector3;
      public
       constructor Create(const aPhysics:TKraft); reintroduce;
       destructor Destroy; override;
       procedure Clear;
       function AddWheel(const aWheel:TWheel=nil):TKraftInt32;
       procedure UpdateWorldTransformVectors;
       procedure UpdateSuspension(const aTimeStep:TKraftScalar);
       procedure UpdateFriction(const aTimeStep:TKraftScalar);
       procedure Update(const aTimeStep:TKraftScalar);
      public
       property WorldTransform:TKraftMatrix4x4 read fWorldTransform;
       property WorldLeft:TKraftVector3 read fWorldLeft;
       property WorldRight:TKraftVector3 read fWorldRight;
       property WorldDown:TKraftVector3 read fWorldDown;
       property WorldUp:TKraftVector3 read fWorldUp;
       property WorldBackward:TKraftVector3 read fWorldBackward;
       property WorldForward:TKraftVector3 read fWorldForward;
       property WorldPosition:TKraftVector3 read fWorldPosition; 
      published
       property Physics:TKraft read fPhysics write fPhysics;
       property RigidBody:TKraftRigidBody read fRigidBody write fRigidBody; 
       property Sliding:boolean read fSliding write fSliding;
       property Wheels[const aIndex:TKraftInt32]:TWheel read GetWheel;
       property CountWheels:TKraftInt32 read fCountWheels;
       property SuspensionStiffness:TKraftScalar read fSuspensionStiffness write fSuspensionStiffness;
       property SuspensionCompression:TKraftScalar read fSuspensionCompression write fSuspensionCompression;
       property SuspensionDamping:TKraftScalar read fSuspensionDamping write fSuspensionDamping;
       property MaxSuspensionTravelCM:TKraftScalar read fMaxSuspensionTravelCM write fMaxSuspensionTravelCM;
       property FrictionSlip:TKraftScalar read fFrictionSlip write fFrictionSlip;
       property MaxSuspensionForce:TKraftScalar read fMaxSuspensionForce write fMaxSuspensionForce;
       property PitchControl:TKraftScalar read fPitchControl write fPitchControl;
       property SteeringValue:TKraftScalar read fSteeringValue write fSteeringValue;
       property CurrentVehicleSpeedKMHour:TKraftScalar read fCurrentVehicleSpeedKMHour write fCurrentVehicleSpeedKMHour;
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
 fContactPointWorld:=Vector3Origin;
 fContactNormalWorld:=Vector3Origin;
 fContactDistance:=0.0;
 fContactShape:=nil;
 fContactRigidBody:=nil;
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
  Project:=Vector3Dot(fContactNormalWorld,fDirectionWorld);
  ProjectedVelocity:=Vector3Dot(aChassis.GetWorldLinearVelocityFromPoint(fContactPointWorld),fContactNormalWorld);
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
  fContactNormalWorld:=fDirectionWorld;
  fClippedInvContactDotSuspension:=1.0;
 end;  
end;

procedure TKraftVehicle.TWheel.UpdateWheelTransformsWS;
var ChassisTransform:TKraftMatrix4x4; 
begin
 ChassisTransform:=fVehicle.RigidBody.WorldTransform;
 fChassisConnectionPointWorld:=Vector3TermMatrixMul(fChassisConnectionPointLocal,ChassisTransform);
 fDirectionWorld:=Vector3TermMatrixMulBasis(fDirectionLocal,ChassisTransform);
 fAxleWorld:=Vector3TermMatrixMulBasis(fAxleLocal,ChassisTransform);
end;

procedure TKraftVehicle.TWheel.UpdateWheelTransform;
var Up,Right,Forwards:TKraftVector3;
    Steering:TKraftScalar;
    SteeringOrn:TKraftQuaternion;
    SteeringMat:TKraftMatrix3x3;
    RotatingOrn:TKraftQuaternion;
    RotatingMat:TKraftMatrix3x3;
    Basis2:TKraftMatrix3x3;
    CombinedMat:TKraftMatrix3x3;
begin
 
 UpdateWheelTransformsWS;
 
 Up:=Vector3Neg(fDirectionWorld);
 Right:=fAxleWorld;
 Forwards:=Vector3Norm(Vector3Cross(Up,Right));
 
 Steering:=fSteering;
 SteeringOrn:=QuaternionFromAxisAngle(Up,Steering);
 SteeringMat:=QuaternionToMatrix3x3(SteeringOrn);
 
 RotatingOrn:=QuaternionFromAxisAngle(Right,-fRotation);
 RotatingMat:=QuaternionToMatrix3x3(RotatingOrn);

 Basis2[0,0]:=-Right.x;
 Basis2[0,1]:=-Right.y;
 Basis2[0,2]:=-Right.z;
 Basis2[1,0]:=Up.x;
 Basis2[1,1]:=Up.y;
 Basis2[1,2]:=Up.z;
 Basis2[2,0]:=Forwards.x;
 Basis2[2,1]:=Forwards.y;
 Basis2[2,2]:=Forwards.z;

 CombinedMat:=Matrix3x3TermMul(Matrix3x3TermMul(SteeringMat,RotatingMat),Basis2);

 fWorldTransform:=Matrix4x4Set(CombinedMat);

 PKraftVector3(@fWorldTransform[3,0])^.xyz:=Vector3Add(fChassisConnectionPointWorld,Vector3ScalarMul(fDirectionWorld,fSuspensionLength)).xyz;

end;

procedure TKraftVehicle.TWheel.ResetSuspension;
begin
 fSuspensionLength:=fSuspensionRestLength;
 fSuspensionRelativeVelocity:=0.0;
 fContactNormalWorld:=Vector3Neg(fDirectionWorld);
 fClippedInvContactDotSuspension:=1.0;
end;

function TKraftVehicle.TWheel.RayCast:TKraftScalar;
var RayLen,HitTime,MinSuspensionLength,MaxSuspensionLength,Project,
    InvProject,ProjectedVelocity:TKraftScalar;
    RayVector,Source,ContactPoint,Target,HitPoint,HitNormal:TKraftVector3;
    HitShape:TKraftShape;    
begin
 
 UpdateWheelTransformsWS;

 result:=-1.0;

 RayLen:=fSuspensionRestLength+fRadius;

 RayVector:=Vector3ScalarMul(fDirectionWorld,RayLen);
 Source:=fChassisConnectionPointWorld;
 ContactPoint:=Vector3Add(Source,RayVector);
 Target:=ContactPoint;

 fRayCastHitValid:=fVehicle.Physics.RayCast(Source,Target,HitShape,HitTime,HitPoint,HitNormal,[0],nil);

 if fRayCastHitValid then begin
  
  fContactDistance:=HitTime*RayLen;

  result:=fContactDistance;

  fContactNormalWorld:=HitNormal;

  fIsInContact:=true;

  fContactShape:=HitShape;

  fContactRigidBody:=fContactShape.RigidBody;

  fSuspensionLength:=fContactDistance-fRadius;

  MinSuspensionLength:=fSuspensionRestLength-fMaxSuspensionTravel;
  MaxSuspensionLength:=fSuspensionRestLength+fMaxSuspensionTravel;
  fSuspensionLength:=Min(Max(fSuspensionLength,MinSuspensionLength),MaxSuspensionLength);

  fContactPointWorld:=HitPoint;

  Project:=Vector3Dot(fContactNormalWorld,fDirectionWorld);
  ProjectedVelocity:=Vector3Dot(fVehicle.fRigidBody.GetWorldLinearVelocityFromPoint(fContactPointWorld),fContactNormalWorld);
  if Project>=-0.1 then begin
   fSuspensionRelativeVelocity:=0.0;
   fClippedInvContactDotSuspension:=1.0/0.1;
  end else begin
   InvProject:=(-1.0)/Project;
   fSuspensionRelativeVelocity:=ProjectedVelocity*InvProject;
   fClippedInvContactDotSuspension:=InvProject;
  end;
 
 end else begin

  fSuspensionLength:=fSuspensionRestLength;
  fSuspensionRelativeVelocity:=0.0;
  fContactNormalWorld:=Vector3Norm(fDirectionWorld);
  fClippedInvContactDotSuspension:=1.0;

 end;

end;

{ TKraftVehicle }

constructor TKraftVehicle.Create(const aPhysics:TKraft);
begin

 inherited Create;

 fPhysics:=aPhysics;

 fRigidBody:=nil;

 fSliding:=false;

 fWheels:=nil;
 fCountWheels:=0;

 fSuspensionStiffness:=5.88;
 fSuspensionCompression:=0.83;
 fSuspensionDamping:=0.88;
 fMaxSuspensionTravelCM:=500.0;
 fFrictionSlip:=10.5;
 fMaxSuspensionForce:=6000.0;

 fPitchControl:=0.0;
 fSteeringValue:=0.0;
 fCurrentVehicleSpeedKMHour:=0.0;

 fForwardVectors:=nil;
 fAxleVectors:=nil;

end;

destructor TKraftVehicle.Destroy;
begin
 Clear;
 inherited Destroy;
end;

procedure TKraftVehicle.Clear;
var Index:TKraftInt32;
begin

 for Index:=0 to fCountWheels-1 do begin
  FreeAndNil(fWheels[Index]);
 end;

 fWheels:=nil;
 fCountWheels:=0;

 fForwardVectors:=nil;
 fAxleVectors:=nil;

end;

function TKraftVehicle.AddWheel(const aWheel:TWheel):TKraftInt32;
begin
 result:=fCountWheels;
 inc(fCountWheels);
 if length(fWheels)<fCountWheels then begin
  SetLength(fWheels,fCountWheels+(fCountWheels shr 1)); 
  SetLength(fForwardVectors,length(fWheels));
  SetLength(fAxleVectors,length(fWheels));
 end;
 if assigned(aWheel) then begin
  fWheels[result]:=aWheel;
 end else begin
  fWheels[result]:=TWheel.Create(self);
 end;
end; 

function TKraftVehicle.GetWheel(const aIndex:TKraftInt32):TWheel;
begin
 if (aIndex>=0) and (aIndex<fCountWheels) then begin
  result:=fWheels[aIndex];
 end else begin
  result:=nil;
 end;
end;

procedure TKraftVehicle.UpdateWorldTransformVectors;
begin
 fWorldTransform:=fRigidBody.WorldTransform;
 fWorldRight:=Vector3(PKraftRawVector3(pointer(@fWorldTransform[0,0]))^);
 fWorldLeft:=Vector3Neg(fWorldRight);
 fWorldUp:=Vector3(PKraftRawVector3(pointer(@fWorldTransform[1,0]))^);
 fWorldDown:=Vector3Neg(fWorldUp);
 fWorldForward:=Vector3(PKraftRawVector3(pointer(@fWorldTransform[2,0]))^);
 fWorldBackward:=Vector3Neg(fWorldForward);
 fWorldPosition:=Vector3(PKraftRawVector3(pointer(@fWorldTransform[3,0]))^);
end;

function TKraftVehicle.GetAxisDirection(const aAxisDirection:TAxisDirection):TKraftVector3;
begin
 case aAxisDirection of
  TAxisDirection.RightVector:begin
   result:=fWorldRight;
  end;
  TAxisDirection.UpVector:begin
   result:=fWorldUp;
  end;
  TAxisDirection.ForwardVector:begin
   result:=fWorldForward;
  end;
  else begin
   result:=Vector3Origin;
  end;
 end;  
end;

procedure TKraftVehicle.UpdateSuspension(const aTimeStep:TKraftScalar);
var WheelIndex:TKraftInt32;
    Wheel:TKraftVehicle.TWheel;
    Force,SuspensionLength,CurrentLength,LengthDifference,
    ProjectedRelativeVelocity,SuspensionDamping:TKraftScalar;
begin
 
 for WheelIndex:=0 to fCountWheels-1 do begin
  
  Wheel:=fWheels[WheelIndex];

  if Wheel.fIsInContact then begin
    
   Force:=0.0;

   // Spring
   begin

    SuspensionLength:=Wheel.fSuspensionRestLength;
    CurrentLength:=Wheel.fSuspensionLength;

    LengthDifference:=SuspensionLength-CurrentLength;

    Force:=Force+(Wheel.fSuspensionStiffness*LengthDifference*Wheel.fClippedInvContactDotSuspension);

   end;

   // Damper
   begin

    ProjectedRelativeVelocity:=Wheel.fSuspensionRelativeVelocity;
    if ProjectedRelativeVelocity<0.0 then begin
     SuspensionDamping:=Wheel.fDampingCompression;
    end else begin
     SuspensionDamping:=Wheel.fDampingRelaxation;
    end;

    Force:=Force-(SuspensionDamping*ProjectedRelativeVelocity); 
      
   end;

   Wheel.fSuspensionForce:=Max(0.0,Force*fRigidBody.Mass);

  end else begin

   Wheel.fSuspensionForce:=0.0;

  end; 
  
 end; 

end;

function ResolveSingleBilateral(const Body1:TKraftRigidBody;
                                 const Pos1:TKraftVector3;
                                 const Body2:TKraftRigidBody;
                                 const Pos2:TKraftVector3;
                                 const Distance:TKraftScalar;
                                 const Normal:TKraftVector3;
                                 const TimeStep:TKraftScalar):TKraftScalar;
const ContactDamping=0.2;
var NormalLenSqr,RelativeVelocity:TKraftScalar;
    Velocity1,Velocity2,Velocity:TKraftVector3;
begin

 NormalLenSqr:=Vector3LengthSquared(Normal);
 if NormalLenSqr>1.1 then begin
  result:=0.0;
  exit;
 end;

{Velocity1:=Vector3Add(Body1.LinearVelocity,Vector3Cross(Body1.AngularVelocity,Vector3Sub(Pos1,Body1.Sweep.c)));
 Velocity2:=Vector3Add(Body2.LinearVelocity,Vector3Cross(Body2.AngularVelocity,Vector3Sub(Pos2,Body2.Sweep.c)));}

 Velocity1:=Body1.GetWorldLinearVelocityFromPoint(Pos1);
 Velocity2:=Body2.GetWorldLinearVelocityFromPoint(Pos2); 
 Velocity:=Vector3Sub(Velocity1,Velocity2);

 RelativeVelocity:=Vector3Dot(Velocity,Normal);

 result:=-ContactDamping*RelativeVelocity*(1.0/(Body1.InverseMass+NormalLenSqr*Body2.InverseMass));
  
end;

function CalcRollingFriction(const Body0:TKraftRigidBody;
                             const Body1:TKraftRigidBody;
                             const FrictionPosWorld:TKraftVector3;
                             const FrictionDirectionWorld:TKraftVector3;
                             const MaxImpulse:TKraftScalar):TKraftScalar;
var VelocityRelativeProjection,jacDiagABInv:TKraftScalar;
    Velocity:TKraftVector3;
begin
 Velocity:=Vector3Sub(Body0.GetWorldLinearVelocityFromPoint(FrictionPosWorld),Body1.GetWorldLinearVelocityFromPoint(FrictionPosWorld));
 VelocityRelativeProjection:=Vector3Dot(Velocity,FrictionDirectionWorld);
 jacDiagABInv:=1.0/(Body0.ComputeImpulseDenominator(FrictionPosWorld,FrictionDirectionWorld)+Body1.ComputeImpulseDenominator(FrictionPosWorld,FrictionDirectionWorld));
 result:=Min(Max(-VelocityRelativeProjection*jacDiagABInv,-MaxImpulse),MaxImpulse);
end;

procedure TKraftVehicle.UpdateFriction(const aTimeStep:TKraftScalar);
const SideFrictionStiffness2=1.0;
var WheelIndex,CountWheelsOnGround:TKraftInt32;
    Wheel:TKraftVehicle.TWheel;
    WheelTransform:TKraftMatrix4x4;
    SurfaceNormalWorld,RelativePosition,SideImpulse:TKraftVector3;
    Projection,SideFactor,ForwardFactor,RollingFriction,
    DefaultRollingFrictionImpulse,MaxImpulse,MaxImpulseSide,x,y,ImpulseSquared,Factor:TKraftScalar;
begin

 if fCountWheels=0 then begin
  exit;
 end;

 CountWheelsOnGround:=0;

 for WheelIndex:=0 to fCountWheels-1 do begin
  Wheel:=fWheels[WheelIndex];
  if Wheel.fIsInContact then begin
   inc(CountWheelsOnGround);
  end;
  Wheel.fSideImpulse:=0.0;
  Wheel.fForwardImpulse:=0.0;
 end; 

 for WheelIndex:=0 to fCountWheels-1 do begin
 
  Wheel:=fWheels[WheelIndex];
 
  if Wheel.fIsInContact then begin
 
   WheelTransform:=Wheel.fWorldTransform;
 
   fAxleVectors[WheelIndex]:=Vector3Neg(Vector3(PKraftRawVector3(pointer(@WheelTransform[0,0]))^));

   SurfaceNormalWorld:=Wheel.fContactNormalWorld;

   Projection:=Vector3Dot(fAxleVectors[WheelIndex],SurfaceNormalWorld);
   fAxleVectors[WheelIndex]:=Vector3Norm(Vector3Sub(fAxleVectors[WheelIndex],Vector3ScalarMul(SurfaceNormalWorld,Projection)));

   fForwardVectors[WheelIndex]:=Vector3Norm(Vector3Cross(SurfaceNormalWorld,fAxleVectors[WheelIndex]));
     
   Wheel.fSideImpulse:=ResolveSingleBilateral(fRigidBody,
                                              Wheel.fContactPointWorld,
                                              Wheel.fContactRigidBody,
                                              Wheel.fContactPointWorld,
                                              0.0,
                                              Wheel.fContactNormalWorld,
                                              aTimeStep)*SideFrictionStiffness2;

  end;
 end;

 SideFactor:=1.0;
 ForwardFactor:=0.5;

 fSliding:=false;

 for WheelIndex:=0 to fCountWheels-1 do begin
 
  Wheel:=fWheels[WheelIndex];
 
  RollingFriction:=0.0;

  Wheel.fSlipInfo:=1.0;

  if Wheel.fIsInContact then begin
   DefaultRollingFrictionImpulse:=0.0;
   if IsZero(Wheel.fBrake) then begin
    MaxImpulse:=DefaultRollingFrictionImpulse;
   end else begin
    MaxImpulse:=Wheel.fBrake;
   end;
   RollingFriction:=CalcRollingFriction(fRigidBody,Wheel.fContactRigidBody,Wheel.fContactPointWorld,fForwardVectors[WheelIndex],MaxImpulse);
   RollingFriction:=RollingFriction+(Wheel.fEngineForce*aTimeStep);
   Factor:=MaxImpulse/RollingFriction;
   Wheel.fSlipInfo:=Wheel.fSlipInfo*Factor;
  end;

  Wheel.fForwardImpulse:=0.0;
  Wheel.fSkidInfo:=1.0;

  if Wheel.fIsInContact then begin
   Wheel.fSkidInfo:=1.0;

   MaxImpulse:=Wheel.fFrictionSlip*Wheel.fSuspensionForce*aTimeStep;
   MaxImpulseSide:=MaxImpulse;

   Wheel.fForwardImpulse:=RollingFriction;

   x:=Wheel.fForwardImpulse*ForwardFactor;
   y:=Wheel.fSideImpulse*SideFactor;

   ImpulseSquared:=sqr(x)+sqr(y);

   Wheel.fSliding:=false;
   if ImpulseSquared>(MaxImpulse*MaxImpulseSide) then begin
    fSliding:=true;
    Wheel.fSliding:=true;
    Factor:=MaxImpulse/sqrt(ImpulseSquared);
    Wheel.fSkidInfo:=Wheel.fSkidInfo*Factor;
   end;
  
  end;

 end;

 if fSliding then begin
  for WheelIndex:=0 to fCountWheels-1 do begin
   Wheel:=fWheels[WheelIndex];
   if not IsZero(Wheel.fSideImpulse) then begin
    Wheel.fForwardImpulse:=Wheel.fForwardImpulse*Wheel.fSkidInfo;
    Wheel.fSideImpulse:=Wheel.fSideImpulse*Wheel.fSkidInfo;
   end;
  end;
 end;

 for WheelIndex:=0 to fCountWheels-1 do begin
  
  Wheel:=fWheels[WheelIndex];

  RelativePosition:=Vector3Sub(Wheel.fContactPointWorld,fRigidBody.Sweep.c);

  if not IsZero(Wheel.fForwardImpulse) then begin
   fRigidBody.ApplyImpulseAtRelativePosition(Vector3ScalarMul(fForwardVectors[WheelIndex],Wheel.fForwardImpulse),RelativePosition);
  end;

  if not IsZero(Wheel.fSideImpulse) then begin
           
   SideImpulse:=Vector3ScalarMul(fAxleVectors[WheelIndex],Wheel.fSideImpulse);

   RelativePosition:=Vector3Sub(RelativePosition,Vector3ScalarMul(fWorldUp,Vector3Dot(RelativePosition,fWorldUp)*(1.0-Wheel.fRollInfluence)));
   
   fRigidBody.ApplyImpulseAtRelativePosition(SideImpulse,RelativePosition);

   Wheel.fContactRigidBody.ApplyImpulseAtPosition(Vector3Neg(SideImpulse),Wheel.fContactPointWorld);

  end;

 end;

end;

procedure TKraftVehicle.Update(const aTimeStep:TKraftScalar);
var WheelIndex:TKraftInt32;
    Wheel:TKraftVehicle.TWheel;
    SuspensionForce,Projection:TKraftScalar;
    Impulse,RelativePosition,Velocity,Forwards:TKraftVector3;
begin

 UpdateWorldTransformVectors;

 for WheelIndex:=0 to fCountWheels-1 do begin
  Wheel:=fWheels[WheelIndex];
  Wheel.UpdateWheelTransform;
 end;

 fCurrentVehicleSpeedKMHour:=Vector3Length(fRigidBody.LinearVelocity)*3.6;
//fCurrentVehicleSpeedKMHour:=Vector3Dot(fWorldForward,fRigidBody.GetWorldLinearVelocityFromPoint(fWorldPosition))*3.6;

 if Vector3Dot(fWorldForward,fRigidBody.LinearVelocity)<0.0 then begin
  fCurrentVehicleSpeedKMHour:=-fCurrentVehicleSpeedKMHour;
 end;

 // Simulate suspension
 for WheelIndex:=0 to fCountWheels-1 do begin
  Wheel:=fWheels[WheelIndex];
  Wheel.RayCast;
 end;

 UpdateSuspension(aTimeStep);

 for WheelIndex:=0 to fCountWheels-1 do begin
  Wheel:=fWheels[WheelIndex];
  SuspensionForce:=Wheel.fSuspensionForce;
  if SuspensionForce>Wheel.fMaxSuspensionForce then begin
   SuspensionForce:=Wheel.fMaxSuspensionForce;
  end;
  Impulse:=Vector3ScalarMul(Wheel.fContactNormalWorld,SuspensionForce*aTimeStep);
  RelativePosition:=Vector3Sub(Wheel.fContactPointWorld,fWorldPosition);
  fRigidBody.ApplyImpulseAtRelativePosition(Impulse,RelativePosition);
 end;

 UpdateFriction(aTimeStep);

 for WheelIndex:=0 to fCountWheels-1 do begin

  Wheel:=fWheels[WheelIndex];

  Velocity:=fRigidBody.GetWorldLinearVelocityFromPoint(Wheel.fChassisConnectionPointWorld);

  if Wheel.fIsInContact then begin
   Projection:=Vector3Dot(Wheel.fContactNormalWorld,fWorldForward);
   Forwards:=Vector3Sub(fWorldForward,Vector3ScalarMul(Wheel.fContactNormalWorld,Projection));
   Projection:=Vector3Dot(Forwards,Velocity);
   Wheel.fDeltaRotation:=(Projection*aTimeStep)/Wheel.fRadius;
  end;

  if abs(Wheel.fBrake)>abs(Wheel.fEngineForce) then begin
   Wheel.fDeltaRotation:=0.0; // Lock wheels at brake 
  end;

  Wheel.fRotation:=Wheel.fRotation+Wheel.fDeltaRotation;
  Wheel.fDeltaRotation:=Wheel.fDeltaRotation*0.99;

 end;
  
end;

end.

